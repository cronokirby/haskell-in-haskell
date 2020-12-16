-- | This module contains the intermediate code generator between STG and C
--
-- The reason this exists is primarily to simplify code generation. You could
-- generate C directly from STG, but this leads to more complicated code.
-- The primary mixing of concerns is that of translating the STG semantics into
-- static information, and ordering that information into actual C. For example,
-- all we need to know from a `let` binding is what kind of things get allocated,
-- and how many. If you generate C directly, you mix the calculation of this
-- information with its usage to generate C code. By separating these two parts,
-- you make both of them much simpler.
--
-- Having a separate stage makes it much easier to generate better C code, since
-- you can easily translate the STG into simple imperative statements, and then
-- analyze those to generate nicer C code.
module Cmm where

import Ourlude
import STG (Tag)

-- | Represents a name we can give to a function
--
-- The individual pieces of a function may not be unique,
-- but if we look at the nested tree of functions and their
-- subfunctions, then we get unique paths.
data FunctionName
  = -- | A standard function name
    StringFunction String
  | -- | A name we can use for the alternatives inside of a function
    Alts
  deriving (Show)

type Index = Int

-- | Represents what type of storage some variable will need
--
-- We can figure this out the first time a variable is used, and then
-- use that information in nested closures to figure out how they're going
-- to access this variable that they've captured
data Storage
  = -- | This variable will be stored in a pointer
    PointerStorage
  | -- | This variable will be stored as an int, with 64 bits
    IntStorage
  | -- | This variable will be stored as a string
    StringStorage
  | -- | This variable is a global function with a certain index
    --
    -- When a variable references a global function, we don't need
    -- to store it alongside the closure, since it can just reference it
    -- directly
    GlobalStorage Index
  deriving (Show)

-- | A location allows us to reference some value concretely
data Location
  = -- | This variable is the nth pointer arg passed to us on the stack
    Arg Index
  | -- | This variable is the nth pointer bound in this closure
    Bound Index
  | -- | This variable is the nth int bound in this closure
    BoundInt Index
  | -- | This variable is the nth string bound in this closure
    BoundString Index
  | -- | This variable is just a global function
    Global Index
  | -- | This variable is a closure we've allocated, using an index to figure out which
    Allocated Index
  | -- | This variable is the nth dead pointer
    --
    -- Buried locations come from the bound names used inside the branches of a
    -- case expression. Since we split cases into two, we need a way to save
    -- and restore this before getting back to the case.
    Buried Index
  | -- | The nth dead int. See `Buried` for more information.
    BuriedInt Index
  | -- | The nth dead string. See `Buried` for more information.
    BuriedString Index
  deriving (Show)

-- | Represents a kind of builtin taking two arguments
data Builtin2
  = -- | IntR <- a + b
    Add2
  | -- | IntR <- a - b
    Sub2
  | -- | IntR <- a * b
    Mul2
  | -- | IntR <- a / b
    Div2
  | -- | IntR <- a < b
    Less2
  | -- | IntR <- a <= b
    LessEqual2
  | -- | IntR <- a > b
    Greater2
  | -- | IntR <- a >= b
    GreaterEqual2
  | -- | IntR <- a == b
    EqualTo2
  | -- | IntR <- a /= b
    NotEqualTo2
  | -- | StringR <- a ++ b
    Concat2
  deriving (Show)

-- | Represents a builtin taking only a single argument
data Builtin1
  = -- | Print out an int
    PrintInt1
  | -- | Print out a string
    PrintString1
  | -- | IntR <- -a
    Negate1
  deriving (Show)

-- | Represents a single instruction in our IR
--
-- The idea is that each of these instructions is a little unit that makes
-- sense on the weird VM you need for lazy execution, and also translates
-- directly to a simple bit of C.
data Instruction
  = -- | Store a given integer into the integer register
    StoreInt Int
  | -- | Store a given string litteral into the string register
    StoreString String
  | -- | Store a given tag into the tag register
    StoreTag Tag
  | -- | Enter the code stored at a given location
    --
    -- For this to be valid, that location needs to actually contain *code*,
    -- of course. `BoundString` would not be a valid location here, for example.
    Enter Location
  | -- | We need to enter the code for the continuation at the top of the stack
    --
    -- In practice, this stack will contain the code for the branches
    -- of a case expression, and this instruction yields control to
    -- whatever branches need to match on the value we're producing.
    EnterCaseContinuation
  | -- | Print that an error happened
    PrintError String
  | -- | Apply a builtin expecting two locations
    Builtin2 Builtin2 Location Location
  | -- | Apply a builtin expecting a single location
    Builtin1 Builtin1 Location Location
  | -- | Exit the program
    Exit
  | -- | Push a pointer onto the argument stack
    SAPush Location
  | -- | Bury a pointer used in a case expression
    Bury Location
  | -- | Bury an int used in a case expression
    BuryInt Location
  | -- | Bury a string used in a case expression
    BuryString Location
  | -- | Allocate a table for a function
    --
    -- This function is going to be a direct descendant of the function
    -- in which this instruction appears.
    --
    -- The index is used, since we may reference this closure
    AllocTable FunctionName Index
  | -- | Allocate an int on the heap
    AllocInt Location
  | -- | Allocate a string on the heap
    AllocString Location
  deriving (Show)

-- | An allocation records information about how much a given expression will allocate
--
-- This is useful, because for GC purposes, we want to reserve the amount of memory
-- we need at the very start of the function, which makes it easier to not
-- have any stale pointers lying around.
data Allocation = Allocation
  { -- | The number of tables for closures allocated
    tablesAllocated :: Int,
    -- | The number of pointers inside closures allocated
    pointersAllocated :: Int,
    -- | The number of ints inside closures allocated
    intsAllocated :: Int,
    -- | The number of points to strings inside closures allocated
    stringsAllocated :: Int,
    -- | The raw strings that this function allocates
    --
    -- We need to know exactly which strings, becuase how much memory is allocated
    -- depends on the length of the string.
    primitiveStringsAllocated :: [String]
  }
  deriving (Show)

instance Semigroup Allocation where
  Allocation t p i s ps <> Allocation t' p' i' s' ps' =
    Allocation (t + t') (p + p') (i + i') (s + s') (ps <> ps')

instance Monoid Allocation where
  mempty = Allocation 0 0 0 0 []

-- | Represents a function.
--
-- Functions are the units of execution, but have a bunch of "metadata"
-- associated with them, and can also potentially have subfunctions.
data Function = Function
  { -- | The name of the function
    functionName :: FunctionName,
    -- | If an index is present, then this function corresponds to a certain global index
    --
    -- We do things this way, that way we can traverse the function tree to build up
    -- a table of index functions to fully resolved function names. Trying
    -- to generate the fully resolved function name at this stage would be annoying.
    isGlobal :: Maybe Index
  }
  deriving (Show)
