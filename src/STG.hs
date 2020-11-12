module STG (Atom (..), Litteral (..), Name) where

import Ourlude
import Simplifier (Builtin (..), Litteral (..), Name)

-- Represents a unit of data simple enough to be passed directly
--
-- With STG, we want to limit the complexity of expressions that appear
-- inside other applications, to make compilation much easier.
data Atom
  = -- A litteral value
    LitteralAtom Litteral
  | -- A reference to a name
    NameAtom Name

-- Represents a tag for an alternative of an algebraic data type
type Tag = Int

-- Represents an expression in our STG language
data Expr
  = -- A litteral value
    Litteral Litteral
  | -- Apply a name (function) to a potentially empty sequence of atoms
    Apply Name [Atom]
  | -- Panic with a given error
    --
    -- This is something we introduce here, to be able to provide an expression
    -- for the default branch of a case
    Error String
  | -- Apply an ADT constructor to a sequence of atoms
    --
    -- The constructor must be fully saturated, i.e. its arity
    -- should match the number of atoms here.
    Constructor Tag [Atom]
  | -- Apply a fully saturated builtin to a given sequence of atoms
    Builtin Builtin [Atom]
  | -- Inspect an expression, and handle the different cases.
    --
    -- We don't take an atom here, because building up a thunk we
    -- immediately evaluate would be a bit pointless
    Case Expr Alts
  | Let [Binding] Expr

-- Represents different branches of shallow alternatives.
--
-- As opposed to patterns, alternatives only look at a single level
-- of data.
--
-- Alternatives always have a default expression.
--
-- We split alternatives for different litterals, mainly to simplify
-- code generation. If we used a single alternative for all litterals,
-- we'd lose information about what types are involved, making code
-- generation more cumbersome
data Alts
  = -- Potential branches for integer litterals, then a default expression
    IntAlts [(Int, Expr)] DefaultAlt
  | -- Potential branches for booleans, then a default expression
    --
    -- Because booleans only have two values, we could simplify this representation,
    -- but that would make generating STG harder, and whatever compiler
    -- for the low level IR we generate should be able to see the redundancy.
    BoolAlts [(Bool, Expr)] DefaultAlt
  | -- Potential branches for string litterals, then a default case
    StringAlts [(String, Expr)] DefaultAlt
  | -- Potential branches for constructor tags, introducing names,
    -- and then we end, as usual, with a default case
    ConstrAlts [(Tag, [Name], Expr)] DefaultAlt

-- A default alternative, which may introduce a variable, or not
data DefaultAlt
  = -- A wildcard alternative, introducing no variables
    WildCard Expr
  | -- A variable alternative, introducing a single variable
    VarAlt Name Expr

-- A flag telling us when a thunk is updateable
--
-- By default, all thunks are updateable, and marking thunks
-- as non-updateable is an optimization for certain thinks
-- which we know to already be fully evaluated, or which aren't
-- used more than once.
data Updateable = NonUpdateable | Updateable

-- Represents a lambda expression
--
-- We first have a list of free variables occurring in the body,
-- the updateable flag, then the list of parameters, and then the body
data LambdaForm = LambdaForm [Name] Updateable [Name] Expr

-- Represents a binding from a name to a lambda form
--
-- In STG, bindings always go through lambda forms, representing
-- the connection with heap allocated thunks.
data Binding = Binding Name LambdaForm

-- Represents an STG program, which is just a list of top level bindings.
data STG = STG [Binding]
