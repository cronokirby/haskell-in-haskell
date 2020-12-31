{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module CWriter (writeC) where

import Cmm hiding (cmm)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Ourlude
import Text.Printf (printf)

-- | The type we use to store global function information
type Globals = IntMap IdentPath

-- | A table telling us how to use each location
--
-- Each entry is usually the name of the variable, or piece
-- of C we can write to use that location.
--
-- This map is usually going to be sparse, but filled with the locations
-- we logically end up using.
newtype LocationTable = LocationTable (Map.Map Location CCode)
  deriving (Show, Semigroup, Monoid)

-- | Given a location table, find the CCode to use a given location
cLocation :: LocationTable -> Location -> Maybe CCode
cLocation (LocationTable mp) = \case
  CurrentNode -> Just "g_NodeRegister"
  IntRegister -> Just "g_IntRegister"
  StringRegister -> Just "g_StringRegister"
  -- Integers don't need to be allocated, so we can always use them
  -- Strings, on the other hand, do need to be explicitly located,
  -- so they need to have an entry here
  PrimIntLocation i -> Just (show i)
  other -> Map.lookup other mp

singleLocation :: Location -> CCode -> LocationTable
singleLocation loc code = LocationTable (Map.singleton loc code)

-- | A type for CCode.
--
-- We could use something more efficient than a string, but this is ok
-- for our explanation purposes
type CCode = String

cAllocationSize :: CCode
cAllocationSize = "allocation_size"

-- | Represents a sequence of function names
--
-- This is a useful intermediate step to help us convert our tree of functions
-- into flat C functions.
data IdentPath = IdentPath [FunctionName] deriving (Eq, Show)

instance Semigroup IdentPath where
  IdentPath names <> IdentPath names' = IdentPath (names <> names')

instance Monoid IdentPath where
  mempty = IdentPath []

-- | Add a function name after the other identifiers
consPath :: FunctionName -> IdentPath -> IdentPath
consPath name = (IdentPath [name] <>)

-- | Display a path as a piece of C code
displayPath :: IdentPath -> CCode
displayPath (IdentPath names) =
  names |> reverse |> map convertName |> ("hs" :) |> intercalate "_"
  where
    convertName :: FunctionName -> CCode
    convertName = \case
      PlainFunction name -> foldMap convertChar name
      CaseFunction index -> "case_" ++ show index
      Entry -> "_entry"
      where
        convertChar :: Char -> String
        convertChar = \case
          '$' -> "_S_"
          '\'' -> "_t_"
          '_' -> "__"
          x -> pure x

-- | Get the table name for some identifier path
tableName :: IdentPath -> CCode
tableName = displayPath >>> ("table_for_" <>)

-- | The number of columns we're currently indented
type Indent = Int

-- | The number of columns each level of indentation should have
indentAmount :: Indent
indentAmount = 2

-- | The context we have access to while generating C code
data Context = Context
  { -- | The path to the current function that we're working on
    currentFunction :: IdentPath,
    -- | The current indentation
    currentIndent :: Indent,
    -- | A map from function indices to full identifiers
    --
    -- Unlike the standard location table, this contains an abstract
    -- identifier we can convert into the name of the function,
    -- or table, or its static pointer.
    --
    -- The location table always contains the static pointer of a global
    globals :: Globals,
    -- | A table for CCode to use different locations
    locationTable :: LocationTable
  }
  deriving (Show)

-- | A context we can use at the start of our traversal
startingContext :: Context
startingContext = Context mempty 0 mempty mempty

-- | A computational context we use when generating C code
newtype CWriter a = CWriter (ReaderT Context (Writer CCode) a)
  deriving (Functor, Applicative, Monad, MonadReader Context, MonadWriter CCode)

-- | Run a CWriter computation, using the starting context
runCWriter :: CWriter a -> (a, CCode)
runCWriter (CWriter m) =
  runReaderT m startingContext |> runWriter

-- | Write a line of C code in the context
writeLine :: CCode -> CWriter ()
writeLine code = do
  amount <- asks currentIndent
  tell (replicate amount ' ')
  tell code
  tell "\n"

-- | Write a comment line
--
-- This is useful to have separate, so that we can potentially disable
-- comment output
comment :: CCode -> CWriter ()
comment code = writeLine ("// " <> code)

-- | Modify some computation to be further indented
indented :: CWriter a -> CWriter a
indented =
  local (\r -> r {currentIndent = indentAmount + currentIndent r})

-- | Execute some computation inside of a named function
insideFunction :: FunctionName -> CWriter a -> CWriter a
insideFunction name =
  local (\r -> r {currentFunction = consPath name (currentFunction r)})

-- | Execute some computation, with access to certain globals
--
-- We'll also have access to the location of their tables
withGlobals :: Globals -> CWriter a -> CWriter a
withGlobals globals =
  local (\r -> r {globals = globals})
    >>> withLocations impliedLocations
  where
    table (i, path) = singleLocation (Global i) (tableName path)

    impliedLocations =
      globals |> IntMap.toList |> foldMap table

-- | Execute some computation, with access to certain locations
withLocations :: LocationTable -> CWriter a -> CWriter a
withLocations newLocations =
  local (\r -> r {locationTable = newLocations <> locationTable r})

-- | Get the CCode to use some location
getCLocation :: Location -> CWriter CCode
getCLocation location = do
  table <- asks locationTable
  return (fromMaybe err (cLocation table location))
  where
    err = error ("could not find C location for " ++ show location)

-- | Compute the the C representation for the current function
--
-- We need access to the context in CWriter to do this, since
-- we need to know which function we're currently in
displayCurrentFunction :: CWriter CCode
displayCurrentFunction = asks currentFunction |> fmap displayPath

-- | Traverse our IR representation, gathering all global functions
--
-- We do this, since each global function has a unique index. This allows us to gather
-- all the global functions used throughout the program in advance.
gatherGlobals :: Cmm -> CWriter (IntMap IdentPath)
gatherGlobals (Cmm functions entry) = gatherInFunctions (entry : functions)
  where
    gatherInFunctions :: [Function] -> CWriter (IntMap IdentPath)
    gatherInFunctions = foldMapM gatherInFunction

    gatherInFunction :: Function -> CWriter (IntMap IdentPath)
    gatherInFunction Function {..} =
      insideFunction functionName <| do
        current <- asks currentFunction
        let thisMapping = maybe mempty (`IntMap.singleton` current) isGlobal
        thoseMappings <- gatherInFunctions subFunctions
        return (thisMapping <> thoseMappings)

genBody :: Body -> CWriter ()
genBody body = do
  reserveBodySpace body
  comment "TODO: handle normal bodies"
  writeLine "return NULL;"

reserveBodySpace :: Body -> CWriter ()
reserveBodySpace (Body alloc _) | alloc == mempty = return ()
reserveBodySpace (Body Allocation {..} _) = do
  comment "reserve enough space on the heap"
  writeLine (printf "size_t %s = 0;" cAllocationSize)
  addSize "table allocations" "sizeof(void*)" tablesAllocated
  addSize "pointer allocations" "sizeof(void*)" pointersAllocated
  addSize "int allocations" "sizeof(int64_t)" intsAllocated
  addSize "string allocations" "sizeof(uint8_t*)" stringsAllocated
  unless (null primitiveStringsAllocated)
    <| comment "primitive string allocations"
  forM_ primitiveStringsAllocated <| \s -> do
    writeLine (printf "%s += sizeof(void*) + strlen(%s) + 1;" cAllocationSize s)
  writeLine (printf "heap_reserve(%s);\n" cAllocationSize)
  where
    addSize _ _ 0 = return ()
    addSize cmt sizeof count = do
      comment cmt
      writeLine (printf "%s += %d * %s;" cAllocationSize count sizeof)

genIntCases :: CCode -> [(Int, Body)] -> Body -> CWriter ()
genIntCases scrut cases default' = do
  writeLine (printf "switch (%s) {" scrut)
  indented <| do
    forM_ cases genCase
    genDefault default'
  writeLine "}"
  where
    genCase (i, body) = do
      writeLine (printf "case %d: {" i)
      indented (genBody body)
      writeLine "}"

    genDefault body = do
      writeLine "default: {"
      indented (genBody body)
      writeLine "}"

genStringCases :: [(String, Body)] -> Body -> CWriter ()
genStringCases _ _ = comment "TODO: Handle string cases"

genFunctionBody :: FunctionBody -> CWriter ()
genFunctionBody = \case
  IntCaseBody cases default' -> genIntCases "g_IntRegister" cases default'
  TagCaseBody cases default' -> genIntCases "g_TagRegister" cases default'
  StringCaseBody cases default' -> genStringCases cases default'
  NormalBody body -> genBody body

-- | Generate the C code for a function
genFunction :: Function -> CWriter ()
genFunction Function {..} =
  insideFunction functionName <| do
    comment (printf "%s" (show functionName))
    current <- displayCurrentFunction
    writeLine (printf "void* %s() {" current)
    indented (genFunctionBody body)
    writeLine "}"
    forM_ subFunctions genFunction

-- | Generate CCode for our Cmm IR
genCmm :: Cmm -> CWriter ()
genCmm (Cmm functions entry) = do
  writeLine "#include \"runtime.c\"\n"
  forM_ functions <| \f -> do
    genFunction f
    writeLine ""
  genFunction entry

-- | Convert our Cmm IR into actual C code
writeC :: Cmm -> CCode
writeC cmm =
  let (globals, _) = runCWriter (gatherGlobals cmm)
   in withGlobals globals (genCmm cmm) |> runCWriter |> snd
