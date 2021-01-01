{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module CWriter (writeC) where

import Cmm hiding (cmm)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable (Foldable (fold))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Ourlude
import Text.Printf (printf)

-- | The type we use to store global function information
type Globals = IntMap IdentPath

{- Locations -}

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

manyLocations :: [(Location, CCode)] -> LocationTable
manyLocations = Map.fromList >>> LocationTable

-- | A type for CCode.
--
-- We could use something more efficient than a string, but this is ok
-- for our explanation purposes
type CCode = String

{- Common variable names -}

-- | A variable name for allocation
allocationSizeVar :: CCode
allocationSizeVar = "allocation_size"

-- | A variable name for the Nth argument passed to us
argVar :: Index -> CCode
argVar n = "arg_" <> show n

-- | A variable name for the Nth constructor argument passed to us
constructorArgVar :: Index -> CCode
constructorArgVar n = "constructor_arg_" <> show n

-- | A variable name for the Nth bound pointer in this closure
boundPointerVar :: Index -> CCode
boundPointerVar n = "bound_pointer_" <> show n

-- | A variable name for the Nth bound int in this closure
boundIntVar :: Index -> CCode
boundIntVar n = "bound_int_" <> show n

-- | A variable name for the Nth bound string in this closure
boundStringVar :: Index -> CCode
boundStringVar n = "bound_string_" <> show n

-- | A temp variable for reading out of a closure
closurePointerTmp :: CCode
closurePointerTmp = "tmp_closure_ptr"

-- | A variable name for the Nth sub function allocated
allocatedVar :: Index -> CCode
allocatedVar n = "allocated_" <> show n

-- | A variable name for the Nth buried pointer
buriedPtrVar :: Index -> CCode
buriedPtrVar n = "buried_ptr_" <> show n

-- | A variable name for the Nth buried int
buriedIntVar :: Index -> CCode
buriedIntVar n = "buried_int_" <> show n

-- | A variable name for the Nth buried string
buriedStringVar :: Index -> CCode
buriedStringVar n = "buried_string_" <> show n

{- Nested Identifiers -}

-- | Represents a sequence of function names
--
-- This is a useful intermediate step to help us convert our tree of functions
-- into flat C functions.
newtype IdentPath = IdentPath [FunctionName] deriving (Eq, Show)

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
    locationTable :: LocationTable,
    -- | A table mapping indexed sub functions to full identifier paths
    subFunctionTable :: IntMap IdentPath
  }
  deriving (Show)

-- | A context we can use at the start of our traversal
startingContext :: Context
startingContext = Context mempty 0 mempty mempty mempty

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

-- | Execute some computation, with access to certain sub function locations
withSubFunctionTable :: [Function] -> CWriter a -> CWriter a
withSubFunctionTable functions m = do
  current <- asks currentFunction
  let makePath Function {..} = consPath functionName current
      table = functions |> map makePath |> zip [0 ..] |> IntMap.fromList
  local (\r -> r {subFunctionTable = table}) m

-- | Get the C function associated with the nth sub function in the current scope
getSubFunction :: Index -> CWriter CCode
getSubFunction n = do
  table <- asks subFunctionTable
  let path = IntMap.findWithDefault err n table
  return (displayPath path)
  where
    err = error ("Sub Function " <> show n <> " has no C function associated with it")

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

-- | Generate the C code to handle the instructions in a body
--
-- This assumes that all the necessary locations have been supplied
genInstructions :: Body -> CWriter ()
genInstructions (Body _ _ []) = writeLine "return NULL;"
genInstructions (Body _ _ [PopExcessConstructorArgs]) = writeLine "return NULL;"
genInstructions (Body _ _ instrs) =
  forM_ instrs <| \instr -> do
    comment (show instr)
    genInstr instr
  where
    genInstr = \case
      StoreInt location ->
        getCLocation location >>= \l ->
          writeLine (printf "g_IntRegister = %s;" l)
      StoreString location ->
        getCLocation location >>= \l ->
          writeLine (printf "g_StringRegister = %s;" l)
      StoreTag tag -> writeLine (printf "g_TagRegister = %d;" tag)
      StoreConstructorArgCount count ->
        writeLine (printf "g_ConstructorArgCountRegister = %d;" count)
      PopExcessConstructorArgs ->
        writeLine "g_SA.top -= g_ConstructorArgCountRegister;"
      Enter _ -> do
        comment "TODO: Handle this correctly"
        writeLine "return NULL;"
      EnterCaseContinuation -> do
        comment "TODO: Handle this correctly"
        writeLine "return NULL;"
      PushSA location ->
        getCLocation location >>= \l -> do
          writeLine (printf "g_SA.top[0] = %s;" l)
          writeLine "++g_SA.top;"
      PushConstructorArg location ->
        getCLocation location >>= \l -> do
          writeLine (printf "g_SA.top[0] = %s;" l)
          writeLine "++g_SA.top;"
      PushCaseContinuation index -> do
        function <- getSubFunction index
        writeLine (printf "g_SB.top[0].as_continuation = &%s;" function)
        writeLine "++g_SB.top;"
      Exit -> writeLine "return NULL;"
      other -> comment "TODO: Handle this correctly"

genNormalBody :: Int -> ArgInfo -> Body -> CWriter ()
genNormalBody argCount bound body = do
  reserveBodySpace body
  args <- if argCount <= 0 then return mempty else popArgs
  boundArgs <- popBound bound
  withLocations (args <> boundArgs) (genInstructions body)
  where
    popArgs :: CWriter LocationTable
    popArgs = do
      comment "popping stack arguments"
      writeLine (printf "g_SA.top -= %d;" argCount)
      pairs <-
        forM [0 .. argCount - 1] <| \n -> do
          let var = argVar n
          writeLine (printf "uint8_t* %s = g_SA.top[%d];" var n)
          return (Arg n, var)
      return (manyLocations pairs)

    popBound :: ArgInfo -> CWriter LocationTable
    popBound (ArgInfo 0 0 0) = return mempty
    popBound ArgInfo {..} = do
      comment "pulling bound arguments"
      writeLine (printf "uint8_t* %s = g_NodeRegister + sizeof(InfoTable*);" closurePointerTmp)
      fold <$> sequence [popPointers, popInts, popStrings]
      where
        popPointers = case boundPointers of
          0 -> return mempty
          count -> do
            comment "pulling bound pointers"
            pairs <-
              forM [0 .. count - 1] <| \n -> do
                let var = boundPointerVar n
                writeLine (printf "uint8_t* %s = read_ptr(%s);" var closurePointerTmp)
                writeLine (printf "%s += sizeof(uint8_t*);" closurePointerTmp)
                return (Bound n, var)
            return (manyLocations pairs)
        popInts = case boundInts of
          0 -> return mempty
          count -> do
            comment "pulling bound ints"
            pairs <-
              forM [0 .. count - 1] <| \n -> do
                let var = boundIntVar n
                writeLine (printf "int64_t %s = read_int(%s);" var closurePointerTmp)
                writeLine (printf "%s += sizeof(int64_t);" closurePointerTmp)
                return (BoundInt n, var)
            return (manyLocations pairs)
        popStrings = case boundStrings of
          0 -> return mempty
          count -> do
            comment "pulling bound strings"
            pairs <-
              forM [0 .. count - 1] <| \n -> do
                let var = boundStringVar n
                writeLine (printf "uint8_t* %s = read_ptr(%s);" var closurePointerTmp)
                writeLine (printf "%s += sizeof(uint8_t*);" closurePointerTmp)
                return (BoundString n, var)
            return (manyLocations pairs)

reserveBodySpace :: Body -> CWriter ()
reserveBodySpace (Body alloc _ _) | alloc == mempty = return ()
reserveBodySpace (Body Allocation {..} _ _) = do
  comment "reserve enough space on the heap"
  writeLine (printf "size_t %s = 0;" allocationSizeVar)
  addSize "table allocations" "sizeof(void*)" tablesAllocated
  addSize "pointer allocations" "sizeof(void*)" pointersAllocated
  addSize "int allocations" "sizeof(int64_t)" intsAllocated
  addSize "string allocations" "sizeof(uint8_t*)" stringsAllocated
  writeLine (printf "heap_reserve(%s);\n" allocationSizeVar)
  where
    addSize _ _ 0 = return ()
    addSize cmt sizeof count = do
      comment cmt
      writeLine (printf "%s += %d * %s;" allocationSizeVar count sizeof)

genContinuationBody :: ArgInfo -> Body -> CWriter ()
genContinuationBody buriedArgs body = do
  reserveBodySpace body
  args <- popConstructorArgs body
  buried <- popBuriedArgs buriedArgs
  withLocations (args <> buried) (genInstructions body)
  where
    popConstructorArgs :: Body -> CWriter LocationTable
    popConstructorArgs (Body _ 0 _) = return mempty
    popConstructorArgs (Body _ count _) = do
      comment "popping constructor arguments"
      writeLine (printf "g_SA.top -= %d;" count)
      pairs <-
        forM [0 .. count - 1] <| \n -> do
          let var = constructorArgVar n
          writeLine (printf "uint8_t* %s = g_SA.top[%d];" var n)
          return (ConstructorArg n, var)
      return (manyLocations pairs)
    popBuriedArgs :: ArgInfo -> CWriter LocationTable
    popBuriedArgs (ArgInfo 0 0 0) = return mempty
    popBuriedArgs ArgInfo {..} = do
      comment "resurrecting buried locals"
      sequence
        [ popPointers boundPointers,
          popInts boundInts,
          popStrings boundStrings
        ]
        |> fmap fold
      where
        popPointers 0 = return mempty
        popPointers count = do
          comment "buried pointers"
          writeLine (printf "g_SA.top -= %d;" count)
          pairs <-
            forM [0 .. count - 1] <| \n -> do
              let var = buriedPtrVar n
              writeLine (printf "uint8_t* %s = g_SA.top[%d];" var n)
              return (Buried n, var)
          return (manyLocations pairs)
        popInts 0 = return mempty
        popInts count = do
          comment "buried ints"
          writeLine (printf "g_SB.top -= %d;" count)
          pairs <-
            forM [0 .. count - 1] <| \n -> do
              let var = buriedIntVar n
              writeLine (printf "int64_t %s = g_SB.top[%d].as_int;" var n)
              return (BuriedInt n, var)
          return (manyLocations pairs)
        popStrings 0 = return mempty
        popStrings count = do
          comment "buried strings"
          writeLine (printf "g_SA.top -= %d;" count)
          pairs <-
            forM [0 .. count - 1] <| \n -> do
              let var = buriedStringVar n
              writeLine (printf "uint8_t* %s = g_SA.top[%d];" var n)
              return (BuriedString n, var)
          return (manyLocations pairs)

genIntCases :: ArgInfo -> CCode -> [(Int, Body)] -> Body -> CWriter ()
genIntCases buriedArgs scrut cases default' = do
  writeLine (printf "switch (%s) {" scrut)
  indented <| do
    forM_ cases genCase
    genDefault default'
  writeLine "}"
  where
    genCase (i, body) = do
      writeLine (printf "case %d: {" i)
      indented (genContinuationBody buriedArgs body)
      writeLine "}"

    genDefault body = do
      writeLine "default: {"
      indented (genContinuationBody buriedArgs body)
      writeLine "}"

genStringCases :: ArgInfo -> [(String, Body)] -> Body -> CWriter ()
genStringCases _ _ _ = comment "TODO: Handle string cases"

genFunctionBody :: Int -> ArgInfo -> FunctionBody -> CWriter ()
genFunctionBody argCount boundArgs = \case
  IntCaseBody cases default' ->
    genIntCases boundArgs "g_IntRegister" cases default'
  TagCaseBody cases default' ->
    genIntCases boundArgs "g_TagRegister" cases default'
  StringCaseBody cases default' ->
    genStringCases boundArgs cases default'
  ContinuationBody body ->
    genContinuationBody boundArgs body
  NormalBody body -> genNormalBody argCount boundArgs body

-- | Generate the C code for a function
genFunction :: Function -> CWriter ()
genFunction Function {..} =
  insideFunction functionName <| do
    comment (printf "%s" (show functionName))
    current <- displayCurrentFunction
    writeLine (printf "void* %s(void);" current)
    forM_ subFunctions genFunction
    writeLine (printf "void* %s() {" current)
    withSubFunctionTable subFunctions
      <| withLocations maybeAllocatedClosures
      <| indented
      <| genFunctionBody argCount boundArgs body
    writeLine "}"
  where
    -- The idea is that we'll initialize these variable on demand as we
    -- see actual alloc instructions, but we know what we're going to name that
    -- variable already.
    maybeAllocatedClosures =
      manyLocations [(Allocated n, allocatedVar n) | n <- zipWith const [0 ..] subFunctions]

genMainFunction :: CWriter ()
genMainFunction = do
  writeLine "int main() {"
  indented <| do
    writeLine "setup();"
    let entry = displayPath (IdentPath [Entry])
    writeLine (printf "CodeLabel label = &%s;" entry)
    writeLine "while (label != NULL) {"
    indented (writeLine "label = (CodeLabel)label();")
    writeLine "}"
    writeLine "cleanup();"
    writeLine "return 0;"
  writeLine "}"

-- | Generate CCode for our Cmm IR
genCmm :: Cmm -> CWriter ()
genCmm (Cmm functions entry) = do
  writeLine "#include \"runtime.c\"\n"
  forM_ functions <| \f -> do
    genFunction f
    writeLine ""
  genFunction entry
  writeLine ""
  genMainFunction

-- | Convert our Cmm IR into actual C code
writeC :: Cmm -> CCode
writeC cmm =
  let (globals, _) = runCWriter (gatherGlobals cmm)
   in withGlobals globals (genCmm cmm) |> runCWriter |> snd
