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
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
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

-- | A variable name for the nth string literal
stringLiteralVar :: Index -> CCode
stringLiteralVar n = "string_literal_" <> show n

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

-- | Get the table pointer name for some identifier path
tablePtrName :: IdentPath -> CCode
tablePtrName = displayPath >>> ("table_pointer_for_" <>)

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
    -- Basically, when we use this location, we're expecting a pointer
    -- holding the location in memory where a pointer to the table lives.
    -- The pointer to the table is allocated in static memory, so we need
    -- the address in static memory where it lives.
    table (i, path) = singleLocation (Global i) (printf "(uint8_t*)&%s" (tablePtrName path))

    impliedLocations =
      globals |> IntMap.toList |> foldMap table

-- | Get the C function for some global
getGlobalFunction :: Index -> CWriter CCode
getGlobalFunction i = do
  globals' <- asks globals
  let path = IntMap.findWithDefault err i globals'
  return (displayPath path)
  where
    err = error ("Global Function " <> show i <> " does not exist")

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

getSubFunctionPath :: Index -> CWriter IdentPath
getSubFunctionPath n =
  asks (subFunctionTable >>> IntMap.findWithDefault err n)
  where
    err = error ("Sub Function " <> show n <> " has no C function associated with it")

-- | Get the C function associated with the nth sub function in the current scope
getSubFunction :: Index -> CWriter CCode
getSubFunction = getSubFunctionPath >>> fmap displayPath

-- | Get the table name for some sub function index
getTableName :: Index -> CWriter CCode
getTableName = getSubFunctionPath >>> fmap tableName

-- | Get the CCode to use some location
getCLocation :: Location -> CWriter CCode
getCLocation location = do
  table <- asks locationTable
  return (fromMaybe err (cLocation table location))
  where
    err = error ("could not find C location for " ++ show location)

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
    genB1 b l = case b of
      PrintInt1 -> writeLine (printf "printf(\"%%ld\\n\", %s);" l)
      PrintString1 ->
        writeLine (printf "printf(\"%%s\\n\", (char*)(%s + sizeof(InfoTable*)));" l)
      Negate1 -> writeLine (printf "g_IntRegister = -%s;" l)

    genB2 b l1 l2 = case b of
      Add2 -> writeLine (printf "g_IntRegister = %s + %s;" l1 l2)
      Sub2 -> writeLine (printf "g_IntRegister = %s - %s;" l1 l2)
      Mul2 -> writeLine (printf "g_IntRegister = %s * %s;" l1 l2)
      Div2 -> writeLine (printf "g_IntRegister = %s / %s;" l1 l2)
      Less2 -> writeLine (printf "g_IntRegister = %s < %s;" l1 l2)
      LessEqual2 -> writeLine (printf "g_IntRegister = %s <= %s;" l1 l2)
      Greater2 -> writeLine (printf "g_IntRegister = %s > %s;" l1 l2)
      GreaterEqual2 -> writeLine (printf "g_IntRegister = %s >= %s;" l1 l2)
      EqualTo2 -> writeLine (printf "g_IntRegister = %s == %s;" l1 l2)
      NotEqualTo2 -> writeLine (printf "g_IntRegister = %s /= %s;" l1 l2)
      Concat2 -> writeLine (printf "g_StringRegister = string_concat(%s, %s);" l1 l2)

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
      Enter (Global i) ->
        getGlobalFunction i >>= \l ->
          writeLine (printf "return &%s;" l)
      Enter location ->
        getCLocation location >>= \l -> do
          writeLine (printf "g_NodeRegister = %s;" l)
          writeLine (printf "return read_info_table(%s)->entry;" l)
      EnterCaseContinuation -> do
        writeLine "--g_SB.top;"
        writeLine "return g_SB.top[0].as_continuation;"
      Exit -> writeLine "return NULL;"
      Builtin2 b location1 location2 -> do
        l1 <- getCLocation location1
        l2 <- getCLocation location2
        genB2 b l1 l2
      Builtin1 b location -> getCLocation location >>= genB1 b
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
      Bury location ->
        getCLocation location >>= \l -> do
          writeLine (printf "g_SA.top[0] = %s;" l)
          writeLine "++g_SA.top;"
      BuryInt location ->
        getCLocation location >>= \l -> do
          writeLine (printf "g_SB.top[0].as_int = %s;" l)
          writeLine "++g_SB.top;"
      BuryString location ->
        getCLocation location >>= \l -> do
          writeLine (printf "g_SA.top[0] = %s;" l)
          writeLine "++g_SA.top;"
      AllocTable index -> do
        let var = allocatedVar index
        table <- getTableName index
        writeLine (printf "uint8_t* %s = heap_cursor();" var)
        writeLine (printf "heap_write_info_table(&%s);" table)
      AllocPointer location ->
        getCLocation location >>= \l ->
          writeLine (printf "heap_write_ptr(%s);" l)
      AllocInt location ->
        getCLocation location >>= \l ->
          writeLine (printf "heap_write_int(%s);" l)
      AllocString location ->
        getCLocation location >>= \l ->
          writeLine (printf "heap_write_ptr(%s);" l)
      PrintError l ->
        writeLine (printf "printf(\"Error:\\n%%s\\n\", (char*)(%s + sizeof(InfoTable*)));" l)

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
  addSize "table allocations" "sizeof(InfoTable*)" tablesAllocated
  addSize "pointer allocations" "sizeof(uint8_t*)" pointersAllocated
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
    currentPath <- asks currentFunction
    let current = displayPath currentPath
        currentTable = tableName currentPath
        currentPointer = tablePtrName currentPath
    writeLine (printf "void* %s(void);" current)
    writeLine (printf "InfoTable %s = { &%s };" currentTable current)
    -- If it this is a global, we need to create a place for the info table
    -- pointer to live
    when (isJust isGlobal)
      <| writeLine (printf "InfoTable* %s = &%s;" currentPointer currentTable)
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

-- | Gather all the literal strings used in our program
gatherStrings :: Cmm -> Set.Set String
gatherStrings (Cmm functions entry) =
  foldMap inFunction (entry : functions)
  where
    inFunction :: Function -> Set.Set String
    inFunction Function {..} =
      inFunctionBody body <> foldMap inFunction subFunctions

    inFunctionBody :: FunctionBody -> Set.Set String
    inFunctionBody = \case
      IntCaseBody branches default' ->
        foldMap inBody (default' : map snd branches)
      StringCaseBody branches default' ->
        foldMap inBody (default' : map snd branches)
      TagCaseBody branches default' ->
        foldMap inBody (default' : map snd branches)
      ContinuationBody body -> inBody body
      NormalBody body -> inBody body

    inBody :: Body -> Set.Set String
    inBody (Body _ _ instrs) = foldMap inInstr instrs

    inInstr :: Instruction -> Set.Set String
    inInstr = \case
      StoreString loc -> inLocation loc
      Builtin2 _ loc1 loc2 -> inLocation loc1 <> inLocation loc2
      Builtin1 _ loc -> inLocation loc
      BuryString loc -> inLocation loc
      AllocString loc -> inLocation loc
      -- Some of these contain locations, but shouldn't contain strings
      _ -> mempty

    inLocation :: Location -> Set.Set String
    inLocation (PrimStringLocation s) = Set.singleton s
    inLocation _ = mempty

-- | Generate the static strings we need in our program
genStaticStrings :: Set.Set String -> CWriter LocationTable
genStaticStrings strings =
  foldMapM (uncurry makeLocation) indexedStrings
  where
    indexedStrings :: [(Index, String)]
    indexedStrings = strings |> Set.toList |> zip [0 ..]

    makeLocation :: Index -> String -> CWriter LocationTable
    makeLocation i s = do
      let bytes = length s + 1
          var = stringLiteralVar i
      writeLine "struct {"
      indented <| do
        writeLine "InfoTable* table;"
        writeLine (printf "char data[%d];" bytes)
      writeLine (printf "} %s = { &table_for_string_literal, %s };" var (show s))
      return (singleLocation (PrimStringLocation s) ("(uint8_t*)&" <> var))

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
genCmm cmm@(Cmm functions entry) = do
  writeLine "#include \"runtime.c\"\n"
  stringLocations <- genStaticStrings (gatherStrings cmm)
  writeLine ""
  withLocations stringLocations <| do
    forM_ functions <| \f -> do
      genFunction f
      writeLine ""
    genFunction entry
    writeLine ""
    writeLine ""
    genMainFunction

-- | Convert our Cmm IR into actual C code
writeC :: Cmm -> CCode
writeC cmm =
  let (globals, _) = runCWriter (gatherGlobals cmm)
   in withGlobals globals (genCmm cmm) |> runCWriter |> snd
