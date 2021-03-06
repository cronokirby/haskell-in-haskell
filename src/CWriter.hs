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
import qualified Data.Set as Set
import Ourlude
import Text.Printf (printf)

-- | The type we use to store global function information
type TopLevels = IntMap IdentPath

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

cafCellFor :: IdentPath -> CCode
cafCellFor path = "caf_cell_for_" <> displayPath path

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

-- | A variable name for the evac function given a bound argument shape
evacArgInfoVar :: ArgInfo -> CCode
evacArgInfoVar (ArgInfo 0 0 0) = "evac_empty"
evacArgInfoVar ArgInfo {..} =
  "evac"
    <> fold
      [ part "pointers" boundPointers,
        part "ints" boundInts,
        part "strings" boundStrings
      ]
  where
    part _ 0 = ""
    part tag n = "_" <> show n <> "_" <> tag

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
    globals :: TopLevels,
    cafs :: TopLevels,
    -- | A table for CCode to use different locations
    locationTable :: LocationTable,
    -- | A table mapping indexed sub functions to full identifier paths
    subFunctionTable :: IntMap IdentPath
  }
  deriving (Show)

-- | A context we can use at the start of our traversal
startingContext :: Context
startingContext = Context mempty 0 mempty mempty mempty mempty

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
withGlobals :: TopLevels -> CWriter a -> CWriter a
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

withCafs :: TopLevels -> CWriter a -> CWriter a
withCafs cafs =
  local (\r -> r {cafs = cafs}) >>> withLocations impliedLocations
  where
    location (i, path) =
      singleLocation (CAF i) (printf "(uint8_t*)&%s" (cafCellFor path))

    impliedLocations =
      cafs |> IntMap.toList |> foldMap location

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

getCafCell :: Index -> CWriter CCode
getCafCell n = asks (cafs >>> IntMap.findWithDefault err n >>> cafCellFor)
  where
    err = error ("CAF " <> show n <> " has no cell associated with it")

-- | Traverse our IR representation, gathering all global functions
--
-- We do this, since each global function has a unique index. This allows us to gather
-- all the global functions used throughout the program in advance.
gatherGlobals :: Cmm -> CWriter (TopLevels, TopLevels)
gatherGlobals (Cmm functions entry) = gatherInFunctions (entry : functions)
  where
    gatherInFunctions :: [Function] -> CWriter (TopLevels, TopLevels)
    gatherInFunctions = foldMapM gatherInFunction

    gatherInFunction :: Function -> CWriter (TopLevels, TopLevels)
    gatherInFunction Function {..} =
      insideFunction functionName <| do
        current <- asks currentFunction
        let thisMapping = case closureType of
              DynamicClosure -> mempty
              GlobalClosure i -> (IntMap.singleton i current, mempty)
              CAFClosure i -> (mempty, IntMap.singleton i current)
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
        writeLine "return g_SB.top[0].as_code;"
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
        writeLine (printf "g_SB.top[0].as_code = &%s;" function)
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
      AllocBlankPointer -> do
        writeLine (printf "heap_write_ptr(NULL);")
      AllocInt location ->
        getCLocation location >>= \l ->
          writeLine (printf "heap_write_int(%s);" l)
      AllocString location ->
        getCLocation location >>= \l ->
          writeLine (printf "heap_write_ptr(%s);" l)
      PrintError s ->
        writeLine (printf "puts(\"Error:\\n%s\");" s)
      PushUpdate -> do
        comment "pushing update frame"
        writeLine "save_SB();"
        writeLine "save_SA();"
        writeLine "g_SB.top[0].as_closure = g_NodeRegister;"
        -- If we encounter a case expression, it knows what to do here.
        writeLine "g_SB.top[1].as_code = &update_constructor;"
        writeLine "g_SB.top += 2;"
      CreateCAFClosure index -> do
        cell <- getCafCell index
        writeLine (printf "*g_CAFListLast = &%s;" cell)
        writeLine (printf "g_CAFListLast = &%s.next;" cell)
        writeLine (printf "%s.closure = heap_cursor();" cell)
        writeLine (printf "g_NodeRegister = %s.closure;" cell)
        writeLine "heap_write_info_table(&table_for_black_hole);"
        -- For padding purposes
        writeLine "heap_write_ptr(NULL);"

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
      pairs <-
        forM [1 .. argCount] <| \n -> do
          let var = argVar n
          writeLine (printf "uint8_t* %s = g_SA.top[-%d];" var n)
          return (Arg (n - 1), var)
      writeLine (printf "g_SA.top -= %d;" argCount)
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
      pairs <-
        forM [1 .. count] <| \n -> do
          let var = constructorArgVar n
          writeLine (printf "uint8_t* %s = g_SA.top[-%d];" var n)
          return (ConstructorArg (n - 1), var)
      writeLine (printf "g_SA.top -= %d;" count)
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
          pairs <-
            forM [1 .. count] <| \n -> do
              let var = buriedPtrVar n
              writeLine (printf "uint8_t* %s = g_SA.top[-%d];" var n)
              return (Buried (n - 1), var)
          writeLine (printf "g_SA.top -= %d;" count)
          return (manyLocations pairs)
        popInts 0 = return mempty
        popInts count = do
          comment "buried ints"
          pairs <-
            forM [1 .. count] <| \n -> do
              let var = buriedIntVar n
              writeLine (printf "int64_t %s = g_SB.top[-%d].as_int;" var n)
              return (BuriedInt (n - 1), var)
          writeLine (printf "g_SB.top -= %d;" count)
          return (manyLocations pairs)
        popStrings 0 = return mempty
        popStrings count = do
          comment "buried strings"
          pairs <-
            forM [1 .. count] <| \n -> do
              let var = buriedStringVar n
              writeLine (printf "uint8_t* %s = g_SA.top[-%d];" var n)
              return (BuriedString (n - 1), var)
          writeLine (printf "g_SA.top -= %d;" count)
          return (manyLocations pairs)

genCasePrelude :: CCode -> CWriter ()
genCasePrelude updateWith = do
  comment "check for constructor updates"
  writeLine "if (g_ConstrUpdateRegister != NULL) {"
  indented <| do
    writeLine updateWith
    writeLine "g_ConstrUpdateRegister = NULL;"
  writeLine "}"

genIntCases :: ArgInfo -> CCode -> CCode -> [(Int, Body)] -> Body -> CWriter ()
genIntCases buriedArgs updateWith scrut cases default' = do
  genCasePrelude updateWith
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
genStringCases buriedArgs cases default' = do
  genCasePrelude "update_with_string();"
  writeLine "char* scrut = (char*)(g_StringRegister + sizeof(InfoTable*));"
  forM_ (zip [0 ..] cases) genCase
  genDefault default'
  where
    genCase :: (Int, (String, Body)) -> CWriter ()
    genCase (i, (s, body)) = do
      let condType = if i == 0 then "if" else "} else if"
      writeLine (printf "%s (strcmp(%s, scrut) == 0) {" condType (show s))
      indented (genContinuationBody buriedArgs body)

    genDefault body = do
      writeLine "} else {"
      indented (genContinuationBody buriedArgs body)
      writeLine "}"

genFunctionBody :: Int -> ArgInfo -> FunctionBody -> CWriter ()
genFunctionBody argCount boundArgs = \case
  IntCaseBody cases default' ->
    genIntCases boundArgs "update_with_int();" "g_IntRegister" cases default'
  TagCaseBody cases default' ->
    genIntCases boundArgs "update_with_constructor();" "g_TagRegister" cases default'
  StringCaseBody cases default' ->
    genStringCases boundArgs cases default'
  ContinuationBody updateType body -> do
    let updateWith = case updateType of
          IntUpdate -> "update_with_int();"
          StringUpdate -> "update_with_string();"
    genCasePrelude updateWith
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
        evac = case closureType of
          DynamicClosure -> printf "&%s" (evacArgInfoVar boundArgs)
          _ -> "&static_evac"
    writeLine (printf "void* %s(void);" current)
    writeLine (printf "InfoTable %s = { &%s, %s };" currentTable current evac)
    -- If it this is a global, we need to create a place for the info table
    -- pointer to live
    case closureType of
      DynamicClosure -> return ()
      GlobalClosure _ ->
        writeLine (printf "InfoTable* %s = &%s;" currentPointer currentTable)
      CAFClosure _ -> do
        writeLine (printf "InfoTable* %s = &%s;" currentPointer currentTable)
        let currentCell = cafCellFor currentPath
        writeLine (printf "CAFCell %s = {&table_for_caf_cell, (uint8_t*)&%s, NULL};" currentCell currentPointer)

    forM_ subFunctions genFunction
    writeLine (printf "void* %s() {" current)
    indented <| do
      writeLine "DEBUG_PRINT(\"%s\\n\", __func__);"
      unless (argCount == 0) <| do
        case closureType of
          GlobalClosure _ ->
            writeLine (printf "g_NodeRegister = (uint8_t*)&%s;" currentPointer)
          _ -> return ()
        writeLine (printf "check_application_update(%d, %s);" argCount current)
      withSubFunctionTable subFunctions
        <| withLocations maybeAllocatedClosures
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
      ContinuationBody _ body -> inBody body
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

-- | Gather all the bound argument shapes in our program
--
-- This is nice so that we can have one garbage collection
-- pattern for each of these.
gatherBoundArgTypes :: Cmm -> Set.Set ArgInfo
gatherBoundArgTypes (Cmm functions function) =
  foldMap inFunction (function : functions)
  where
    inFunction :: Function -> Set.Set ArgInfo
    inFunction Function {..} =
      inThisFunction <> foldMap inFunction subFunctions
      where
        -- We only include this pattern if it can be potentially GCed
        inThisFunction = case closureType of
          DynamicClosure -> Set.singleton boundArgs
          _ -> Set.empty

-- | Generate the evacuation function for a certain argument shape
genEvacFunction :: ArgInfo -> CWriter ()
genEvacFunction info@ArgInfo {..} = do
  let thisFunction = evacArgInfoVar info
  writeLine (printf "uint8_t* %s(uint8_t* base) {" thisFunction)
  indented <| do
    comment "relocating closure"
    writeLine "size_t closure_size = sizeof(InfoTable*);"
    case info of
      ArgInfo 0 0 0 ->
        writeLine "closure_size += sizeof(uint8_t*);"
      _ -> do
        writeLine
          ( printf
              "closure_size += %d * sizeof(uint8_t*);"
              (boundPointers + boundStrings)
          )
        writeLine (printf "closure_size += %d * sizeof(int64_t);" boundInts)
    writeLine "uint8_t* new_base = heap_cursor();"
    writeLine "heap_write(base, closure_size);"
    comment "replacing old closure with indirection"
    writeLine "memcpy(base, &table_pointer_for_already_evac, sizeof(InfoTable*));"
    writeLine "memcpy(base + sizeof(InfoTable*), &new_base, sizeof(uint8_t*));"
    comment "evacuating roots recursively"
    writeLine "uint8_t* cursor = new_base + sizeof(InfoTable*);"
    writeLine "uint8_t* root;"
    replicateM_ boundPointers <| do
      writeLine "root = read_ptr(cursor);"
      writeLine "collect_root(&root);"
      writeLine "memcpy(cursor, &root, sizeof(uint8_t*));"
      writeLine "cursor += sizeof(uint8_t*);"
    unless (boundInts == 0)
      <| writeLine (printf "cursor += %d * sizeof(int64_t);" boundInts)
    replicateM_ boundStrings <| do
      writeLine "root = read_ptr(cursor);"
      writeLine "collect_root(&root);"
      writeLine "memcpy(cursor, &root, sizeof(uint8_t*));"
      writeLine "cursor += sizeof(uint8_t*);"
    writeLine "return new_base;"

  writeLine "}"
  writeLine ""

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
  forM_ (gatherBoundArgTypes cmm) genEvacFunction
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
  let ((globals, cafs), _) = runCWriter (gatherGlobals cmm)
   in genCmm cmm |> withGlobals globals |> withCafs cafs |> runCWriter |> snd
