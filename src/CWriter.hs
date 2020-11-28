{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module CWriter (writeC) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List (intercalate)
import qualified Data.Map as Map
import Ourlude
import STG
  ( Alts (..),
    Atom (..),
    Binding (..),
    Expr (..),
    LambdaForm (..),
    Primitive (..),
    STG (..),
    Updateable (..),
  )
import Text.Printf (printf)

-- A type for CCode.
--
-- We could use something more efficient than a string, but this is ok
-- for our explanation purposes
type CCode = String

-- A type for indentation levels
type Indent = Int

newtype IdentPath = IdentPath [String] deriving (Eq, Show)

instance Semigroup IdentPath where
  IdentPath p1 <> IdentPath p2 = IdentPath (p2 <> p1)

instance Monoid IdentPath where
  mempty = IdentPath []

ident :: String -> IdentPath
ident name = IdentPath [name]

-- Convert an identifier path to a C identifier
convertPath :: IdentPath -> String
convertPath (IdentPath ps) = reverse ps |> map convertIdentifier |> intercalate "_"
  where
    convertIdentifier :: String -> String
    convertIdentifier name
      | '$' `elem` name || '#' `elem` name || head name `elem` "0123456789" =
        "gen_" ++ (name |> replace '$' "S" |> replace '#' "P")
      where
        replace char by str = foldMap (\c -> if c == char then by else [c]) str
    convertIdentifier name = "user_" ++ name

tableFor :: IdentPath -> String
tableFor path = "table_for_" ++ convertPath path

-- The storage type that a variable has
data VarStorage
  = -- A variable that holds a void*
    PointerStorage
  | -- A variable holding an integer
    IntStorage
  | -- A variable holding a char*
    StringStorage
  | -- A variable holding a global storage
    GlobalStorage
  deriving (Eq, Show)

-- A location some variable can be
data Location
  = -- A temporary variable with some name
    Temp String
  | -- A temporary variable pointing to an int
    TempInt String
  | -- A temporary variable pointing to a String
    TempString String
  | -- Global function with a certain identifier path
    GlobalFunction IdentPath
  | -- The location of this thing is the current node
    CurrentNode
  deriving (Eq, Show)

data Env = Env
  { -- The current function in the environment
    currentFunction :: IdentPath,
    -- The information we have about what storage type a variable has
    varStorages :: Map.Map String VarStorage,
    -- The information we have about what location a variable is in
    varLocations :: Map.Map String Location
  }

defaultEnv :: Env
defaultEnv = Env mempty mempty mempty

data CState = CState
  { currentIndent :: Indent,
    varCount :: Int
  }

defaultState :: CState
defaultState = CState 0 0

newtype CWriter a = CWriter (StateT CState (ReaderT Env (Writer CCode)) a)
  deriving (Functor, Applicative, Monad, MonadWriter CCode, MonadState CState, MonadReader Env)

runCWriter :: CWriter () -> CCode
runCWriter (CWriter m) = runStateT m defaultState |> (`runReaderT` defaultEnv) |> execWriter

indentAmount :: Indent
indentAmount = 2

indent :: CWriter ()
indent = modify' (\s -> s {currentIndent = currentIndent s + indentAmount})

unindent :: CWriter ()
unindent = modify' (\s -> s {currentIndent = max 0 (currentIndent s - indentAmount)})

fresh :: CWriter String
fresh = do
  count <- gets varCount
  modify' (\s -> s {varCount = count + 1})
  return ("x" ++ show count)

writeLine :: CCode -> CWriter ()
writeLine code = do
  amount <- gets currentIndent
  tell (replicate amount ' ')
  tell code
  tell "\n"

locationOf :: String -> CWriter Location
locationOf name = do
  maybeInfo <- asks (varLocations >>> Map.lookup name)
  maybe (error ("No location for: " ++ show name)) return maybeInfo

storageOf :: String -> CWriter VarStorage
storageOf name = do
  maybeInfo <- asks (varStorages >>> Map.lookup name)
  maybe (error ("No location for: " ++ show name)) return maybeInfo

withLocation :: String -> Location -> CWriter a -> CWriter a
withLocation name location = withLocations [(name, location)]

withLocations :: [(String, Location)] -> CWriter a -> CWriter a
withLocations mp = local (\r -> r {varLocations = Map.fromList mp <> varLocations r})

withStorage :: String -> VarStorage -> CWriter a -> CWriter a
withStorage name storage = withStorages [(name, storage)]

withStorages :: [(String, VarStorage)] -> CWriter a -> CWriter a
withStorages mp = local (\r -> r {varStorages = Map.fromList mp <> varStorages r})

insideFunction :: String -> CWriter a -> CWriter a
insideFunction name m = do
  fullPath <- getFullPath name
  local (\r -> r {currentFunction = fullPath}) m

getFullPath :: String -> CWriter IdentPath
getFullPath name = do
  current <- asks currentFunction
  return (current <> ident name)

-- Mark all of the closures that can be made global inside a let expression
--
-- We need to do this before we create the definitions for these functions,
-- because how they might call themselves recursively depends on this information,
-- and they want this information for the other definitions, which might
-- depend on eachother mutually.
withBindingStorages :: [Binding] -> CWriter a -> CWriter a
withBindingStorages bindings m = do
  let withBestStorage (Binding name' (LambdaForm [] N _ _)) = (name', GlobalStorage)
      withBestStorage (Binding name _) = (name, PointerStorage)
      storages = map withBestStorage bindings
  withStorages storages m

writeDefinitionsFor :: Expr -> CWriter ()
writeDefinitionsFor = \case
  (Let bindings e) ->
    withBindingStorages bindings <| do
      forM_ bindings (\(Binding name lf) -> genLambdaForm name lf)
      writeDefinitionsFor e
  (Case e _ alts) -> do
    writeDefinitionsFor e
    genAlts alts
  _ -> return ()

genAlts :: Alts -> CWriter ()
genAlts alts = do
  insideFunction "$alts" (writeDefinitionsForAlts alts)
  writeLine ""
  path <- getFullPath "$alts"
  writeLine ("void* " ++ convertPath path ++ "(void) {")
  indent
  writeLine "return NULL;"
  unindent
  writeLine "}"
  where
    writeDefinitionsForAlts :: Alts -> CWriter ()
    writeDefinitionsForAlts = \case
      IntPrim _ e -> writeDefinitionsFor e
      StringPrim _ e -> writeDefinitionsFor e
      ConstrAlts as default' -> do
        forM_ (zip [(0 :: Int) ..] as) (\(i, (_, e)) -> insideFunction (show i) (writeDefinitionsFor e))
        forM_ default' (insideFunction "$default" <<< writeDefinitionsFor)
      IntAlts as default' -> do
        forM_ (zip [(0 :: Int) ..] as) (\(i, (_, e)) -> insideFunction (show i) (writeDefinitionsFor e))
        forM_ default' (insideFunction "$default" <<< writeDefinitionsFor)
      StringAlts as default' -> do
        forM_ (zip [(0 :: Int) ..] as) (\(i, (_, e)) -> insideFunction (show i) (writeDefinitionsFor e))
        forM_ default' (insideFunction "$default" <<< writeDefinitionsFor)

genLambdaForm :: String -> LambdaForm -> CWriter ()
genLambdaForm myName (LambdaForm bound _ args expr) =
  -- We know that all of the arguments will be pointers
  withMyOwnLocation
    <| withStorages (zip args (repeat PointerStorage))
    <| do
      insideFunction myName (writeDefinitionsFor expr)
      writeLine ""
      myPath <- getFullPath myName
      writeLine (printf "void* %s(void) {" (convertPath myPath))
      indent
      withAllocatedArguments (handle expr)
      unindent
      writeLine "}"
      -- Only write the info table if this isn't a globally stored function
      storageOf myName >>= \case
        GlobalStorage -> return ()
        _ -> writeLine (printf "InfoTable %s = { &%s, NULL, NULL };" (tableFor myPath) (convertPath myPath))
  where
    withMyOwnLocation :: CWriter a -> CWriter a
    withMyOwnLocation m =
      storageOf myName >>= \case
        GlobalStorage -> do
          myPath <- getFullPath myName
          withLocation myName (GlobalFunction myPath) m
        PointerStorage -> withLocation myName CurrentNode m
        badStorage -> error (printf "the function %s cannot be stored inside %s" myName (show badStorage))

    locationsForBound :: [String] -> CWriter [(String, Location)]
    locationsForBound args' = do
      -- If the variable is bound, we must already have a storage for it
      storages <- forM args' (\arg -> (arg,) <$> storageOf arg)
      let pluck s = storages |> filter (snd >>> (== s)) |> map fst
          pointers = pluck PointerStorage
          ints = pluck IntStorage
          strings = pluck StringStorage
          globals = pluck GlobalStorage
      pointerLocs <-
        forM (zip [(0 :: Int) ..] pointers) <| \(i, arg) -> do
          tmp <- fresh
          writeLine (printf "void* %s;" tmp)
          writeLine (printf "memcpy(&%s, RegNode + sizeof(InfoTable*) + sizeof(void*) * %d, sizeof(void*));" tmp i)
          return (arg, Temp tmp)
      let pointerCount = length pointers
      intLocs <-
        forM (zip [(0 :: Int) ..] ints) <| \(i, arg) -> do
          tmp <- fresh
          writeLine (printf "int64_t %s;" tmp)
          writeLine (printf "memcpy(&%s, RegNode + sizeof(InfoTable*) + sizeof(void*) * %d + sizeof(int64_t) * %d, sizeof(int64_t));" tmp pointerCount i)
          return (arg, TempInt tmp)
      let intCount = length ints
      stringLocs <-
        forM (zip [(0 :: Int) ..] strings) <| \(i, arg) -> do
          tmp <- fresh
          writeLine (printf "char* %s;" tmp)
          writeLine (printf "memcpy(&%s, RegNode + sizeof(InfoTable*) + sizeof(void*) * %d + sizeof(int64_t) * %d + sizeof(char*) * %d, sizeof(int64_t));" tmp pointerCount intCount i)
          return (arg, TempString tmp)
      globalLocs <- forM globals (\arg -> (arg,) <$> locationOf arg)
      return (pointerLocs <> intLocs <> stringLocs <> globalLocs)

    locationsForArgs :: [String] -> CWriter [(String, Location)]
    locationsForArgs =
      mapM <| \arg -> do
        tmp <- fresh
        writeLine (printf "void* %s = SA_pop();" tmp)
        return (arg, Temp tmp)

    -- This will allocate temporary C variables for all the arguments
    -- passed to us on the stack, and the closure arguments we have
    withAllocatedArguments :: CWriter a -> CWriter a
    withAllocatedArguments m = do
      boundLocations <- locationsForBound bound
      argLocations <- locationsForArgs args
      withLocations (boundLocations <> argLocations) m

    handle :: Expr -> CWriter ()
    handle (Error s) = do
      writeLine "puts(\"Error:\");"
      writeLine (printf "puts(\"%s\")" s)
      writeLine "return NULL;"
    handle (Primitive p) = do
      case p of
        PrimInt i -> writeLine (printf "RegInt = %d;" i)
        PrimString s -> writeLine (printf "RegString = \"%s\";" s)
      writeLine "return SB_pop();"
    handle (Apply function atoms) = do
      forM_ atoms <| \case
        NameAtom n -> locationOf n >>= \case
          TempInt _ -> error (printf "function %s cannot be applied to primitive int" function)
          TempString _ -> error (printf "function %s cannot be applied to primitive string" function)
          Temp tmp ->
            writeLine (printf "SA_push(%s);" tmp)
          GlobalFunction path ->
            writeLine (printf "SA_push(&%s);" (convertPath path))
          CurrentNode ->
            writeLine "SA_push(RegNode);"
        -- This is an implementation detail of our compiler, really
        -- In practice Haskell allows functions to accept primitive arguments,
        -- but we only have primitive values as intermediates
        PrimitiveAtom p ->
          error (printf "function %s cannot be applied to primitive %s" function (show p))
      locationOf function >>= \case
        TempInt _ -> error (printf "Cannot call function %s with int location" function)
        TempString _ -> error (printf "Cannot call function %s with string location" function)
        Temp tmp -> do
          ret <- fresh 
          writeLine (printf "RegNode = %s;" tmp)
          writeLine (printf "InfoTable* %s;" ret)
          writeLine (printf "memcpy(&%s, RegNode, sizeof(Infotable*));" ret)
          writeLine (printf "return %s->entry;" ret)
        CurrentNode -> do
          ret <- fresh 
          writeLine (printf "InfoTable* %s;" ret)
          writeLine (printf "memcpy(&%s, RegNode, sizeof(Infotable*));" ret)
          writeLine (printf "return %s->entry;" ret)
        GlobalFunction path -> do
          writeLine (printf "return &%s;" (convertPath path))
    handle _ = writeLine "return NULL;"

generate :: STG -> CWriter ()
generate (STG bindings entry) = do
  writeLine "#include \"runtime.c\""
  let topLevelNames = map (\(Binding name _) -> name) bindings
      globalStorages = zip topLevelNames (repeat GlobalStorage)
      globalLocations = map (\name -> (name, GlobalFunction (ident name))) topLevelNames
  entryPath <- getFullPath "$entry"
  withLocations globalLocations <| withStorages globalStorages <| do
    forM_ bindings (\(Binding name form) -> genLambdaForm name form)
    withLocation "$entry" (GlobalFunction entryPath)
      <| withStorage "$entry" GlobalStorage
      <| genLambdaForm "$entry" entry
  writeLine ""
  writeLine "int main() {"
  indent
  writeLine (printf "CodeLabel label = &%s;" (convertPath entryPath))
  writeLine "while (label != NULL) {"
  indent
  writeLine "label = label();"
  unindent
  writeLine "}"
  writeLine "return 0;"
  unindent
  writeLine "}"

writeC :: STG -> CCode
writeC stg = runCWriter (generate stg)
