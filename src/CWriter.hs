{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module CWriter (writeC) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List (intercalate)
import qualified Data.Map as Map
import Debug.Trace
import Ourlude
import STG
  ( Alts (..),
    Atom (..),
    Binding (..),
    BoxType (..),
    Builtin (..),
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

boxTypeStorage :: BoxType -> VarStorage
boxTypeStorage IntBox = IntStorage
boxTypeStorage StringBox = StringStorage

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
  deriving (Eq, Show)

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
  maybe (error ("No storage for: " ++ show name)) return maybeInfo

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
  (Case e free alts) -> do
    writeDefinitionsFor e
    genAlts free alts
  _ -> return ()

genAlts :: [String] -> Alts -> CWriter ()
genAlts deadNames alts = do
  insideFunction "$alts" (writeDefinitionsForAlts alts)
  writeLine ""
  path <- getFullPath "$alts"
  writeLine ("void* " ++ convertPath path ++ "(void) {")
  indent
  locations <- mapM resurrectName deadNames
  withLocations locations (handle alts)
  unindent
  writeLine "}"
  where
    resurrectName :: String -> CWriter (String, Location)
    resurrectName name = do
      tmp <- fresh
      storageOf name >>= \case
        PointerStorage -> do
          writeLine (printf "void* %s = SA_pop();" tmp)
          return (name, Temp tmp)
        IntStorage -> do
          writeLine (printf "int64_t %s = SB_pop_int();" tmp)
          return (name, TempInt name)
        StringStorage -> do
          writeLine (printf "const char* %s = SB_pop_str();" tmp)
          return (name, TempString name)
        -- We should know of the location then, if it's a global function
        GlobalStorage -> do
          location <- locationOf name
          return (name, location)

    writeDefinitionsForAlts :: Alts -> CWriter ()
    writeDefinitionsForAlts = \case
      BindPrim boxType n e -> withStorage n (boxTypeStorage boxType) (writeDefinitionsFor e)
      Unbox boxType n e -> withStorage n (boxTypeStorage boxType) (writeDefinitionsFor e)
      ConstrAlts as default' -> do
        forM_ (zip [(0 :: Int) ..] as) (uncurry writeDef)
        forM_ default' (insideFunction "$default" <<< writeDefinitionsFor)
        where
          writeDef i ((_, ns), e) =
            writeDefinitionsFor e
              |> insideFunction (show i)
              |> withStorages (zip ns (repeat PointerStorage))
      IntAlts as default' -> do
        forM_ (zip [(0 :: Int) ..] as) (\(i, (_, e)) -> insideFunction (show i) (writeDefinitionsFor e))
        forM_ default' (insideFunction "$default" <<< writeDefinitionsFor)
      StringAlts as default' -> do
        forM_ (zip [(0 :: Int) ..] as) (\(i, (_, e)) -> insideFunction (show i) (writeDefinitionsFor e))
        forM_ default' (insideFunction "$default" <<< writeDefinitionsFor)

    handleIntRegister :: String -> Expr -> CWriter ()
    handleIntRegister name e = do
      tmp <- fresh
      writeLine (printf "int64_t %s = RegInt;" tmp)
      withStorage name IntStorage
        <| withLocation name (TempInt tmp)
        <| genExpr e

    handleStringRegister :: String -> Expr -> CWriter ()
    handleStringRegister name e = do
      tmp <- fresh
      writeLine (printf "const char* %s = RegString;" tmp)
      withStorage name StringStorage
        <| withLocation name (TempString tmp)
        <| genExpr e

    handle :: Alts -> CWriter ()
    handle (BindPrim IntBox name e) = handleIntRegister name e
    handle (Unbox IntBox name e) = handleIntRegister name e
    handle (BindPrim StringBox name e) = handleStringRegister name e
    handle (Unbox StringBox name e) = handleStringRegister name e
    handle (IntAlts as default') = do
      writeLine "switch (RegInt) {"
      indent
      forM_ as <| \(i, e) -> do
        writeLine (printf "case %d: {" i)
        indent
        genExpr e
        writeLine "break;"
        unindent
        writeLine "}"
      case default' of
        Nothing -> return ()
        Just e -> do
          writeLine "default: {"
          indent
          genExpr e
          unindent
          writeLine "}"
      unindent
      writeLine "}"
    handle (StringAlts as default') = do
      genIfElse as
      case default' of
        Nothing -> writeLine "}"
        Just e -> do
          writeLine "} else {"
          indent
          genExpr e
          unindent
          writeLine "}"
      where
        genIfElse :: [(String, Expr)] -> CWriter ()
        genIfElse [] = return ()
        genIfElse ((firstS, firstE) : xs) = do
          writeLine (printf "if (RegString == %s) {" (show firstS))
          indent
          genExpr firstE
          unindent
          forM_ xs <| \(s, e) -> do
            writeLine (printf "} else if (RegString == %s) {" (show s))
            indent
            genExpr e
            unindent
    handle (ConstrAlts as default') = do
      writeLine "switch (RegTag) {"
      indent
      forM_ as <| \((tag, names), e) -> do
        writeLine (printf "case %d: {" tag)
        indent
        -- We know that all the constructor arguments are boxed, and on the stack, in straightforward order
        located <- mapM grabNameFromStack names
        withLocations located <| withStorages (zip names (repeat PointerStorage)) <| genExpr e
        unindent
        writeLine "}"
      case default' of
        Nothing -> return ()
        Just e -> do
          writeLine "default: {"
          indent
          genExpr e
          unindent
          writeLine "}"
      unindent
      writeLine "}"
      where
        grabNameFromStack name = do
          tmp <- fresh
          writeLine (printf "void* %s = SA_pop();" tmp)
          return (name, Temp tmp)

genExpr :: Expr -> CWriter ()
genExpr (Error s) = do
  writeLine "puts(\"Error:\");"
  writeLine (printf "puts(\"%s\")" s)
  writeLine "return NULL;"
genExpr (Primitive p) = do
  case p of
    PrimInt i -> writeLine (printf "RegInt = %d;" i)
    PrimString s -> writeLine (printf "RegString = \"%s\";" s)
  writeLine "return SB_pop();"
genExpr (Apply function atoms) = do
  -- Reverse order, so that we can pop in argument order, and get our args
  forM_ (reverse atoms) <| \case
    NameAtom n ->
      locationOf n >>= \case
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
      writeLine (printf "memcpy(&%s, RegNode, sizeof(InfoTable*));" ret)
      writeLine (printf "return %s->entry;" ret)
    CurrentNode -> do
      ret <- fresh
      writeLine (printf "InfoTable* %s;" ret)
      writeLine (printf "memcpy(&%s, RegNode, sizeof(InfoTable*));" ret)
      writeLine (printf "return %s->entry;" ret)
    GlobalFunction path -> do
      writeLine (printf "return &%s;" (convertPath path))
genExpr (Builtin b atoms) = case b of
  Add -> int2Op "+"
  Sub -> int2Op "-"
  Div -> int2Op "/"
  Mul -> int2Op "/"
  Less -> int2Op "<"
  LessEqual -> int2Op "<="
  Greater -> int2Op ">"
  GreaterEqual -> int2Op ">="
  EqualTo -> int2Op "=="
  NotEqualTo -> int2Op "!="
  Negate -> do
    int1 <- asInt (head atoms)
    writeLine (printf "RegInt = -%s;" int1)
    writeLine "return SB_pop();"
  ExitWithInt -> do
    int1 <- asInt (head atoms)
    writeLine (printf "printf(\"%%d\\n\", %s);" int1)
    writeLine "return NULL;"
  ExitWithString -> do
    str1 <- asInt (head atoms)
    writeLine (printf "printf(\"%%s\\n\", %s);" str1)
    writeLine "return NULL;"
  Concat -> do
    (str1, str2) <- grab2Strings atoms
    writeLine (printf "RegString = H_concat(%s, %s);" str1 str2)
    writeLine "return SB_pop()"
  where
    asInt (PrimitiveAtom (PrimInt i)) = return (show i)
    asInt (NameAtom n) =
      locationOf n >>= \case
        TempInt tmp -> return tmp
        loc -> do
          ask >>= \r -> traceShow r (return ())
          error (printf "location %s does not contain int" (show loc))
    asInt arg = error (printf "arg %s cannot be used as int" (show arg))

    asString (PrimitiveAtom (PrimString s)) = return (show s)
    asString (NameAtom n) =
      locationOf n >>= \case
        TempString tmp -> return tmp
        loc -> error (printf "location %s does not contain string" (show loc))
    asString arg = error (printf "arg %s cannot be used as string" (show arg))

    grab2Ints [arg1, arg2] = do
      int1 <- asInt arg1
      int2 <- asInt arg2
      return (int1, int2)
    grab2Ints as = error (printf "expected two arguments to %s: got %d" (show b) (length as))

    grab2Strings [arg1, arg2] = do
      str1 <- asString arg1
      str2 <- asString arg2
      return (str1, str2)
    grab2Strings as = error (printf "expected two arguments to %s: got %d" (show b) (length as))

    int2Op op = do
      (int1, int2) <- grab2Ints atoms
      writeLine (printf "RegInt = %s %s %s;" int1 op int2)
      writeLine "return SB_pop();"
genExpr (Case scrut free _) = do
  altsPath <- getFullPath "$alts"
  forM_ (reverse free) <| locationOf >=> \case
    Temp tmp -> writeLine (printf "SA_push(%s);" tmp)
    TempInt tmp -> writeLine (printf "SB_push_int(%s);" tmp)
    TempString tmp -> writeLine (printf "SB_push_str(%s);" tmp)
    CurrentNode -> writeLine "SB_push(RegNode);"
    GlobalFunction _ -> return ()
  writeLine (printf "SB_push(&%s);" (convertPath altsPath))
  genExpr scrut
genExpr _ = do
  writeLine "panic(\"UNIMPLEMENTED\");"
  writeLine "return NULL;"

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
      insideFunction myName <| withAllocatedArguments <| genExpr expr
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
