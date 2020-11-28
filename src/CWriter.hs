{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CWriter (writeC) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Ourlude
import STG (Binding (..), STG (..))

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
convertPath (IdentPath ps) = reverse ps |> foldMap convertIdentifier
  where
    convertIdentifier :: String -> String
    convertIdentifier name
      | '$' `elem` name || '#' `elem` name =
        "gen_" ++ (name |> replace '$' "_S_" |> replace '#' "_P_")
      where
        replace char by str = foldMap (\c -> if c == char then by else [c]) str
    convertIdentifier name = "user_" ++ name

tableFor :: IdentPath -> String
tableFor path = "table_for_" ++ convertPath path

newtype Env = Env
  { -- The current function in the environment
    currentFunction :: IdentPath
  }

defaultEnv :: Env
defaultEnv = Env mempty

newtype CWriter a = CWriter (StateT Indent (ReaderT Env (Writer CCode)) a)
  deriving (Functor, Applicative, Monad, MonadWriter CCode, MonadState Indent, MonadReader Env)

runCWriter :: CWriter () -> CCode
runCWriter (CWriter m) = runStateT m 0 |> (`runReaderT` defaultEnv) |> execWriter

indentAmount :: Indent
indentAmount = 2

indent :: CWriter ()
indent = modify' (+ indentAmount)

unindent :: CWriter ()
unindent = modify' (\x -> max 0 (x - indentAmount))

writeLine :: CCode -> CWriter ()
writeLine code = do
  amount <- get
  tell (replicate amount ' ')
  tell code
  tell "\n"

insideFunction :: String -> CWriter a -> CWriter a
insideFunction name m = do
  fullPath <- getFullPath name
  local (\r -> r {currentFunction = fullPath}) m

getFullPath :: String -> CWriter IdentPath
getFullPath name = do
  current <- asks currentFunction
  return (current <> ident name)

genLambdaForm :: String -> CWriter ()
genLambdaForm name = do
  path <- getFullPath name
  writeLine ""
  writeLine ("void* " ++ convertPath path ++ "(void) {")
  indent
  writeLine "return NULL;"
  unindent
  writeLine "}"
  writeLine ("InfoTable " ++ tableFor path ++ " = { &" ++ convertPath path ++ ", NULL, NULL };")

generate :: STG -> CWriter ()
generate (STG bindings _) = do
  writeLine "#include \"runtime.c\""
  forM_ bindings (\(Binding name _) -> genLambdaForm name)
  genLambdaForm "$entry"
  entryPath <- getFullPath "$entry"
  writeLine ""
  writeLine "int main() {"
  indent
  writeLine ("CodeLabel label = &" ++ convertPath entryPath ++ ";")
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
