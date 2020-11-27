{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CWriter (writeC) where

import Control.Monad.State
import Control.Monad.Writer
import Ourlude
import STG (STG (..), Binding(..))

-- A type for CCode.
--
-- We could use something more efficient than a string, but this is ok
-- for our explanation purposes
type CCode = String

-- A type for indentation levels
type Indent = Int

newtype CWriter a = CWriter (StateT Indent (Writer CCode) a)
  deriving (Functor, Applicative, Monad, MonadWriter CCode, MonadState Indent)

runCWriter :: CWriter () -> CCode
runCWriter (CWriter m) = runStateT m 0 |> execWriter

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

-- Convert an identifier
--
-- We do this in a way that generates both valid C, and allows us
-- to never clash between generated names, user names, and runtime
-- C functions
convertIdentifier :: String -> String
convertIdentifier name
  | '$' `elem` name || '#' `elem` name =
    "gen_" ++ (name |> replace '$' "_S_" |> replace '#' "_P_")
  where
    replace char by str = foldMap (\c -> if c == char then by else [c]) str
convertIdentifier name = "user_" ++ name

genLambdaForm :: String -> CWriter ()
genLambdaForm ident = do
  writeLine ""
  writeLine ("void " ++ ident ++ "(void) {")
  indent
  writeLine "return;"
  unindent
  writeLine "}"

generate :: STG -> CWriter ()
generate (STG bindings _) = do
    writeLine "#include \"runtime.c\""
    forM_ bindings (\(Binding name _) -> genLambdaForm (convertIdentifier name))
    genLambdaForm "entry"
    writeLine ""
    writeLine "int main() {"
    indent
    writeLine "return 0;"
    unindent
    writeLine "}" 

writeC :: STG -> CCode
writeC stg = runCWriter (generate stg)

