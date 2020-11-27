{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CWriter (writeC) where

import Control.Monad.State
import Control.Monad.Writer
import Ourlude
import STG (STG (..))

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

writeC :: STG -> CCode
writeC _ = runCWriter <| do
  writeLine "int main() {"
  indent
  writeLine "return 0;"
  unindent
  writeLine "}"
