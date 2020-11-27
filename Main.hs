module Main where

import Control.Monad ((>=>))
import Data.Char (toLower)
import qualified Lexer
import Ourlude
import qualified Parser
import qualified STG
import qualified Simplifier
import System.Environment (getArgs)
import Types (Scheme)
import Text.Pretty.Simple (pPrint, pPrintString)
import qualified Typer

-- An error that occurrs in a stage, including its name and what went wrong
data StagedError = StagedError String String

-- Stage a result with an error that can be shown, given that stage's name
stageEither :: Show e => String -> Either e a -> Either StagedError a
stageEither name m = case m of
  Left e -> Left (StagedError name (show e))
  Right a -> Right a

-- Print a staged error
printStagedError :: StagedError -> IO ()
printStagedError (StagedError name err) = do
  putStrLn (name ++ " Error:")
  pPrintString err

-- Represents a stage taking input `a` and producing output `b`
data Stage a b = Stage
  { -- The name of the stage
    name :: String,
    -- The function allowing us to run a stage on some input, producing an error
    runStage :: a -> Either StagedError b
  }

-- Make a stage, given a name, and a function producing a result that we can show
makeStage :: Show e => String -> (a -> Either e b) -> Stage a b
makeStage name r = Stage name (r >>> stageEither name)

-- Compose two stages together
(>->) :: Stage a b -> Stage b c -> Stage a c
(>->) (Stage _ r1) (Stage n2 r2) = Stage n2 (r1 >=> r2)

-- Execute a stage by printing out the result or errors
execStage :: Show b => Stage a b -> a -> IO ()
execStage (Stage name r) a = case r a of
  Left err -> printStagedError err
  Right b -> do
    putStrLn (name ++ ":")
    pPrint b

lexerStage :: Stage String [Lexer.Token]
lexerStage = makeStage "Lexer" Lexer.lexer

parserStage :: Stage [Lexer.Token] Parser.AST
parserStage = makeStage "Parser" Parser.parser

simplifierStage :: Stage Parser.AST (Simplifier.AST ())
simplifierStage = makeStage "Simplifier" Simplifier.simplifier

typerStage :: Stage (Simplifier.AST ()) (Simplifier.AST Scheme)
typerStage = makeStage "Typer" Typer.typer

stgStage :: Stage (Simplifier.AST Scheme) STG.STG
stgStage = makeStage "STG" STG.stg

-- Read out which stages to execute based on a string
readStage :: String -> Maybe (String -> IO ())
readStage "lex" =
  lexerStage |> execStage |> Just
readStage "parse" =
  lexerStage >-> parserStage |> execStage |> Just
readStage "simplify" =
  lexerStage >-> parserStage >-> simplifierStage |> execStage |> Just
readStage "type" =
  lexerStage >-> parserStage >-> simplifierStage >-> typerStage |> execStage |> Just
readStage "stg" =
  lexerStage >-> parserStage >-> simplifierStage >-> typerStage >-> stgStage |> execStage |> Just
readStage _ = Nothing


-- The arguments we'll need for our program
data Args = Args FilePath (String -> IO ())

parseArgs :: [String] -> Maybe Args
parseArgs (stageName : file : _) = do
  stage <- readStage (map toLower stageName)
  return (Args file stage)
parseArgs _ = Nothing

process :: Args -> IO ()
process (Args path stage) = do
  content <- readFile path
  stage content

main :: IO ()
main = do
  stringArgs <- getArgs
  case parseArgs stringArgs of
    Nothing -> putStrLn "Unrecognized arguments"
    Just args -> process args
