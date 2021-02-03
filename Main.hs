module Main where

import Control.Monad ((>=>))
import Data.Char (toLower)
import qualified Lexer
import Ourlude
import qualified Parser
import qualified Simplifier
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Pretty.Simple (pPrint, pPrintString)

data StagedError = StagedError String String

stageEither :: Show e => String -> Either e a -> Either StagedError a
stageEither name m = case m of
  Left e -> Left (StagedError name (show e))
  Right a -> Right a

printStagedError :: StagedError -> IO ()
printStagedError (StagedError name err) = do
  putStr name
  putStrLn " Error:"
  pPrintString err

data Stage i o = Stage
  { name :: String,
    runStage :: i -> Either StagedError o
  }

makeStage :: Show e => String -> (i -> Either e o) -> Stage i o
makeStage name r = Stage name (r >>> stageEither name)

lexerStage :: Stage String [Lexer.Token]
lexerStage = makeStage "Lexer" Lexer.lexer

parserStage :: Stage [Lexer.Token] Parser.AST
parserStage = makeStage "Parser" Parser.parser

simplifierStage :: Stage Parser.AST (Simplifier.AST ())
simplifierStage = makeStage "Simplifier" Simplifier.simplifier

(>->) :: Stage a b -> Stage b c -> Stage a c
(>->) (Stage _ r1) (Stage n2 r2) = Stage n2 (r1 >=> r2)

printStage :: Show b => Stage a b -> a -> IO ()
printStage (Stage name r) a = case r a of
  Left err -> do
    printStagedError err
    exitFailure
  Right b -> do
    putStrLn (name ++ ":")
    pPrint b

data Args = Args FilePath (String -> IO ())

readStage :: String -> Maybe (String -> IO ())
readStage "lex" =
  lexerStage |> printStage |> Just
readStage "parse" =
  lexerStage >-> parserStage |> printStage |> Just
readStage "simplify" =
  lexerStage >-> parserStage >-> simplifierStage |> printStage |> Just
readStage _ = Nothing

process :: Args -> IO ()
process (Args path stage) = do
  content <- readFile path
  stage content

parseArgs :: [String] -> Maybe Args
parseArgs (stageName : file : _) =
  Args file <$> readStage (map toLower stageName)
parseArgs _ = Nothing

main :: IO ()
main = do
  stringArgs <- getArgs
  case parseArgs stringArgs of
    Nothing -> putStrLn "Unrecognized arguments"
    Just args -> process args
