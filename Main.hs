module Main where

import Data.Char (toLower)
import Lexer (lexer)
import Ourlude
import Parser (parser)
import Simplifier (simplifier)
import System.Environment (getArgs)

-- How far does the user want us to go
data Stage = Lex | Parse | Simplify deriving (Eq)

readStage :: String -> Maybe Stage
readStage "lex" = Just Lex
readStage "parse" = Just Parse
readStage "simplify" = Just Simplify
readStage _ = Nothing

-- The arguments we'll need for our program
data Args = Args FilePath Stage

parseArgs :: [String] -> Maybe Args
parseArgs (stageName : file : _) = do
  stage <- readStage (map toLower stageName)
  return (Args file stage)
parseArgs _ = Nothing

process :: Args -> IO ()
process (Args path stage) = do
  content <- readFile path
  lex content
  where
    lex content = case lexer content of
      Left err -> do
        putStrLn "Lexer Error:"
        print err
      Right tokens ->
        if stage == Lex
          then do
            putStrLn "Tokens:"
            print tokens
          else parse tokens
    parse tokens = case parser tokens of
      Left err -> do
        putStrLn "Parser Error:"
        print err
      Right parsed ->
        if stage == Parse
          then do
            putStrLn "Parsed:"
            print parsed
          else simplify parsed
    simplify ast = case simplifier ast of
      Left err -> do
        putStrLn "Simplifier Error:"
        print err
      Right simplified ->
        if stage == Simplify
          then do
            putStrLn "Simplified:"
            print simplified
          else return ()

main :: IO ()
main = do
  stringArgs <- getArgs
  case parseArgs stringArgs of
    Nothing -> putStrLn "Unrecognized arguments"
    Just args -> process args
