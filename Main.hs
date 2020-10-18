module Main where

import Data.Char (toLower)
import Lexer (lexer)
import Ourlude
import System.Environment (getArgs)

-- How far does the user want us to go
data Stage = Lex deriving (Eq)

readStage :: String -> Maybe Stage
readStage "lex" = Just Lex
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
            putStrLn "Tokens"
            print tokens
          else return ()

main :: IO ()
main = do
  stringArgs <- getArgs
  print stringArgs
  case parseArgs stringArgs of
    Nothing -> putStrLn "Unrecognized arguments"
    Just args -> process args
