module STGTest (tests) where

import Lexer (lexer)
import Ourlude
import Parser (parser)
import STG (STG (..), stg)
import Simplifier (simplifier)
import Test.Tasty
import Test.Tasty.HUnit
import Typer (typer)

doesCompile :: String -> Maybe Bool
doesCompile str = do
  let eitherToMaybe = either (const Nothing) Just
  tokens <- eitherToMaybe (lexer str)
  raw <- eitherToMaybe (parser tokens)
  simple <- eitherToMaybe (simplifier raw)
  typed <- eitherToMaybe (typer simple)
  -- Hack to fully evaluate STG
  return ('\0' `notElem` (typed |> stg |> show))

shouldCompile :: String -> Assertion
shouldCompile s = Just True @=? doesCompile s

tests :: TestTree
tests =
  testGroup
    "STG Tests"
    [ testCase "litterals" (shouldCompile "{ x = 3; y = \"foo\"; y = True }"),
      testCase "one level arithmetic" (shouldCompile "{ x = 2 + 2 }")
    ]