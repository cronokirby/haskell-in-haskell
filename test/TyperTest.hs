module TyperTest (tests) where

import Data.Maybe (isJust)
import Lexer (lexer)
import Ourlude
import Parser (parser)
import Simplifier (simplifier)
import Test.Tasty
import Test.Tasty.HUnit
import Typer (typer)

doesType :: String -> Bool
doesType str =
  let eitherToMaybe = either (const Nothing) Just
      result = do
        tokens <- eitherToMaybe (lexer str)
        raw <- eitherToMaybe (parser tokens)
        simple <- eitherToMaybe (simplifier raw)
        eitherToMaybe (typer simple)
   in isJust result

shouldType :: String -> Assertion
shouldType s = True @=? doesType s

shouldNotType :: String -> Assertion
shouldNotType s = False @=? doesType s

tests :: TestTree
tests =
  testGroup
    "Typer Tests"
    [ testCase "arithmetic assignments" (shouldType "{ x = 3; y = x + x }"),
      testCase "string + int fails" (shouldNotType "{ x = 3 + \"foo\" }"),
      testCase "let propagates types" (shouldNotType "{ x = let { y = 3 } in y; x1 = x ++ \"foo\" }"),
      testCase "comparisons 1" (shouldNotType "{ x = 2 > True }"),
      testCase "comparisons 2" (shouldNotType "{ x = 2 >= True }"),
      testCase "comparisons 3" (shouldNotType "{ x = 2 == True }"),
      testCase "comparisons 4" (shouldNotType "{ x = 2 /= True }"),
      testCase "comparisons 5" (shouldNotType "{ x = 2 <= True }"),
      testCase "comparisons 6" (shouldNotType "{ x = 2 < True }"),
      testCase "boolean operators" (shouldType "{ x = 2 == 2 || 3 == 4 && 5 == 5 }")
    ]
