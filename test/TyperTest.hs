module TyperTest (tests) where

import Lexer (lexer)
import Ourlude
import Parser (parser)
import Simplifier (simplifier)
import Test.Tasty
import Test.Tasty.HUnit
import Typer (typer)

doesType :: String -> Maybe Bool
doesType str = do
  let eitherToMaybe = either (const Nothing) Just
  tokens <- eitherToMaybe (lexer str)
  raw <- eitherToMaybe (parser tokens)
  simple <- eitherToMaybe (simplifier raw)
  return <| case typer simple of
    Left _ -> False
    Right _ -> True

shouldType :: String -> Assertion
shouldType s = Just True @=? doesType s

shouldNotType :: String -> Assertion
shouldNotType s = Just False @=? doesType s

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
      testCase "boolean operators" (shouldType "{ x = 2 == 2 || 3 == 4 && 5 == 5 }"),
      testCase "$ and ." (shouldType "{ inc = \\x -> x + 1; x = (inc . inc) $ 1 }"),
      testCase "declared types" (shouldNotType "{ x :: String; x = 3 }"),
      testCase "type synonyms" (shouldType "{ type X = Int; x :: X; x = 3 }")
    ]
