module SimplifierTest (tests) where

import Lexer (lexer)
import Ourlude
import Parser (parser)
import Simplifier
import Test.Tasty
import Test.Tasty.HUnit

shouldSimplify :: String -> AST -> Assertion
shouldSimplify str ast =
  let eitherToMaybe = either (const Nothing) Just
      result = do
        tokens <- eitherToMaybe (lexer str)
        raw <- eitherToMaybe (parser tokens)
        eitherToMaybe (simplifier raw)
   in Just ast @=? result

tests :: TestTree
tests =
  testGroup
    "Simplifier Tests"
    [ testCase
        "simple definitions"
        ( shouldSimplify
            "x = 2"
            ( AST [ValueDefinition (NameDefinition "x" Nothing (LittExpr (IntLitteral 2)))]
            )
        )
    ]