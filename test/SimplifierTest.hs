module SimplifierTest (tests) where

import Lexer (lexer)
import Ourlude
import Parser (parser)
import Simplifier
import Test.Tasty
import Test.Tasty.HUnit

shouldSimplify :: String -> Assertion
shouldSimplify str =
  let eitherToMaybe = either (const Nothing) Just
      result = do
        tokens <- eitherToMaybe (lexer str)
        raw <- eitherToMaybe (parser tokens)
        eitherToMaybe (simplifier raw)
        Just True
   in Just True @=? result

tests :: TestTree
tests =
  testGroup
    "Simplifier Tests"
    [ testCase
        "simple definitions"
        ( shouldSimplify
            "x = 2"
        ),
      testCase
        "single argument functions"
        ( shouldSimplify
            "f x = 3"
        ),
      testCase
        "wildcard argument functions"
        ( shouldSimplify
            "f _ = 3"
        ),
      testCase
        "singular multi argument definition"
        ( shouldSimplify
            "f _ _ = 3"
        ),
      testCase
        "disjoint definitions"
        ( shouldSimplify
            "{ f 3 = 3; f 4 = 4 }"
        ),
      testCase
        "catch all definitions"
        ( shouldSimplify
            "{ f 3 = 3; f _ = 3 }"
        )
    ]