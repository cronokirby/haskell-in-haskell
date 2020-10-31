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
        ),
      testCase
        "single argument functions"
        ( shouldSimplify
            "f x = 3"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "f"
                        Nothing
                        ( LambdaExpr
                            "$0"
                            ( CaseExpr
                                (NameExpr "$0")
                                [PatternDef (NamePattern "x") (LittExpr (IntLitteral 3))]
                            )
                        )
                    )
                ]
            )
        ),
      testCase
        "wildcard argument functions"
        ( shouldSimplify
            "f _ = 3"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "f"
                        Nothing
                        ( LambdaExpr
                            "$0"
                            ( CaseExpr
                                (NameExpr "$0")
                                [PatternDef WildcardPattern (LittExpr (IntLitteral 3))]
                            )
                        )
                    )
                ]
            )
        )
    ]