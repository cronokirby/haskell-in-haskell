module ParserTest (tests) where

import Lexer (lexer)
import Ourlude
import Parser
import Test.Tasty
import Test.Tasty.HUnit

shouldParse :: String -> AST -> Assertion
shouldParse str ast =
  let eitherToMaybe = either (const Nothing) Just
      tokens = lexer str
      result =
        eitherToMaybe tokens >>= \toks ->
          let parsed = parse toks
           in eitherToMaybe parsed
   in Just ast @=? result

tests :: TestTree
tests =
  testGroup
    "Parser Tests"
    [ testCase
        "parsing basic definitions"
        ( shouldParse
            "{ x :: Int; x = 3 }"
            (AST [ValueDefinition (TypeAnnotation "x" IntType), ValueDefinition (NameDefinition "x" (LittExpr (IntLitteral 3)))])
        ),
      testCase
        "parsing let definitions"
        ( shouldParse
            "x = \n  let\n    y = 3\n  in y"
            (AST [ValueDefinition (NameDefinition "x" (LetExpr [NameDefinition "y" (LittExpr (IntLitteral 3))] (NameExpr "y")))])
        ),
      testCase
        "parsing where definitions"
        ( shouldParse
            "x = y where y = 3"
            (AST [ValueDefinition (NameDefinition "x" (WhereExpr (NameExpr "y") [NameDefinition "y" (LittExpr (IntLitteral 3))]))])
        ),
      testCase
        "parsing binary expressions"
        ( shouldParse
            "x = a + f a * a / a - a ++ a"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "x"
                        ( BinExpr
                            Concat
                            ( BinExpr
                                Sub
                                ( BinExpr
                                    Add
                                    (NameExpr "a")
                                    ( BinExpr
                                        Div
                                        ( BinExpr
                                            Mul
                                            (ApplyExpr (NameExpr "f") [(NameExpr "a")])
                                            (NameExpr "a")
                                        )
                                        (NameExpr "a")
                                    )
                                )
                                (NameExpr "a")
                            )
                            (NameExpr "a")
                        )
                    )
                ]
            )
        ),
      testCase
        "parsing patterns"
        ( shouldParse
            "x = case x of { _ -> 3; A (B y) y -> 3; y -> y }"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "x"
                        ( CaseExpr
                            (NameExpr "x")
                            [ PatternDef WildcardPattern (LittExpr (IntLitteral 3)),
                              PatternDef (ConstructorPattern "A" [ConstructorPattern "B" [NamePattern "y"], NamePattern "y"]) (LittExpr (IntLitteral 3)),
                              PatternDef (NamePattern "y") (NameExpr "y")
                            ]
                        )
                    )
                ]
            )
        ),
      testCase
        "parsing negation"
        (shouldParse "x = -3" (AST [ValueDefinition (NameDefinition "x" (NegateExpr (LittExpr (IntLitteral 3))))])),
      testCase
        "litteral patterns"
        ( shouldParse
            "x = case x of { \"foo\" -> x }"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "x"
                        (CaseExpr (NameExpr "x") [PatternDef (LitteralPattern (StringLitteral "foo")) (NameExpr "x")])
                    )
                ]
            )
        ),
      testCase
        "function types"
        ( shouldParse
            "x :: (Int -> String) -> A -> B"
            ( AST [ValueDefinition (TypeAnnotation "x" (FunctionType (FunctionType IntType StringType) (FunctionType (CustomType "A") (CustomType "B"))))]
            )
        ),
      testCase
        "type definitions"
        ( shouldParse
            "type X = Int;data L = A Int | B String (String -> String)"
            ( AST
                [ TypeSynonym "X" IntType,
                  TypeDefinition
                    "L"
                    [ ConstructorDefinition "A" [IntType],
                      ConstructorDefinition "B" [StringType, FunctionType StringType StringType]
                    ]
                ]
            )
        ),
      testCase
        "lambda expressions"
          (
            shouldParse "x = \\f a -> f"
              (AST [ValueDefinition (NameDefinition "x" (LambdaExpr ["f", "a"] (NameExpr "f")))])
          )
    ]
