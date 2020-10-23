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
            (AST [ValueDefinition (TypeAnnotation "x" IntType), ValueDefinition (NameDefinition "x" [] (LittExpr (IntLitteral 3)))])
        ),
      testCase
        "parsing let definitions"
        ( shouldParse
            "x = \n  let\n    y = 3\n  in y"
            (AST [ValueDefinition (NameDefinition "x" [] (LetExpr [NameDefinition "y" [] (LittExpr (IntLitteral 3))] (NameExpr "y")))])
        ),
      testCase
        "parsing where definitions"
        ( shouldParse
            "x = y where y = 3"
            (AST [ValueDefinition (NameDefinition "x" [] (WhereExpr (NameExpr "y") [NameDefinition "y" [] (LittExpr (IntLitteral 3))]))])
        ),
      testCase
        "parsing binary expressions"
        ( shouldParse
            "x = a + f a * a / a - a ++ a"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "x"
                        []
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
                        []
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
        (shouldParse "x = -3" (AST [ValueDefinition (NameDefinition "x" [] (NegateExpr (LittExpr (IntLitteral 3))))])),
      testCase
        "litteral patterns"
        ( shouldParse
            "x = case x of { \"foo\" -> x }"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "x"
                        []
                        (CaseExpr (NameExpr "x") [PatternDef (LitteralPattern (StringLitteral "foo")) (NameExpr "x")])
                    )
                ]
            )
        ),
      testCase
        "function types"
        ( shouldParse
            "x :: (Int -> String) -> A -> B"
            ( AST [ValueDefinition (TypeAnnotation "x" (FunctionType (FunctionType IntType StringType) (FunctionType (CustomType "A" []) (CustomType "B" []))))]
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
                    []
                    [ ConstructorDefinition "A" [IntType],
                      ConstructorDefinition "B" [StringType, FunctionType StringType StringType]
                    ]
                ]
            )
        ),
      testCase
        "lambda expressions"
        ( shouldParse
            "x = \\f a -> f"
            (AST [ValueDefinition (NameDefinition "x" [] (LambdaExpr ["f", "a"] (NameExpr "f")))])
        ),
      testCase
        "polymorphism"
        ( shouldParse
            "foo :: a -> List a; data List a = Cons a (List a) | Nil"
            ( AST
                [ ValueDefinition (TypeAnnotation "foo" (FunctionType (TypeVar "a") (CustomType "List" [TypeVar "a"]))),
                  TypeDefinition
                    "List"
                    ["a"]
                    [ ConstructorDefinition "Cons" [TypeVar "a", (CustomType "List" [TypeVar "a"])],
                      ConstructorDefinition "Nil" []
                    ]
                ]
            )
        ),
      testCase
        "boolean operators"
        ( shouldParse
            "x = x && x || x && x"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "x"
                        []
                        (BinExpr Or (BinExpr And (NameExpr "x") (NameExpr "x")) (BinExpr And (NameExpr "x") (NameExpr "x")))
                    )
                ]
            )
        ),
      testCase
        "=="
        ( shouldParse
            "x = x == x"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "x"
                        []
                        (BinExpr EqualTo (NameExpr "x") (NameExpr "x"))
                    )
                ]
            )
        ),
      testCase
        "/="
        ( shouldParse
            "x = x /= x"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "x"
                        []
                        (BinExpr NotEqualTo (NameExpr "x") (NameExpr "x"))
                    )
                ]
            )
        ),
      testCase
        ">="
        ( shouldParse
            "x = x >= x"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "x"
                        []
                        (BinExpr GreaterEqual (NameExpr "x") (NameExpr "x"))
                    )
                ]
            )
        ),
      testCase
        ">"
        ( shouldParse
            "x = x > x"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "x"
                        []
                        (BinExpr Greater (NameExpr "x") (NameExpr "x"))
                    )
                ]
            )
        ),
      testCase
        "<"
        ( shouldParse
            "x = x < x"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "x"
                        []
                        (BinExpr Less (NameExpr "x") (NameExpr "x"))
                    )
                ]
            )
        ),
      testCase
        "<="
        ( shouldParse
            "x = x <= x"
            ( AST
                [ ValueDefinition
                    ( NameDefinition
                        "x"
                        []
                        (BinExpr LessEqual (NameExpr "x") (NameExpr "x"))
                    )
                ]
            )
        ),
      testCase
        "$"
        ( shouldParse
            "x = f $ g $ x"
            (AST [ValueDefinition (NameDefinition "x" [] (BinExpr Cash (NameExpr "f") (BinExpr Cash (NameExpr "g") (NameExpr "x"))))])
        ),
      testCase
        "."
        ( shouldParse
            "x = f . g . x"
            (AST [ValueDefinition (NameDefinition "x" [] (BinExpr Compose (NameExpr "f") (BinExpr Compose (NameExpr "g") (NameExpr "x"))))])
        ),
      testCase
        "if then else"
        ( shouldParse
            "x = if x then x else if x then x else x"
            ( AST
                [ ValueDefinition
                    ( NameDefinition "x" [] (IfExpr (NameExpr "x") (NameExpr "x") (IfExpr (NameExpr "x") (NameExpr "x") (NameExpr "x")))
                    )
                ]
            )
        ),
      testCase
        "names with patterns"
        ( shouldParse
            "x _ (A x) = 3"
            ( AST
                [ ValueDefinition
                    ( NameDefinition "x" [WildcardPattern, ConstructorPattern "A" [NamePattern "x"]] (LittExpr (IntLitteral 3))
                    )
                ]
            )
        )
    ]
