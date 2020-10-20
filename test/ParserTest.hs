module ParserTest (tests) where

import Lexer (lexer)
import Ourlude
import Parser (AST (..), BinOp (..), Definition (..), Expr (..), Litteral (..), Pattern (..), PatternDef (..), TypeExpr (..), parse)
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
            (AST [TypeDefinition "x" IntType, Definition "x" (LittExpr (IntLitteral 3))])
        ),
      testCase
        "parsing let definitions"
        ( shouldParse
            "x = \n  let\n    y = 3\n  in y"
            (AST [Definition "x" (LetExpr [Definition "y" (LittExpr (IntLitteral 3))] (NameExpr "y"))])
        ),
      testCase
        "parsing where definitions"
        ( shouldParse
            "x = y where y = 3"
            (AST [Definition "x" (WhereExpr (NameExpr "y") [Definition "y" (LittExpr (IntLitteral 3))])])
        ),
      testCase
        "parsing binary expressions"
        ( shouldParse
            "x = a + f a * a / a - a ++ a"
            ( AST
                [ Definition
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
                ]
            )
        ),
      testCase
        "parsing patterns"
        ( shouldParse
            "x = case x of { _ -> 3; A (B y) y -> 3; y -> y }"
            ( AST
                [ Definition
                    "x"
                    ( CaseExpr
                        (NameExpr "x")
                        [ PatternDef WildcardPattern (LittExpr (IntLitteral 3)),
                          PatternDef (ConstructorPattern "A" [ConstructorPattern "B" [NamePattern "y"], NamePattern "y"]) (LittExpr (IntLitteral 3)),
                          PatternDef (NamePattern "y") (NameExpr "y")
                        ]
                    )
                ]
            )
        ),
      testCase
        "parsing negation"
        (shouldParse "x = -3" (AST [Definition "x" (NegateExpr (LittExpr (IntLitteral 3)))])),
      testCase
        "litteral patterns"
        ( shouldParse
            "x = case x of { \"foo\" -> x }"
            ( AST
                [ Definition
                    "x"
                    (CaseExpr (NameExpr "x") [PatternDef (LitteralPattern (StringLitteral "foo")) (NameExpr "x")])
                ]
            )
        ),
      testCase
        "function types"
        ( shouldParse
            "x :: (Int -> String) -> A -> B"
            ( AST [TypeDefinition "x" (FunctionType (FunctionType IntType StringType) (FunctionType (CustomType "A") (CustomType "B")))]
            )
        )
    ]
