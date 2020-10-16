module LexerTest (tests) where

import Lexer (Token (..), lexer)
import Ourlude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Lexer Tests"
    [ testCase "lexing keywords" (Just [Let, Where, In, Data, Type, If, Then, Else, Case, Of] @=? lexer "let where in data type if then else case of"),
      testCase "lexing operators" (Just operators @=? lexer "() {} ; :: -> | \\ / + - * = . $"),
      testCase "lexing names" (Just [Name "foo32'", TypeName "A34'"] @=? lexer "foo32' A34'"),
      testCase "lexing integer litterals" (Just [Dash, IntLitt 42, IntLitt 32] @=? lexer "-42 32")
    ]
  where
    operators =
      [ OpenParens,
        CloseParens,
        OpenBrace,
        CloseBrace,
        Semicolon,
        DoubleColon,
        ThinArrow,
        VBar,
        BSlash,
        FSlash,
        Plus,
        Dash,
        Asterisk,
        Equal,
        Dot,
        Dollar
      ]
