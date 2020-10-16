module LexerTest (tests) where

import Lexer (Token (..), lexer)
import Ourlude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Lexer Tests"
    [ testCase "lexing keywords" (Just [StartOfFile, OpenBrace, In, Data, Type, If, Then, Else, Case, CloseBrace] @=? lexer "in data type if then else case"),
      testCase "lexing operators" (Just operators @=? lexer "() {} ; :: -> | \\ / + - * = . $"),
      testCase "lexing names" (Just [StartOfFile, OpenBrace, Name "foo32'", TypeName "A34'", CloseBrace] @=? lexer "foo32' A34'"),
      testCase "lexing integer litterals" (Just [StartOfFile, OpenBrace, Dash, IntLitt 42, IntLitt 32, CloseBrace] @=? lexer "-42 32")
    ]
  where
    operators =
      [ StartOfFile,
        OpenBrace,
        OpenParens,
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
        Dollar,
        CloseBrace
      ]
