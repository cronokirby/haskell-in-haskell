module LexerTest (tests) where

import Lexer (Token (..), lexer)
import Ourlude
import Test.Tasty
import Test.Tasty.HUnit

shouldLex :: String -> [Token] -> Assertion
shouldLex str as = Right as @=? lexer str

tests :: TestTree
tests =
  testGroup
    "Lexer Tests"
    [ testCase "lexing keywords" (shouldLex "in data type if then else case" [OpenBrace, In, Data, Type, If, Then, Else, Case, CloseBrace]),
      testCase "lexing operators" (shouldLex "() ; :: -> | \\ / + - * = . $" operators),
      testCase "lexing names" (shouldLex "foo32' A34'" [OpenBrace, Name "foo32'", TypeName "A34'", CloseBrace]),
      testCase "lexing integer litterals" (shouldLex "-42 32" [OpenBrace, Dash, IntLitt 42, IntLitt 32, CloseBrace])
    ]
  where
    operators =
      [ OpenBrace,
        OpenParens,
        CloseParens,
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
