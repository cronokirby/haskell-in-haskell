module LexerTest (tests) where

import Lexer (Token (..), lexer)
import Ourlude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Lexer Tests"
    [ testCase "parsing keywords" (Just [Let, Case, Of, If, Then, Else] @=? lexer "let case \n of if then else"),
      testCase "parsing operators" (Just [ThinArrow, Dash] @=? lexer "-> -")
    ]
