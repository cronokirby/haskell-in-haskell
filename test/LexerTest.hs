module LexerTest (tests) where

import Lexer (Token (..), lexer)
import Ourlude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Lexer Tests"
    [ testCase "parsing keywords" (lexer "letcaseifthenelse" @=? Just [Let, Case, If, Then, Else])
    ]
