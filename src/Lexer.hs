{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lexer (Token (..), lexer) where

import Control.Applicative
import Data.List (foldl1')
import Ourlude

newtype Lexer a = Lexer {runLexer :: String -> Maybe (a, String)}

instance Functor Lexer where
  fmap f (Lexer l) = Lexer (l >>> fmap (\(a, s) -> (f a, s)))

instance Applicative Lexer where
  pure a = Lexer (\input -> Just (a, input))
  Lexer lF <*> Lexer lA =
    Lexer <| \input -> case lF input of
      Nothing -> Nothing
      Just (f, rest) -> lA rest |> fmap (\(a, s) -> (f a, s))

instance Alternative Lexer where
  empty = Lexer (const Nothing)
  Lexer lA <|> Lexer lB =
    Lexer <| \input -> case (lA input, lB input) of
      (res, Nothing) -> res
      (Nothing, res) -> res
      -- Implement the longest match rule
      (a@(Just (_, restA)), b@(Just (_, restB))) ->
        if length restA <= length restB then a else b

satisfies :: (Char -> Bool) -> Lexer Char
satisfies p =
  Lexer <| \input -> case input of
    c : cs | p c -> Just (c, cs)
    _ -> Nothing

char :: Char -> Lexer Char
char target = satisfies (== target)

string :: String -> Lexer String
string = traverse char

-- Represents a kind of Token we can lex out.
--
-- The idea is that our lexer will convert a stream / list of
-- characters into a corresponding stream / list of tokens,
-- which correspond to individual portions of the stream.
data Token
  = Let -- `let`
  | Where -- `where`
  | In -- `in`
  | Data -- `data`
  | Type -- `type`
  | If -- `if`
  | Then -- `then`
  | Else -- `else`
  | Case -- `case`
  | Of -- `of`
  | Underscore -- `_`
  | OpenParens -- `(`
  | CloseParens -- `)`
  | OpenBrace -- `{`
  | CloseBrace -- `}`
  | Semicolon -- `;`
  | DoubleColon -- `::`
  | ThinArrow -- `->`
  | VBar -- `|`
  | BSlash -- `\`
  | FSlash -- `/`
  | Plus -- `+`
  | Dash -- `-`
  | Asterisk -- `*`
  | Equal -- `=`
  | Dot -- `.`
  | Dollar -- `$`
  | IntLitt Int -- An integer litteral
  | TypeName String -- A reference to some kind of type name
  | Name String -- A reference to some kind of name
  deriving (Eq, Show)

token :: Lexer Token
token =
  foldl1'
    (<|>)
    [ Let <$ string "let",
      Where <$ string "where",
      In <$ string "in",
      Data <$ string "data",
      Type <$ string "type",
      If <$ string "if",
      Then <$ string "then",
      Else <$ string "else",
      Case <$ string "case",
      Of <$ string "of"
    ]

lexer :: String -> Maybe [Token]
lexer = runLexer (many token) >>> fmap fst