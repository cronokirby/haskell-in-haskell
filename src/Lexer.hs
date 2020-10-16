{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lexer (Token (..), lexer) where

import Control.Applicative (Alternative (..), liftA2)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List (foldl1')
import Ourlude

-- A Lexer takes an input string, and can consume part of that string to return a result, or fail
--
-- Lexers are like parser combinators, except that they cannot do conditional decision making,
-- or return multiple results. They always return the result that consumed more input,
-- which corresponds to the "longest match" rule you want in a lexical analyzer
newtype Lexer a = Lexer {runLexer :: String -> Maybe (a, String)}

-- We can map over the result of a lexer, without changing what strings are recognized
instance Functor Lexer where
  fmap f (Lexer l) = Lexer (l >>> fmap (\(a, s) -> (f a, s)))

-- We can squash two lexers together, getting a lexer that recognizes the first input,
-- followed by the second input
instance Applicative Lexer where
  pure a = Lexer (\input -> Just (a, input))
  Lexer lF <*> Lexer lA =
    Lexer <| \input -> case lF input of
      Nothing -> Nothing
      Just (f, rest) -> lA rest |> fmap (\(a, s) -> (f a, s))

-- We can choose between two successful lexes by picking the one that consumed more input
instance Alternative Lexer where
  empty = Lexer (const Nothing)
  Lexer lA <|> Lexer lB =
    Lexer <| \input -> case (lA input, lB input) of
      (res, Nothing) -> res
      (Nothing, res) -> res
      -- Implement the longest match rule
      (a@(Just (_, restA)), b@(Just (_, restB))) ->
        if length restA <= length restB then a else b

-- A lexer that matches a single character matching a predicate
satisfies :: (Char -> Bool) -> Lexer Char
satisfies p =
  Lexer <| \input -> case input of
    c : cs | p c -> Just (c, cs)
    _ -> Nothing

-- A lexer that matches a single character
char :: Char -> Lexer Char
char target = satisfies (== target)

-- A lexer that matches an entire string
string :: String -> Lexer String
string = traverse char

-- Create an alternation of a list of lexers.
--
-- This will match if any of the lexers matches, picking the longest match, as usual.
oneOf :: [Lexer a] -> Lexer a
oneOf = foldl1' (<|>)

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

-- Lex out one of the tokens in our language
token :: Lexer Token
token = keywords <|> operators <|> intLitt <|> typeName <|> name
  where
    keywords :: Lexer Token
    keywords =
      oneOf
        [ Let <$ string "let",
          Where <$ string "where",
          In <$ string "in",
          Data <$ string "data",
          Type <$ string "type",
          If <$ string "if",
          Then <$ string "then",
          Else <$ string "else",
          Case <$ string "case",
          Of <$ string "of",
          Underscore <$ string "_"
        ]
    operators :: Lexer Token
    operators =
      oneOf
        [ OpenParens <$ char '(',
          CloseParens <$ char ')',
          OpenBrace <$ char '{',
          CloseBrace <$ char '}',
          Semicolon <$ char ';',
          DoubleColon <$ string "::",
          ThinArrow <$ string "->",
          VBar <$ char '|',
          BSlash <$ char '\\',
          FSlash <$ char '/',
          Plus <$ char '+',
          Dash <$ char '-',
          Asterisk <$ char '*',
          Equal <$ char '=',
          Dot <$ char '.',
          Dollar <$ char '$'
        ]
    intLitt :: Lexer Token
    intLitt = some (satisfies isDigit) |> fmap (read >>> IntLitt)
    continuesName :: Lexer Char
    continuesName = satisfies isAlphaNum <|> char '\''
    typeName :: Lexer Token
    typeName = (liftA2 (:) (satisfies isUpper) (many continuesName)) |> fmap TypeName
    name :: Lexer Token
    name = (liftA2 (:) (satisfies isLower) (many continuesName)) |> fmap Name

-- A lexer that matches as much whitespace as ossible
whitespace :: Lexer String
whitespace = some (satisfies isSpace)

-- A raw token is either a "real" token, or some whitespace that we actually want to ignore
data RawToken = Whitespace String | RawToken Token

-- Lex a specific string, producing a list of tokens if no errors occurred.
lexer :: String -> Maybe [Token]
lexer = runLexer (some item) >>> fmap (fst >>> filterTokens)
  where
    item = fmap RawToken token <|> fmap Whitespace whitespace
    filterTokens =
      let go (RawToken x) acc = x : acc
          go (Whitespace _) acc = acc
       in foldr go []
