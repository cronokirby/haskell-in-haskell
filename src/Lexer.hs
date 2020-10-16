{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lexer (Token (..), lexer) where

import Control.Applicative (Alternative (..), liftA2)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
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

whitespace :: Lexer String
whitespace = some (satisfies isSpace)

-- A raw token is either a "real" token, or some whitespace that we actually want to ignore
data RawToken = Whitespace String | RawToken Token

lexer :: String -> Maybe [Token]
lexer = runLexer (some item) >>> fmap (fst >>> filterTokens)
  where
    item = fmap RawToken token <|> fmap Whitespace whitespace
    filterTokens =
      let go (RawToken x) acc = x : acc
          go (Whitespace _) acc = acc
       in foldr go []
