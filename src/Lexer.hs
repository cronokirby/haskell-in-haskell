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
  | StartOfFile
  | IntLitt Int -- An integer litteral
  | TypeName String -- A reference to some kind of type name
  | Name String -- A reference to some kind of name
  deriving (Eq, Show)

-- Lex out one of the tokens in our language
token :: Lexer (Token, String)
token = keywords <|> operators <|> intLitt <|> typeName <|> name
  where
    with :: Functor f => b -> f a -> f (b, a)
    with b = fmap (\a -> (b, a))
    keywords :: Lexer (Token, String)
    keywords =
      oneOf
        [ Let `with` string "let",
          Where `with` string "where",
          In `with` string "in",
          Data `with` string "data",
          Type `with` string "type",
          If `with` string "if",
          Then `with` string "then",
          Else `with` string "else",
          Case `with` string "case",
          Of `with` string "of",
          Underscore `with` string "_"
        ]
    operators :: Lexer (Token, String)
    operators =
      oneOf
        [ OpenParens `with` string "(",
          CloseParens `with` string ")",
          OpenBrace `with` string "{",
          CloseBrace `with` string "}",
          Semicolon `with` string ";",
          DoubleColon `with` string "::",
          ThinArrow `with` string "->",
          VBar `with` string "|",
          BSlash `with` string "\\",
          FSlash `with` string "/",
          Plus `with` string "+",
          Dash `with` string "-",
          Asterisk `with` string "*",
          Equal `with` string "=",
          Dot `with` string ".",
          Dollar `with` string "$"
        ]
    intLitt :: Lexer (Token, String)
    intLitt = some (satisfies isDigit) |> fmap (\x -> (IntLitt (read x), x))
    continuesName :: Lexer Char
    continuesName = satisfies isAlphaNum <|> char '\''
    typeName :: Lexer (Token, String)
    typeName = (liftA2 (:) (satisfies isUpper) (many continuesName)) |> fmap (\x -> (TypeName x, x))
    name :: Lexer (Token, String)
    name = (liftA2 (:) (satisfies isLower) (many continuesName)) |> fmap (\x -> (Name x, x))

-- A raw token is either a "real" token, or some whitespace that we actually want to ignore
data RawToken = Blankspace String | Newline | RawToken Token String

rawLexer :: Lexer [RawToken]
rawLexer = fmap (RawToken StartOfFile "" :) (some item)
  where
    item = whitespace <|> fmap (uncurry RawToken) token
    whitespace = blankspace <|> newline
    blankspace = Blankspace <$> some (satisfies (\x -> isSpace x && x /= '\n'))
    newline = Newline <$ char '\n'

data LinePosition = Start | Middle

-- Some type annotated with a position
data Positioned a = Positioned a LinePosition Int

-- Take tokens and whitespace, and return positioned tokens, with whitespace filtered out
position :: [RawToken] -> [Positioned Token]
position = foldr go ((Start, 0), []) >>> snd
  where
    eat :: (LinePosition, Int) -> RawToken -> ((LinePosition, Int), Maybe (Positioned Token))
    eat _ Newline = ((Start, 0), Nothing)
    eat (pos, col) (Blankspace s) = ((pos, col + length s), Nothing)
    eat (pos, col) (RawToken t s) = ((Middle, col + length s), Just (Positioned t pos col))
    go :: RawToken -> ((LinePosition, Int), [Positioned Token]) -> ((LinePosition, Int), [Positioned Token])
    go raw (p, acc) = case eat p raw of
      (p', Just tok) -> (p', tok : acc)
      (p', Nothing) -> (p', acc)

-- A layout is either one explicitly declared by the user, or implicitly declared at a certain column
data Layout = Explicit | Implicit Int

layout :: [Positioned Token] -> Maybe [Token]
layout tokens = go tokens []
  where
    startsLayout :: Token -> Bool
    startsLayout t = elem t [Let, Where, Of, StartOfFile]
    go :: [Positioned Token] -> [Layout] -> Maybe [Token]
    -- An explicit } must close a corresponding explicit layout
    go (Positioned CloseBrace _ _ : ts) (Explicit : ls) = fmap (CloseBrace :) (go ts ls)
    go (Positioned CloseBrace _ _ : _) _ = Nothing
    -- An explicit { starts an explicit layout
    go (Positioned OpenBrace _ _ : ts) ls = fmap (OpenBrace :) (go ts (Explicit : ls))
    -- If we see a token that starts a layout, three things can happen
    go (Positioned starter _ _ : tok@(Positioned t _ n) : ts) ls' | startsLayout starter = case ls' of
      -- If no layouts exist yet, then we can start at any indentation
      [] -> fmap ([starter, OpenBrace, t] ++) (go ts [Implicit n])
      -- Otherwise we need to be further indented to start a new layout
      Implicit m : ls | n > m -> fmap ([starter, OpenBrace, t] ++) (go ts (Implicit n : Implicit m : ls))
      -- If we're less indented, that means that we've skipped a layout
      ls -> fmap ([starter, OpenBrace, CloseBrace] ++) (go (tok : ts) ls)
    -- If a starting token is at the same level of implicit indentation, that continues the layout
    go (Positioned t Start n : ts) (Implicit m : ls) | n == m = fmap ([Semicolon, t] ++) (go ts (Implicit m : ls))
    -- If a starting token has less than the implicit indentation, then close that layout
    go (tok@(Positioned _ Start n) : ts) (Implicit m : ls) | n < m = fmap (CloseBrace :) (go (tok : ts) ls)
    -- If nothing else applies, we just want to emit the tokens we see
    go (Positioned t _ _ : ts) ls = fmap (t :) (go ts ls)
    -- Close all of the implicit layouts
    go [] (Implicit _ : ls) = fmap (OpenBrace :) (go [] ls)
    -- Any remaining explicit layouts are unclosed, and an error
    go [] (Explicit : _) = Nothing
    go [] [] = Just []

-- Lex a specific string, producing a list of tokens if no errors occurred.
lexer :: String -> Maybe [Token]
lexer input = do
  (raw, _) <- runLexer rawLexer input
  raw |> position |> layout
