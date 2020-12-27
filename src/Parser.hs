{-# LANGUAGE LambdaCase #-}

module Parser (AST (..), parser) where

import Control.Applicative (Alternative (..), liftA2)
import Data.List (foldl')
import Lexer (Token (..))
import Ourlude

newtype Parser a = Parser {runParser :: [Token] -> [(a, [Token])]}

instance Functor Parser where
  fmap f (Parser p) = Parser (p >>> fmap (first f))

instance Applicative Parser where
  pure a = Parser (\input -> [(a, input)])
  Parser pF <*> Parser pA =
    Parser <| \input -> do
      (f, rest) <- pF input
      (a, s) <- pA rest
      return (f a, s)

instance Alternative Parser where
  empty = Parser (const [])
  Parser pA <|> Parser pB =
    Parser <| \input -> pA input ++ pB input

satisfies :: (Token -> Bool) -> Parser Token
satisfies p =
  Parser <| \case
    t : ts | p t -> [(t, ts)]
    _ -> []

token :: Token -> Parser Token
token = (==) >>> satisfies

pluck :: (Token -> Maybe a) -> Parser a
pluck f =
  Parser <| \case
    t : ts -> case f t of
      Just res -> [(res, ts)]
      _ -> []
    _ -> []

data ParseError = UnimplementedError deriving (Show)

data AST = AST deriving (Show)

parser :: [Token] -> Either ParseError AST
parser _ = Left UnimplementedError
