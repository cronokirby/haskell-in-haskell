{-# LANGUAGE LambdaCase #-}

module Parser
  ( AST (..),
    ValName,
    ConstructorName,
    Name,
    Definition (..),
    ConstructorDefinition (..),
    ValueDefinition (..),
    Expr (..),
    Literal (..),
    BinOp (..),
    Pattern (..),
    parser,
  )
where

import Control.Applicative (Alternative (..), liftA2)
import Data.List (foldl')
import Lexer (Token (..))
import Ourlude
import Types (Type (..), TypeName, TypeVar)

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

newtype AST = AST [Definition] deriving (Eq, Show)

data Definition
  = ValueDefinition ValueDefinition
  | DataDefinition TypeName [TypeVar] [ConstructorDefinition]
  | TypeSynonym TypeName Type
  deriving (Eq, Show)

data ValueDefinition
  = TypeAnnotation ValName Type
  | NameDefinition ValName [Pattern] Expr
  deriving (Eq, Show)

type ConstructorName = String

data ConstructorDefinition
  = ConstructorDefinition ConstructorName [Type]
  deriving (Eq, Show)

data Literal
  = IntLiteral Int
  | StringLiteral String
  | BoolLiteral Bool
  deriving (Eq, Ord, Show)

type Name = String

type ValName = String

data Expr
  = LitExpr Literal
  | NameExpr Name
  | IfExpr Expr Expr Expr
  | LambdaExpr [ValName] Expr
  | ApplyExpr Expr [Expr]
  | NegateExpr Expr
  | BinExpr BinOp Expr Expr
  | CaseExpr Expr [(Pattern, Expr)]
  | LetExpr [ValueDefinition] Expr
  | WhereExpr Expr [ValueDefinition]
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | EqualTo
  | NotEqualTo
  | Or
  | And
  | Cash
  | Compose
  deriving (Eq, Show)

data Pattern
  = WildcardPattern
  | NamePattern ValName
  | LiteralPattern Literal
  | ConstructorPattern ConstructorName [Pattern]
  deriving (Eq, Show)

parser :: [Token] -> Either ParseError AST
parser _ = Left UnimplementedError
