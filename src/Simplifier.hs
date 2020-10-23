module Simplifier (Parser.BinOp (..), Parser.Pattern (..), Parser.Litteral (..)) where

import Ourlude
import qualified Parser as Parser

type Name = String

type TypeName = String

type BinOp = Parser.BinOp

data Definition
  = ValueDefinition ValueDefinition
  | TypeDefinition TypeName [Name] [ConstructorDefinition]

data ValueDefinition = NameDefinition String (Maybe SchemeExpr) Expr

data ConstructorDefinition = ConstructorDefinition TypeName [TypeExpr]

data TypeExpr
  = StringType
  | IntType
  | BoolType
  | CustomType TypeName [TypeExpr]
  | TypeVar Name
  | FunctionType TypeExpr TypeExpr

data SchemeExpr = SchemeExpr [Name] TypeExpr

data Expr
  = LetExpr [ValueDefinition] Expr
  | CaseExpr Expr [PatternDef]
  | LittExpr Litteral
  | Builtin Builtin
  | NameExpr Name
  | ApplyExpr Expr Expr
  | LambdaExpr Name Expr

data Builtin
  = Add
  | Sub
  | Mul
  | Div
  | Compose
  | Concat
  | Cash
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | EqualTo
  | NotEqualTo
  | And
  | Or
  | Negate

type Pattern = Parser.Pattern

type Litteral = Parser.Litteral

data PatternDef = PatternDef Pattern Expr
