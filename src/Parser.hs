module Parser where

import Ourlude

newtype AST = AST [Definition]

data Definition
  = TypeDefinition String TypeExpr
  | Definition String Expr

data TypeExpr
  = StringType
  | IntType
  | CustomType String

data Expr
  = BinExpr BinOp Expr Expr
  | Let [Definition] Expr
  | Where Expr [Definition]
  | Apply Expr Expr
  | Case Expr [PatternDef]

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Concat

data PatternDef = PatternDef Pattern Expr

data Pattern = Wildcard | VarPattern String | ConstructorPattern String [Pattern]
