{-# LANGUAGE LambdaCase #-}

module Simplifier
  ( Pattern (..),
    Litteral (..),
    TypeExpr (..),
  )
where

import Data.Function (on)
import Data.List (foldl', foldr1, groupBy)
import Data.Maybe (catMaybes)
import Ourlude
import Parser (Litteral (..), Pattern (..), TypeExpr (..))
import qualified Parser as Parser

type Name = String

type TypeName = String

type BinOp = Parser.BinOp

data Definition
  = ValueDefinition ValueDefinition
  | TypeDefinition TypeName [Name] [ConstructorDefinition]
  | TypeSynonym TypeName TypeExpr

data ValueDefinition = NameDefinition String (Maybe SchemeExpr) Expr

data ConstructorDefinition = ConstructorDefinition TypeName [TypeExpr]

data SchemeExpr = SchemeExpr [Name] TypeExpr

closeTypeExpr :: TypeExpr -> SchemeExpr
closeTypeExpr t = SchemeExpr (names t) t
  where
    names StringType = []
    names IntType = []
    names BoolType = []
    names (CustomType _ typs) = typs >>= names
    names (TypeVar n) = [n]
    names (FunctionType t1 t2) = names t1 ++ names t2

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

data PatternDef = PatternDef Pattern Expr

convertExpr :: Parser.Expr -> Expr
-- We replace binary expressions with the corresponding bultin functions
convertExpr (Parser.BinExpr op e1 e2) =
  let b = case op of
        Parser.Add -> Add
        Parser.Sub -> Sub
        Parser.Mul -> Mul
        Parser.Div -> Div
        Parser.Compose -> Compose
        Parser.Concat -> Concat
        Parser.Cash -> Cash
        Parser.Less -> Less
        Parser.LessEqual -> LessEqual
        Parser.Greater -> Greater
        Parser.GreaterEqual -> GreaterEqual
        Parser.EqualTo -> EqualTo
        Parser.NotEqualTo -> NotEqualTo
        Parser.And -> And
        Parser.Or -> Or
   in ApplyExpr (ApplyExpr (Builtin b) (convertExpr e1)) (convertExpr e2)
-- Negation is replaced by a built in function as well
convertExpr (Parser.NegateExpr e) = ApplyExpr (Builtin Negate) (convertExpr e)
convertExpr (Parser.WhereExpr e defs) =
  convertExpr (Parser.LetExpr defs e)
convertExpr (Parser.IfExpr cond thenn elsse) =
  CaseExpr
    (convertExpr cond)
    [ PatternDef (LitteralPattern (BoolLitteral True)) (convertExpr thenn),
      PatternDef (LitteralPattern (BoolLitteral False)) (convertExpr elsse)
    ]
convertExpr (Parser.NameExpr name) = NameExpr name
convertExpr (Parser.LittExpr litt) = LittExpr litt
convertExpr (Parser.LambdaExpr names body) =
  foldr LambdaExpr (convertExpr body) names
convertExpr (Parser.ApplyExpr f exprs) =
  foldl' (\acc x -> ApplyExpr acc (convertExpr x)) (convertExpr f) exprs
convertExpr (Parser.CaseExpr expr patterns) =
  let patterns' = map transformPat patterns
      transformPat (Parser.PatternDef p e) = PatternDef p (convertExpr e)
   in CaseExpr (convertExpr expr) patterns'
convertExpr (Parser.NameExpr name) = NameExpr name
convertExpr (Parser.LittExpr litt) = LittExpr litt

data SimplifierError
  = MultipleTypeAnnotations String [SchemeExpr]
  | DifferentPatternLengths String [Int]
  | UnimplementedAnnotation String

data PatternTree = Leaf Expr | Branch [(Pattern, PatternTree)] | Empty

-- Calculate the depth of a given tree of patterns
--
-- This is useful to know the number of lambda arguments we might need
treeDepth :: PatternTree -> Int
treeDepth (Leaf _) = 0
treeDepth (Branch bs) = map (snd >>> treeDepth) bs |> maximum

-- True if a given pattern completely encompases another
subsumes :: Pattern -> Pattern -> Bool
subsumes WildcardPattern WildcardPattern = True
subsumes WildcardPattern (NamePattern _) = True
subsumes WildcardPattern _ = True
subsumes (NamePattern _) (NamePattern _) = True
subsumes (NamePattern _) WildcardPattern = True
subsumes (NamePattern _) _ = True
subsumes (LitteralPattern l1) (LitteralPattern l2) | l1 == l2 = True
subsumes (ConstructorPattern c1 pats1) (ConstructorPattern c2 pats2) =
  c1 == c2 && length pats1 == length pats2 && (all (uncurry subsumes) (zip pats1 pats2))
subsumes _ _ = False

addBranches :: ([Pattern], Expr) -> PatternTree -> PatternTree
addBranches ([], expr) Empty = Leaf expr
addBranches (p : ps, expr) Empty = Branch [(p, addBranches (ps, expr) Empty)]
addBranches (p : ps, expr) (Branch bs) =
  if any (\(pat, _) -> subsumes pat p) bs
    then Branch bs
    else Branch ((p, addBranches (ps, expr) Empty) : bs)

convertValueDefinition :: [Parser.ValueDefinition] -> Either SimplifierError [ValueDefinition]
convertValueDefinition = groupBy ((==) `on` getName) >>> traverse gather
  where
    getTypeAnnotations ls =
      (catMaybes <<< (`map` ls)) <| \case
        Parser.TypeAnnotation _ typ -> Just typ
        _ -> Nothing
    squashPatterns :: [Parser.ValueDefinition] -> [([Pattern], Expr)]
    squashPatterns ls =
      (catMaybes <<< (`map` ls)) <| \case
        Parser.NameDefinition _ pats body -> Just (pats, convertExpr body)
        _ -> Nothing
    getName :: Parser.ValueDefinition -> Name
    getName (Parser.TypeAnnotation name _) = name
    getName (Parser.NameDefinition name _ _) = name
    gather :: [Parser.ValueDefinition] -> Either SimplifierError ValueDefinition
    gather [] = error "groupBy returned empty list"
    gather information = do
      let name = getName (head information)
          annotations = getTypeAnnotations information
      schemeExpr <- case map closeTypeExpr annotations of
        [] -> Right Nothing
        [single] -> Right (Just single)
        tooMany -> Left (MultipleTypeAnnotations name tooMany)
      let pats = squashPatterns information
          patLengths = map (fst >>> length) pats
      patLength <- case patLengths of
        [] -> Left (UnimplementedAnnotation name)
        (l : ls) | all (== l) ls -> Right l
        ls -> Left (DifferentPatternLengths name ls)
      return undefined
