{-# LANGUAGE LambdaCase #-}

module Simplifier
  ( Pattern (..),
    Litteral (..),
    TypeExpr (..),
    SchemeExpr (..),
    ConstructorDefinition (..),
    SimplifierError (..),
    AST (..),
    Expr (..),
    PatternDef (..),
    Definition (..),
    ValueDefinition (..),
    Name,
    TypeName,
    simplifier,
  )
where

import Data.Function (on)
import Data.List (foldl', groupBy)
import Data.Maybe (catMaybes)
import Ourlude
import Parser (ConstructorDefinition (..), Litteral (..), Pattern (..), TypeExpr (..))
import qualified Parser as Parser

type Name = String

type TypeName = String

newtype AST t = AST [Definition t] deriving (Eq, Show)

data Definition t
  = ValueDefinition (ValueDefinition t)
  | TypeDefinition TypeName [Name] [ConstructorDefinition]
  | TypeSynonym TypeName TypeExpr
  deriving (Eq, Show)

data ValueDefinition t = NameDefinition String (Maybe SchemeExpr) t (Expr t) deriving (Eq, Show)

data SchemeExpr = SchemeExpr [Name] TypeExpr deriving (Eq, Show)

closeTypeExpr :: TypeExpr -> SchemeExpr
closeTypeExpr t = SchemeExpr (names t) t
  where
    names StringType = []
    names IntType = []
    names BoolType = []
    names (CustomType _ typs) = typs >>= names
    names (TypeVar n) = [n]
    names (FunctionType t1 t2) = names t1 ++ names t2

data Expr t
  = LetExpr [ValueDefinition t] (Expr t)
  | CaseExpr (Expr t) [PatternDef t]
  | LittExpr Litteral
  | Builtin Builtin
  | NameExpr Name
  | ApplyExpr (Expr t) (Expr t)
  | LambdaExpr Name t (Expr t)
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data PatternDef t = PatternDef Pattern (Expr t) deriving (Eq, Show)

data SimplifierError
  = MultipleTypeAnnotations String [SchemeExpr]
  | DifferentPatternLengths String [Int]
  | UnimplementedAnnotation String
  deriving (Eq, Show)

convertExpr :: Parser.Expr -> Either SimplifierError (Expr ())
-- We replace binary expressions with the corresponding bultin functions
convertExpr (Parser.BinExpr op e1 e2) = do
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
  e1' <- convertExpr e1
  e2' <- convertExpr e2
  return (ApplyExpr (ApplyExpr (Builtin b) e1') e2')
-- Negation is replaced by a built in function as well
convertExpr (Parser.NegateExpr e) = ApplyExpr (Builtin Negate) <$> (convertExpr e)
convertExpr (Parser.WhereExpr e defs) =
  convertExpr (Parser.LetExpr defs e)
convertExpr (Parser.IfExpr cond thenn elsse) = do
  cond' <- convertExpr cond
  thenn' <- convertExpr thenn
  elsse' <- convertExpr elsse
  return
    ( CaseExpr
        cond'
        [ PatternDef (LitteralPattern (BoolLitteral True)) thenn',
          PatternDef (LitteralPattern (BoolLitteral False)) elsse'
        ]
    )
convertExpr (Parser.NameExpr name) = Right (NameExpr name)
convertExpr (Parser.LittExpr litt) = Right (LittExpr litt)
convertExpr (Parser.LambdaExpr names body) = do
  body' <- convertExpr body
  return (foldr (\x acc -> LambdaExpr x () acc) body' names)
convertExpr (Parser.ApplyExpr f exprs) = do
  f' <- convertExpr f
  exprs' <- traverse convertExpr exprs
  return (foldl' (\acc x -> ApplyExpr acc x) f' exprs')
convertExpr (Parser.CaseExpr expr patterns) = do
  let transformPat (Parser.PatternDef p e) = PatternDef p <$> (convertExpr e)
  patterns' <- traverse transformPat patterns
  expr' <- convertExpr expr
  return (CaseExpr expr' patterns')
convertExpr (Parser.NameExpr name) = Right (NameExpr name)
convertExpr (Parser.LittExpr litt) = Right (LittExpr litt)
convertExpr (Parser.LetExpr defs e) = do
  defs' <- convertValueDefinitions defs
  e' <- convertExpr e
  return (LetExpr defs' e')

data PatternTree = Leaf (Expr ()) | Branch [(Pattern, PatternTree)] | Empty deriving (Eq, Show)

-- Calculate the depth of a given tree of patterns
--
-- This is useful to know the number of lambda arguments we might need
treeDepth :: PatternTree -> Int
treeDepth (Leaf _) = 0
treeDepth (Branch bs) = map (snd >>> treeDepth) bs |> maximum |> (+ 1)

-- True if a given pattern is equivalent, or captures more values than another
covers :: Pattern -> Pattern -> Bool
covers WildcardPattern WildcardPattern = True
covers WildcardPattern (NamePattern _) = True
covers WildcardPattern _ = True
covers (NamePattern _) (NamePattern _) = True
covers (NamePattern _) WildcardPattern = True
covers (NamePattern _) _ = True
covers (LitteralPattern l1) (LitteralPattern l2) | l1 == l2 = True
covers (ConstructorPattern c1 pats1) (ConstructorPattern c2 pats2) =
  c1 == c2 && length pats1 == length pats2 && (all (uncurry covers) (zip pats1 pats2))
covers _ _ = False

-- This adds a full block pattern to the pattern tree
addBranches :: ([Pattern], Expr ()) -> PatternTree -> PatternTree
addBranches _ (Leaf expr) = Leaf expr
addBranches ([], expr) Empty = Leaf expr
addBranches (p : ps, expr) Empty = Branch [(p, addBranches (ps, expr) Empty)]
addBranches (p : ps, expr) (Branch bs) =
  -- We trickle down the case generation when we cover that pattern,
  -- but we only need to create a new branch if no pattern completely covers us
  let trickle (pat, tree) =
        if covers p pat
          then (pat, addBranches (ps, expr) tree)
          else (pat, tree)
      trickled = map trickle bs
      extra = (p, addBranches (ps, expr) Empty)
      bs' =
        if any (\(pat, _) -> covers pat p) bs
          then trickled
          else extra : trickled
   in Branch bs'

lambdaNames :: [Name]
lambdaNames = map (\x -> "$" ++ show x) [(0 :: Integer) ..]

-- Convert a pattern tree into a correct expression.
--
-- This creates a lambda expression, and then creates the necessary
-- nested cases.
convertTree :: PatternTree -> Expr ()
convertTree tree =
  foldr
    (\x acc -> LambdaExpr x () acc)
    (fold lambdaNames tree)
    (take (treeDepth tree) lambdaNames)
  where
    fold :: [Name] -> PatternTree -> Expr ()
    fold _ (Leaf expr) = expr
    fold (n : ns) (Branch bs) =
      let makeCase (pat, tree') = PatternDef pat (fold ns tree')
       in CaseExpr (NameExpr n) (map makeCase (reverse bs))

-- This converts value definitions by gathering the different patterns into a single lambda expression,
-- and adding the optional type annotation if it exists.
-- This will emit errors if any discrepencies are encountered.
convertValueDefinitions :: [Parser.ValueDefinition] -> Either SimplifierError [ValueDefinition ()]
convertValueDefinitions = groupBy ((==) `on` getName) >>> traverse gather
  where
    getTypeAnnotations ls =
      (catMaybes <<< (`map` ls)) <| \case
        Parser.TypeAnnotation _ typ -> Just typ
        _ -> Nothing
    squashPatterns :: [Parser.ValueDefinition] -> Either SimplifierError [([Pattern], Expr ())]
    squashPatterns ls = do
      let strip (Parser.NameDefinition _ pats body) = do
            body' <- convertExpr body
            return (Just (pats, body'))
          strip _ = return Nothing
      stripped <- traverse strip ls
      return (catMaybes stripped)
    getName :: Parser.ValueDefinition -> Name
    getName (Parser.TypeAnnotation name _) = name
    getName (Parser.NameDefinition name _ _) = name
    gather :: [Parser.ValueDefinition] -> Either SimplifierError (ValueDefinition ())
    gather [] = error "groupBy returned empty list"
    gather information = do
      let name = getName (head information)
          annotations = getTypeAnnotations information
      schemeExpr <- case map closeTypeExpr annotations of
        [] -> Right Nothing
        [single] -> Right (Just single)
        tooMany -> Left (MultipleTypeAnnotations name tooMany)
      pats <- squashPatterns information
      let patLengths = map (fst >>> length) pats
      case patLengths of
        [] -> Left (UnimplementedAnnotation name)
        (l : ls) | all (== l) ls -> Right ()
        ls -> Left (DifferentPatternLengths name ls)
      let tree = foldl' (\acc x -> addBranches x acc) Empty pats
          expr = convertTree tree
      return (NameDefinition name schemeExpr () expr)

convertDefinitions :: [Parser.Definition] -> Either SimplifierError [Definition ()]
convertDefinitions defs =
  let split = foldr go ([], [])
        where
          go (Parser.TypeDefinition name args cstr) (ts, vs) =
            (TypeDefinition name args cstr : ts, vs)
          go (Parser.TypeSynonym name typ) (ts, vs) = (TypeSynonym name typ : ts, vs)
          go (Parser.ValueDefinition v) (ts, vs) = (ts, v : vs)
      (typedefs, valuedefs) = split defs
   in do
        valuedefs' <- convertValueDefinitions valuedefs
        return (typedefs ++ map ValueDefinition valuedefs')

simplifier :: Parser.AST -> Either SimplifierError (AST ())
simplifier (Parser.AST defs) = AST <$> (convertDefinitions defs)
