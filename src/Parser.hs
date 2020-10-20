{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative (..), liftA2)
import Data.List (foldl')
import Lexer (Token (..))
import Ourlude

newtype Parser a = Parser ([Token] -> [(a, [Token])])

instance Functor Parser where
  fmap f (Parser p) = Parser (p >>> fmap (\(a, s) -> (f a, s)))

instance Applicative Parser where
  pure a = Parser (\input -> [(a, input)])
  Parser lF <*> Parser lA =
    Parser <| \input -> do
      (f, rest) <- lF input
      (a, s) <- lA rest
      return (f a, s)

instance Alternative Parser where
  empty = Parser (const [])
  Parser lA <|> Parser lB =
    Parser <| \input -> lA input ++ lB input

satisifies :: (Token -> Bool) -> Parser Token
satisifies p =
  Parser <| \case
    t : ts | p t -> [(t, ts)]
    _ -> []

pluck :: (Token -> Maybe a) -> Parser a
pluck f =
  Parser <| \case
    t : ts -> case f t of
      Just res -> [(res, ts)]
      _ -> []
    _ -> []

token :: Token -> Parser Token
token = (==) >>> satisifies

data Associativity = LeftAssociative | RightAssociative

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = pure [] <|> liftA2 (:) p (many (sep *> p))

ops :: Associativity -> (sep -> a -> a -> a) -> Parser a -> Parser sep -> Parser a
ops assoc combine p s = liftA2 squash p (many (liftA2 (,) s p))
  where
    squash = case assoc of
      RightAssociative -> foldr (\(sep, a) acc -> combine sep a acc)
      LeftAssociative -> foldl' (\acc (sep, a) -> combine sep acc a)

name :: Parser String
name =
  pluck <| \case
    Name n -> Just n
    _ -> Nothing

typeName :: Parser String
typeName =
  pluck <| \case
    TypeName n -> Just n
    _ -> Nothing

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
  | LetExpr [Definition] Expr
  | WhereExpr Expr [Definition]
  | NameExpr String
  | IntExpr Int
  | NegateExpr Expr
  | StringExpr String
  | ApplyExpr Expr [Expr]
  | CaseExpr Expr [PatternDef]

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Concat

data PatternDef = PatternDef Pattern Expr

data Pattern = WildcardPattern | VarPattern String | ConstructorPattern String [Pattern]

definition :: Parser Definition
definition = nameDefinition <|> typeDefinition
  where
    nameDefinition = liftA2 Definition (name <* token Equal) expr
    typeDefinition = liftA2 TypeDefinition (typeName <* token Equal) typeExpr

typeExpr :: Parser TypeExpr
typeExpr = typeName |> fmap extract
  where
    extract "Int" = IntType
    extract "String" = StringType
    extract other = CustomType other

braced :: Parser a -> Parser [a]
braced p = token OpenBrace *> sepBy p (token Semicolon) <* token CloseBrace

expr :: Parser Expr
expr = notWhereExpr <|> whereExpr <|> caseExpr
  where
    notWhereExpr = letExpr <|> binExpr <|> caseExpr
    letExpr = liftA2 LetExpr (token Let *> braced definition) (token In *> expr)
    whereExpr = liftA2 WhereExpr notWhereExpr (token Where *> braced definition)

caseExpr :: Parser Expr
caseExpr = liftA2 CaseExpr (token Case *> expr <* token Of) (braced patternDef)
  where
    patternDef = liftA2 PatternDef onePattern expr

onePattern :: Parser Pattern
onePattern = wildCardPattern <|> varPattern <|> constructorPattern
  where
    wildCardPattern = WildcardPattern <$ token Underscore
    varPattern = fmap VarPattern name
    constructorPattern = liftA2 ConstructorPattern typeName (many onePattern)

binExpr :: Parser Expr
binExpr = concatExpr
  where
    concatExpr = ops RightAssociative makeConcat addSubExpr (token PlusPlus)
      where
        makeConcat PlusPlus a b = BinExpr Concat a b
        makeConcat t _ _ = error ("makeConcat: Unexpected Token " ++ show t)
    addSubExpr = ops RightAssociative makeAddSub mulDivExpr (token Plus <|> token Dash)
      where
        makeAddSub Plus a b = BinExpr Add a b
        makeAddSub Dash a b = BinExpr Sub a b
        makeAddSub t _ _ = error ("makeAddSub: Unexpected Token " ++ show t)
    mulDivExpr = ops LeftAssociative makeMulDiv unaryExpr (token Asterisk <|> token FSlash)
      where
        makeMulDiv Asterisk a b = BinExpr Mul a b
        makeMulDiv FSlash a b = BinExpr Mul a b
        makeMulDiv t _ _ = error ("makeMulDiv: Unexpected Token " ++ show t)

unaryExpr :: Parser Expr
unaryExpr = negateExpr <|> appExpr
  where
    negateExpr = (token Dash *> appExpr) |> fmap NegateExpr

appExpr :: Parser Expr
appExpr = some factor |> fmap extract
  where
    extract [e] = e
    extract (e : es) = ApplyExpr e es
    extract _ = error "appExpr: No elements produced after some"

factor :: Parser Expr
factor = intLitt <|> stringLitt <|> nameExpr <|> parensExpr
  where
    intLitt =
      pluck <| \case
        IntLitt i -> Just (IntExpr i)
        _ -> Nothing
    stringLitt =
      pluck <| \case
        StringLitt s -> Just (StringExpr s)
        _ -> Nothing
    nameExpr = (name <|> typeName) |> fmap NameExpr
    parensExpr = token OpenParens *> expr <* token CloseParens
