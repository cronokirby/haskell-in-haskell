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

opt :: Parser a -> Parser (Maybe a)
opt p = (Just <$> p) <|> pure Nothing

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

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))

opsL :: (sep -> a -> a -> a) -> Parser a -> Parser sep -> Parser a
opsL combine p s = liftA2 squash p (many (liftA2 (,) s p))
  where
    squash = foldl' (\acc (sep, a) -> combine sep acc a)

opsR :: (sep -> a -> a -> a) -> Parser a -> Parser sep -> Parser a
opsR combine p s = liftA2 squash p (many (liftA2 (,) s p))
  where
    squash start annotated =
      let (start', annotated') =
            foldl'
              (\(oldStart, stack) (sep, a) -> (a, (sep, oldStart) : stack))
              (start, [])
              annotated
       in foldl' (\acc (sep, a) -> combine sep a acc) start' annotated'

braced :: Parser a -> Parser [a]
braced p = token OpenBrace *> sepBy1 p (token Semicolon) <* token CloseBrace

parensed :: Parser a -> Parser a
parensed p = token OpenParens *> p <* token CloseParens

type Name = String

type TypeName = String

newtype AST = AST [Definition] deriving (Eq, Show)

data Definition
  = ValueDefinition ValueDefinition
  | TypeDefinition TypeName [ConstructorDefinition]
  | TypeSynonym TypeName TypeExpr
  deriving (Eq, Show)

data ConstructorDefinition = ConstructorDefinition TypeName [TypeExpr] deriving (Eq, Show)

data ValueDefinition
  = TypeAnnotation String TypeExpr
  | NameDefinition String Expr
  deriving (Eq, Show)

data TypeExpr
  = StringType
  | IntType
  | CustomType TypeName
  | FunctionType TypeExpr TypeExpr
  deriving (Eq, Show)

data Expr
  = BinExpr BinOp Expr Expr
  | LetExpr [ValueDefinition] Expr
  | WhereExpr Expr [ValueDefinition]
  | LambdaExpr [Name] Expr
  | NameExpr Name
  | LittExpr Litteral
  | NegateExpr Expr
  | ApplyExpr Expr [Expr]
  | CaseExpr Expr [PatternDef]
  deriving (Eq, Show)

data Litteral
  = IntLitteral Int
  | StringLitteral String
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Concat
  deriving (Eq, Show)

data PatternDef = PatternDef Pattern Expr deriving (Eq, Show)

data Pattern
  = WildcardPattern
  | NamePattern String
  | LitteralPattern Litteral
  | ConstructorPattern String [Pattern]
  deriving (Eq, Show)

ast :: Parser AST
ast = fmap AST (braced definition)

definition :: Parser Definition
definition = (fmap ValueDefinition valueDefinition) <|> typeDefinition <|> typeSynonym
  where
    typeDefinition = token Data *> liftA2 TypeDefinition (typeName <* token Equal) (sepBy1 constructorDefinition (token VBar))
    typeSynonym = token Type *> liftA2 TypeSynonym (typeName <* token Equal) typeExpr

constructorDefinition :: Parser ConstructorDefinition
constructorDefinition = liftA2 ConstructorDefinition typeName (many baseType)

valueDefinition :: Parser ValueDefinition
valueDefinition = nameDefinition <|> typeDefinition
  where
    nameDefinition = liftA2 NameDefinition (name <* token Equal) expr
    typeDefinition = liftA2 TypeAnnotation (name <* token DoubleColon) typeExpr

typeExpr :: Parser TypeExpr
typeExpr = opsR (const FunctionType) baseType (token ThinArrow)

baseType :: Parser TypeExpr
baseType = namedType <|> parensed typeExpr
  where
    namedType = typeName |> fmap extract
      where
        extract "Int" = IntType
        extract "String" = StringType
        extract other = CustomType other

expr :: Parser Expr
expr = notWhereExpr <|> whereExpr
  where
    notWhereExpr = letExpr <|> lambdaExpr <|> binExpr <|> caseExpr
    letExpr = token Let *> liftA2 LetExpr (braced valueDefinition) (token In *> expr)
    lambdaExpr = token BSlash *> liftA2 LambdaExpr (some name) (token ThinArrow *> expr)
    whereExpr = liftA2 WhereExpr notWhereExpr (token Where *> braced valueDefinition)

caseExpr :: Parser Expr
caseExpr = liftA2 CaseExpr (token Case *> expr <* token Of) (braced patternDef)
  where
    patternDef = liftA2 PatternDef onePattern (token ThinArrow *> expr)

onePattern :: Parser Pattern
onePattern = notConstructorPattern <|> constructorPattern
  where
    notConstructorPattern = wildCardPattern <|> varPattern <|> littPattern
    wildCardPattern = WildcardPattern <$ token Underscore
    varPattern = fmap NamePattern name
    littPattern = fmap LitteralPattern litteral
    constructorPattern = liftA2 ConstructorPattern typeName (many insideConstructorPattern)
    insideConstructorPattern = notConstructorPattern <|> parensed onePattern

binExpr :: Parser Expr
binExpr = concatExpr
  where
    concatExpr = opsL makeConcat addSubExpr (token PlusPlus)
      where
        makeConcat PlusPlus a b = BinExpr Concat a b
        makeConcat t _ _ = error ("makeConcat: Unexpected Token " ++ show t)
    addSubExpr = opsL makeAddSub mulDivExpr (token Plus <|> token Dash)
      where
        makeAddSub Plus a b = BinExpr Add a b
        makeAddSub Dash a b = BinExpr Sub a b
        makeAddSub t _ _ = error ("makeAddSub: Unexpected Token " ++ show t)
    mulDivExpr = opsL makeMulDiv unaryExpr (token Asterisk <|> token FSlash)
      where
        makeMulDiv Asterisk a b = BinExpr Mul a b
        makeMulDiv FSlash a b = BinExpr Div a b
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
factor = littExpr <|> nameExpr <|> parensed expr
  where
    littExpr = fmap LittExpr litteral

    nameExpr = (name <|> typeName) |> fmap NameExpr

name :: Parser Name
name =
  pluck <| \case
    Name n -> Just n
    _ -> Nothing

typeName :: Parser TypeName
typeName =
  pluck <| \case
    TypeName n -> Just n
    _ -> Nothing

litteral :: Parser Litteral
litteral = intLitt <|> stringLitt
  where
    intLitt =
      pluck <| \case
        IntLitt i -> Just (IntLitteral i)
        _ -> Nothing
    stringLitt =
      pluck <| \case
        StringLitt s -> Just (StringLitteral s)
        _ -> Nothing

data ParseError = FailedParse | AmbiguousParse [(AST, [Token])] deriving (Show)

parse :: [Token] -> Either ParseError AST
parse input =
  let (Parser runParser) = ast
   in case runParser input of
        [] -> Left FailedParse
        [(res, _)] -> Right res
        tooMany -> Left (AmbiguousParse tooMany)
