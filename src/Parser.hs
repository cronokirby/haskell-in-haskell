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

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))

opsL :: Parser (a -> a -> a) -> Parser a -> Parser a
opsL sep p = liftA2 squash p (many (liftA2 (,) sep p))
  where
    squash = foldl' (\acc (combine, a) -> combine acc a)

opsR :: Parser (a -> a -> a) -> Parser a -> Parser a
opsR sep p = liftA2 squash p (many (liftA2 (,) sep p))
  where
    squash start annotated =
      let (start', annotated') = foldl' shift (start, []) annotated
          shift (oldStart, stack) (combine, a) = (a, (combine, oldStart) : stack)
       in foldl' (\acc (combine, a) -> combine a acc) start' annotated'

braced :: Parser a -> Parser [a]
braced p = token OpenBrace *> sepBy1 p (token Semicolon) <* token CloseBrace

parensed :: Parser a -> Parser a
parensed p = token OpenParens *> p <* token CloseParens

type ValName = String

type ConstructorName = String

type Name = String

newtype AST = AST [Definition] deriving (Eq, Show)

data Definition
  = ValueDefinition ValueDefinition
  | DataDefinition TypeName [TypeVar] [ConstructorDefinition]
  | TypeSynonym TypeName Type
  deriving (Eq, Show)

data ConstructorDefinition = ConstructorDefinition ConstructorName [Type] deriving (Eq, Show)

data ValueDefinition
  = TypeAnnotation ValName Type
  | NameDefinition ValName [Pattern] Expr
  deriving (Eq, Show)

data Expr
  = BinExpr BinOp Expr Expr
  | LetExpr [ValueDefinition] Expr
  | WhereExpr Expr [ValueDefinition]
  | IfExpr Expr Expr Expr
  | LambdaExpr [ValName] Expr
  | NameExpr Name
  | LitExpr Literal
  | NegateExpr Expr
  | ApplyExpr Expr [Expr]
  | CaseExpr Expr [(Pattern, Expr)]
  deriving (Eq, Show)

data Literal
  = IntLiteral Int
  | StringLiteral String
  | BoolLiteral Bool
  deriving (Eq, Ord, Show)

data BinOp
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
  deriving (Eq, Show)

data Pattern
  = WildcardPattern
  | NamePattern ValName
  | LiteralPattern Literal
  | ConstructorPattern ConstructorName [Pattern]
  deriving (Eq, Show)

ast :: Parser AST
ast = fmap AST (braced definition)

definition :: Parser Definition
definition = fmap ValueDefinition valueDefinition <|> typeDefinition <|> typeSynonym
  where
    typeDefinition =
      token Data
        *> ( DataDefinition
               <$> typeName
               <*> many typeVar
               <*> (token Equal *> sepBy1 constructorDefinition (token VBar))
           )
    typeSynonym = token Type *> liftA2 TypeSynonym (typeName <* token Equal) typeExpr

constructorDefinition :: Parser ConstructorDefinition
constructorDefinition = liftA2 ConstructorDefinition constructorName (many unspacedType)

valueDefinition :: Parser ValueDefinition
valueDefinition = nameDefinition <|> typeDefinition
  where
    nameDefinition = NameDefinition <$> valName <*> many unspacedPattern <*> (token Equal *> expr)
    typeDefinition = liftA2 TypeAnnotation (valName <* token DoubleColon) typeExpr

typeExpr :: Parser Type
typeExpr = opsR ((:->) <$ token ThinArrow) baseType
  where
    baseType = singleType <|> customType
    customType = liftA2 CustomType typeName (many unspacedType)

unspacedType :: Parser Type
unspacedType = namedType <|> singleType
  where
    namedType = fmap (`CustomType` []) typeName

singleType :: Parser Type
singleType = fmap TVar typeVar <|> primType <|> parensed typeExpr
  where
    primType =
      (IntT <$ token IntTypeName)
        <|> (StringT <$ token StringTypeName)
        <|> (BoolT <$ token BoolTypeName)

expr :: Parser Expr
expr = notWhereExpr <|> whereExpr
  where
    notWhereExpr = letExpr <|> ifExpr <|> lambdaExpr <|> binExpr <|> caseExpr
    letExpr = token Let *> liftA2 LetExpr (braced valueDefinition) (token In *> expr)
    ifExpr = IfExpr <$> (token If *> expr) <*> (token Then *> expr) <*> (token Else *> expr)
    lambdaExpr = token BSlash *> liftA2 LambdaExpr (some valName) (token ThinArrow *> expr)
    whereExpr = liftA2 WhereExpr notWhereExpr (token Where *> braced valueDefinition)

caseExpr :: Parser Expr
caseExpr = liftA2 CaseExpr (token Case *> expr <* token Of) (braced patternDef)
  where
    patternDef = liftA2 (,) onePattern (token ThinArrow *> expr)

onePattern :: Parser Pattern
onePattern = unspacedPattern <|> argfulPattern
  where
    argfulPattern = liftA2 ConstructorPattern constructorName (some unspacedPattern)

unspacedPattern :: Parser Pattern
unspacedPattern = simplePattern <|> parensed onePattern
  where
    simplePattern = wildCardPattern <|> varPattern <|> litPattern <|> singleConstructor
    singleConstructor = fmap (`ConstructorPattern` []) constructorName
    wildCardPattern = WildcardPattern <$ token Underscore
    varPattern = fmap NamePattern valName
    litPattern = fmap LiteralPattern literal

binExpr :: Parser Expr
binExpr = cashExpr
  where
    cashExpr = opsR (BinExpr Cash <$ token Dollar) orExpr
    orExpr = opsR (BinExpr Or <$ token VBarVBar) andExpr
    andExpr = opsR (BinExpr And <$ token AmpersandAmpersand) comparisonExpr
    comparisonExpr = opsL comparisonOperator concatExpr
      where
        comparisonOperator =
          (BinExpr Less <$ token LeftAngle)
            <|> (BinExpr LessEqual <$ token LeftAngleEqual)
            <|> (BinExpr Greater <$ token RightAngle)
            <|> (BinExpr GreaterEqual <$ token RightAngleEqual)
            <|> (BinExpr EqualTo <$ token EqualEqual)
            <|> (BinExpr NotEqualTo <$ token FSlashEqual)
    concatExpr = opsL (BinExpr Concat <$ token PlusPlus) addSubExpr
    addSubExpr = opsL addSubOperator mulDivExpr
      where
        addSubOperator =
          (BinExpr Add <$ token Plus)
            <|> (BinExpr Sub <$ token Dash)
    mulDivExpr = opsL mulDivOperator composeExpr
      where
        mulDivOperator =
          (BinExpr Mul <$ token Asterisk)
            <|> (BinExpr Div <$ token FSlash)
    composeExpr = opsR (BinExpr Compose <$ token Dot) unaryExpr

unaryExpr :: Parser Expr
unaryExpr = negateExpr <|> appExpr
  where
    negateExpr = (token Dash *> appExpr) |> fmap NegateExpr

appExpr :: Parser Expr
appExpr = some factor |> fmap extract
  where
    extract [] = error "appExpr: No elements produced after some"
    extract [e] = e
    extract (e : es) = ApplyExpr e es

factor :: Parser Expr
factor = littExpr <|> nameExpr <|> parensed expr
  where
    littExpr = fmap LitExpr literal
    nameExpr = fmap NameExpr name

lowerName :: Parser ValName
lowerName =
  pluck <| \case
    LowerName n -> Just n
    _ -> Nothing

upperName :: Parser TypeName
upperName =
  pluck <| \case
    UpperName n -> Just n
    _ -> Nothing

typeVar :: Parser TypeVar
typeVar = lowerName

typeName :: Parser TypeName
typeName = upperName

valName :: Parser ValName
valName = lowerName

constructorName :: Parser ConstructorName
constructorName = upperName

name :: Parser Name
name = valName <|> constructorName

literal :: Parser Literal
literal = intLit <|> stringLit <|> boolLit
  where
    intLit =
      pluck <| \case
        IntLit i -> Just (IntLiteral i)
        _ -> Nothing
    stringLit =
      pluck <| \case
        StringLit s -> Just (StringLiteral s)
        _ -> Nothing
    boolLit =
      pluck <| \case
        BoolLit b -> Just (BoolLiteral b)
        _ -> Nothing

data ParseError = FailedParse | AmbiguousParse [(AST, [Token])] deriving (Show)

parser :: [Token] -> Either ParseError AST
parser input = case runParser ast input of
  [] -> Left FailedParse
  [(res, _)] -> Right res
  tooMany -> Left (AmbiguousParse tooMany)
