{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lexer (Token (..), lexer) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, gets, modify', runState)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List (foldl', foldl1')
import Data.Maybe (listToMaybe)
import Ourlude

data LexerError
  = Unexpected Char
  | UnexpectedEOF
  | UnmatchedLayout
  deriving (Eq, Show)

unexpected :: String -> LexerError
unexpected [] = UnexpectedEOF
unexpected (c : _) = Unexpected c

newtype Lexer a = Lexer
  { runLexer :: String -> Either LexerError (a, String)
  }

satisfies :: (Char -> Bool) -> Lexer Char
satisfies p =
  Lexer <| \case
    c : cs | p c -> Right (c, cs)
    rest -> Left (unexpected rest)

char :: Char -> Lexer Char
char target = satisfies (== target)

instance Functor Lexer where
  fmap f (Lexer l) = Lexer (l >>> fmap (first f))

instance Applicative Lexer where
  pure a = Lexer (\input -> Right (a, input))
  Lexer lF <*> Lexer lA =
    Lexer <| \input -> do
      (f, rest) <- lF input
      (a, s) <- lA rest
      return (f a, s)

string :: String -> Lexer String
string = traverse char

instance Alternative Lexer where
  empty = Lexer (Left <<< unexpected)
  Lexer lA <|> Lexer lB =
    Lexer <| \input -> case (lA input, lB input) of
      (res, Left _) -> res
      (Left _, res) -> res
      (a@(Right (_, restA)), b@(Right (_, restB))) ->
        if length restA <= length restB then a else b

oneOf :: Alternative f => [f a] -> f a
oneOf = foldl1' (<|>)

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
  | PlusPlus -- `++`
  | Dash -- `-`
  | Asterisk -- `*`
  | Equal -- `=`
  | Dollar -- `$`
  | LeftAngle -- `<`
  | Dot -- `.`
  | LeftAngleEqual -- `<=`
  | RightAngle -- `>`
  | RightAngleEqual -- `>=`
  | EqualEqual -- `==`
  | FSlashEqual -- `/=`
  | VBarVBar -- `||`
  | AmpersandAmpersand -- `&&`
  -- An Int litteral
  | IntLitt Int -- An Int litteral
  -- A String litteral
  | StringLitt String
  | -- A Bool litteral
    BoolLitt Bool
  | -- The type `Int`
    IntTypeName
  | -- The type `String`
    StringTypeName
  | -- The type `Bool`
    BoolTypeName
  | -- A name starting with an uppercase letter
    UpperName String
  | -- A name starting witha lowercase letter
    LowerName String
  deriving (Eq, Show)

token :: Lexer (Token, String)
token = keyword <|> operator <|> litteral <|> name
  where
    with :: Functor f => b -> f a -> f (b, a)
    with b = fmap (b,)

    keyword :: Lexer (Token, String)
    keyword =
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

    operator :: Lexer (Token, String)
    operator =
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
          PlusPlus `with` string "++",
          Dash `with` string "-",
          Asterisk `with` string "*",
          Equal `with` string "=",
          Dot `with` string ".",
          Dollar `with` string "$",
          LeftAngle `with` string "<",
          LeftAngleEqual `with` string "<=",
          RightAngle `with` string ">",
          RightAngleEqual `with` string ">=",
          FSlashEqual `with` string "/=",
          EqualEqual `with` string "==",
          VBarVBar `with` string "||",
          AmpersandAmpersand `with` string "&&"
        ]

    litteral :: Lexer (Token, String)
    litteral = intLitt <|> stringLitt <|> boolLitt
      where
        intLitt :: Lexer (Token, String)
        intLitt =
          some (satisfies isDigit)
            |> fmap (\x -> (IntLitt (read x), x))

        stringLitt :: Lexer (Token, String)
        stringLitt =
          char '"' *> many (satisfies (/= '"')) <* char '"'
            |> fmap (\x -> (StringLitt x, x))
        
        boolLitt :: Lexer (Token, String)
        boolLitt =
          (BoolLitt True `with` string "True")
            <|> (BoolLitt False `with` string "False")

    name :: Lexer (Token, String)
    name = primName <|> upperName <|> lowerName
      where
        primName :: Lexer (Token, String)
        primName =
          (IntTypeName `with` string "Int")
            <|> (StringTypeName `with` string "String")
            <|> (BoolTypeName `with` string "Bool")

        continuesName :: Lexer Char
        continuesName = satisfies isAlphaNum <|> char '\''

        followedBy :: Lexer Char -> Lexer Char -> Lexer String
        followedBy l1 l2 = liftA2 (:) l1 (many l2)

        upperName :: Lexer (Token, String)
        upperName =
          (satisfies isUpper `followedBy` continuesName)
            |> fmap (\x -> (UpperName x, x))

        lowerName :: Lexer (Token, String)
        lowerName =
          (satisfies isLower `followedBy` continuesName)
            |> fmap (\x -> (LowerName x, x))

lexer :: String -> Either LexerError [Token]
lexer = runLexer (token |> fmap fst |> some) >>> fmap fst
