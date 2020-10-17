{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Lexer (Token (..), lexer) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, gets, modify', runState)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List (foldl', foldl1')
import Data.Maybe (listToMaybe)
import Ourlude

-- Represents the kind of error that can occur
data LexerError = Unexpected Char | UnexpectedEOF | UnmatchedLayout deriving (Eq, Show)

-- Create the right lex error when we encounter an unexpected string
unexpected :: String -> LexerError
unexpected [] = UnexpectedEOF
unexpected (c : _) = Unexpected c

-- A Lexer takes an input string, and can consume part of that string to return a result, or fail
--
-- Lexers are like parser combinators, except that they cannot do conditional decision making,
-- or return multiple results. They always return the result that consumed more input,
-- which corresponds to the "longest match" rule you want in a lexical analyzer
newtype Lexer a = Lexer {runLexer :: String -> Either LexerError (a, String)}

-- We can map over the result of a lexer, without changing what strings are recognized
instance Functor Lexer where
  fmap f (Lexer l) = Lexer (l >>> fmap (\(a, s) -> (f a, s)))

-- We can squash two lexers together, getting a lexer that recognizes the first input,
-- followed by the second input
instance Applicative Lexer where
  pure a = Lexer (\input -> Right (a, input))
  Lexer lF <*> Lexer lA =
    Lexer <| \input -> do
      (f, rest) <- lF input
      (a, s) <- lA rest
      return (f a, s)

-- We can choose between two successful lexes by picking the one that consumed more input
instance Alternative Lexer where
  empty = Lexer (Left <<< unexpected)
  Lexer lA <|> Lexer lB =
    Lexer <| \input -> case (lA input, lB input) of
      (res, Left _) -> res
      (Left _, res) -> res
      -- Implement the longest match rule
      (a@(Right (_, restA)), b@(Right (_, restB))) ->
        if length restA <= length restB then a else b

-- A lexer that matches a single character matching a predicate
satisfies :: (Char -> Bool) -> Lexer Char
satisfies p =
  Lexer <| \input -> case input of
    c : cs | p c -> Right (c, cs)
    rest -> Left (unexpected rest)

-- A lexer that matches a single character
char :: Char -> Lexer Char
char target = satisfies (== target)

-- A lexer that matches an entire string
string :: String -> Lexer String
string = traverse char

-- Create an alternation of a list of lexers.
--
-- This will match if any of the lexers matches, picking the longest match, as usual.
oneOf :: [Lexer a] -> Lexer a
oneOf = foldl1' (<|>)

-- Represents a kind of Token we can lex out.
--
-- The idea is that our lexer will convert a stream / list of
-- characters into a corresponding stream / list of tokens,
-- which correspond to individual portions of the stream.
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
  | Dash -- `-`
  | Asterisk -- `*`
  | Equal -- `=`
  | Dot -- `.`
  | Dollar -- `$`
  | IntLitt Int -- An integer litteral
  | TypeName String -- A reference to some kind of type name
  | Name String -- A reference to some kind of name
  deriving (Eq, Show)

-- Lex out one of the tokens in our language
token :: Lexer (Token, String)
token = keywords <|> operators <|> intLitt <|> typeName <|> name
  where
    with :: Functor f => b -> f a -> f (b, a)
    with b = fmap (\a -> (b, a))
    keywords :: Lexer (Token, String)
    keywords =
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
    operators :: Lexer (Token, String)
    operators =
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
          Dash `with` string "-",
          Asterisk `with` string "*",
          Equal `with` string "=",
          Dot `with` string ".",
          Dollar `with` string "$"
        ]
    intLitt :: Lexer (Token, String)
    intLitt = some (satisfies isDigit) |> fmap (\x -> (IntLitt (read x), x))
    continuesName :: Lexer Char
    continuesName = satisfies isAlphaNum <|> char '\''
    typeName :: Lexer (Token, String)
    typeName = (liftA2 (:) (satisfies isUpper) (many continuesName)) |> fmap (\x -> (TypeName x, x))
    name :: Lexer (Token, String)
    name = (liftA2 (:) (satisfies isLower) (many continuesName)) |> fmap (\x -> (Name x, x))

-- A raw token is either a "real" token, or some whitespace that we actually want to ignore
data RawToken = Blankspace String | Comment String | Newline | RawToken Token String

-- A Lexer for raw tokens
rawLexer :: Lexer [RawToken]
rawLexer = some (whitespace <|> comment <|> fmap (uncurry RawToken) token)
  where
    whitespace = blankspace <|> newline
    blankspace = Blankspace <$> some (satisfies (\x -> isSpace x && x /= '\n'))
    comment = Comment <$> (string "--" *> some (satisfies (\x -> x /= '\n')))
    newline = Newline <$ char '\n'

-- Represents a position some token can have in the middle of a line.
--
-- A token is either at the start of the line, or appears somewhere in the middle
data LinePosition = Start | Middle deriving (Eq, Show)

-- Some type annotated with a position
data Positioned a = Positioned a LinePosition Int deriving (Show)

-- Take tokens and whitespace, and return positioned tokens, with whitespace filtered out
position :: [RawToken] -> [Positioned Token]
position = foldl' go ((Start, 0), []) >>> snd >>> reverse
  where
    eat :: (LinePosition, Int) -> RawToken -> ((LinePosition, Int), Maybe (Positioned Token))
    eat _ Newline = ((Start, 0), Nothing)
    eat _ (Comment _) = ((Start, 0), Nothing)
    eat (pos, col) (Blankspace s) = ((pos, col + length s), Nothing)
    eat (pos, col) (RawToken t s) = ((Middle, col + length s), Just (Positioned t pos col))
    go :: ((LinePosition, Int), [Positioned Token]) -> RawToken -> ((LinePosition, Int), [Positioned Token])
    go (p, acc) raw = case eat p raw of
      (p', Just tok) -> (p', tok : acc)
      (p', Nothing) -> (p', acc)

-- A layout is either one explicitly declared by the user, or implicitly declared at a certain column
data Layout = Explicit | Implicit Int

data LayoutState = LayoutState {layouts :: [Layout], tokens :: [Token], expectingLayout :: Bool}

type LayoutM a = ExceptT LexerError (State LayoutState) a

yieldToken :: Token -> LayoutM ()
yieldToken t = modify' (\s -> s {tokens = t : tokens s})

pushLayout :: Layout -> LayoutM ()
pushLayout l = modify' (\s -> s {layouts = l : layouts s})

popLayout :: LayoutM ()
popLayout = modify' (\s -> s {layouts = pop (layouts s)})
  where
    pop [] = []
    pop (_ : xs) = xs

topLayout :: LayoutM (Maybe Layout)
topLayout = gets layouts |> fmap listToMaybe

compareIndentation :: Int -> LayoutM Ordering
compareIndentation col = do
  top <- topLayout
  return <| case top of
    Nothing -> GT
    Just Explicit -> GT
    Just (Implicit n) -> compare col n

runLayoutM :: LayoutM a -> Either LexerError [Token]
runLayoutM =
  runExceptT >>> (`runState` (LayoutState [] [] True)) >>> \case
    (Left e, _) -> Left e
    (Right _, LayoutState _ ts _) -> Right (reverse ts)

layout :: [Positioned Token] -> Either LexerError [Token]
layout inputs =
  runLayoutM <| do
    mapM_ step inputs
    closeImplicitLayouts
  where
    startsLayout :: Token -> Bool
    startsLayout t = elem t [Let, Where, Of]
    step :: Positioned Token -> LayoutM ()
    step (Positioned t linePos col) = do
      expectingLayout' <- gets expectingLayout
      case t of
        CloseBrace -> closeExplicitLayout
        OpenBrace | expectingLayout' -> startExplicitLayout
        _ | startsLayout t -> modify' (\s -> s {expectingLayout = True})
        _ | expectingLayout' -> startImplicitLayout col
        _ | linePos == Start -> continueImplicitLayout col
        _ -> return ()
      yieldToken t
    closeExplicitLayout :: LayoutM ()
    closeExplicitLayout =
      topLayout >>= \case
        Just Explicit -> popLayout
        _ -> throwError (Unexpected '}')
    startExplicitLayout :: LayoutM ()
    startExplicitLayout = do
      modify' (\s -> s {expectingLayout = False})
      pushLayout Explicit
    startImplicitLayout :: Int -> LayoutM ()
    startImplicitLayout col = do
      modify' (\s -> s {expectingLayout = False})
      compareIndentation col >>= \case
        GT -> do
          yieldToken OpenBrace
          pushLayout (Implicit col)
        _ -> do
          yieldToken OpenBrace
          yieldToken CloseBrace
          continueImplicitLayout col
    continueImplicitLayout :: Int -> LayoutM ()
    continueImplicitLayout col = do
      closeFurtherLayouts
      compareIndentation col >>= \case
        EQ -> yieldToken Semicolon
        _ -> return ()
      where
        closeFurtherLayouts =
          compareIndentation col >>= \case
            LT -> do
              yieldToken CloseBrace
              popLayout
              closeFurtherLayouts
            _ -> return ()
    closeImplicitLayouts :: LayoutM ()
    closeImplicitLayouts =
      topLayout >>= \case
        Nothing -> return ()
        Just Explicit -> throwError UnmatchedLayout
        Just (Implicit _) -> do
          yieldToken CloseBrace
          popLayout
          closeImplicitLayouts

-- Lex a specific string, producing a list of tokens if no errors occurred.
lexer :: String -> Either LexerError [Token]
lexer input = do
  (raw, _) <- runLexer rawLexer input
  raw |> position |> layout
