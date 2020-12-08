{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lexer (Token (..), lexer) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, gets, modify', runState)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List (foldl', foldl1')
import Data.Maybe (listToMaybe, maybeToList)
import Ourlude

-- Represents the kind of error that can occur
data LexerError
  = -- We encountered a character we weren't expecting to see
    Unexpected Char
  | -- We reached the end of a file while still expecting characters
    UnexpectedEOF
  | -- An indentation layout was created but not closed
    UnmatchedLayout
  deriving (Eq, Show)

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
  fmap f (Lexer l) = Lexer (l >>> fmap (first f))

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
  Lexer <| \case
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
oneOf :: Alternative f => [f a] -> f a
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
  | IntLitt Int -- An Int litteral
  | StringLitt String -- A String litteral
  | BoolLitt Bool -- A Bool litteral
  | IntTypeName -- The typename `Int`
  | StringTypeName -- The typename `String`
  | BoolTypeName -- The typename `Bool`
  | UpperName String -- A reference to a name beginning with an upper case
  | LowerName String -- A reference to a name beginning with a lower case
  deriving (Eq, Show)

-- Lex out one of the tokens in our language
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
        intLitt = some (satisfies isDigit) |> fmap (\x -> (IntLitt (read x), x))

        stringLitt :: Lexer (Token, String)
        stringLitt = char '"' *> (many (satisfies (/= '"')) <* char '"') |> fmap (\x -> (StringLitt x, x))

        boolLitt :: Lexer (Token, String)
        boolLitt = (BoolLitt True `with` string "True") <|> (BoolLitt False `with` string "False")

    name :: Lexer (Token, String)
    name = primName <|> upperName <|> lowerName
      where
        continuesName :: Lexer Char
        continuesName = satisfies isAlphaNum <|> char '\''

        followedBy :: Lexer Char -> Lexer Char -> Lexer String
        followedBy l1 l2 = liftA2 (:) l1 (many l2)

        upperName :: Lexer (Token, String)
        upperName = (satisfies isUpper `followedBy` continuesName) |> fmap (\x -> (UpperName x, x))

        lowerName :: Lexer (Token, String)
        lowerName = (satisfies isLower `followedBy` continuesName) |> fmap (\x -> (LowerName x, x))

        primName :: Lexer (Token, String)
        primName =
          (IntTypeName `with` string "Int")
            <|> (StringTypeName `with` string "String")
            <|> (BoolTypeName `with` string "Bool")

-- A raw token is either a "real" token, or some whitespace that we actually want to ignore
data RawToken = Blankspace String | Comment String | Newline | RawToken Token String

-- A Lexer for raw tokens
rawLexer :: Lexer [RawToken]
rawLexer = some (whitespace <|> comment <|> fmap (uncurry RawToken) token)
  where
    whitespace = blankspace <|> newline
    blankspace = Blankspace <$> some (satisfies (\x -> isSpace x && x /= '\n'))
    comment = Comment <$> (string "--" *> many (satisfies (/= '\n')))
    newline = Newline <$ char '\n'

-- Represents a position some token can have in the middle of a line.
--
-- A token is either at the start of the line, or appears somewhere in the middle
data LinePosition = Start | Middle deriving (Eq, Show)

-- Some type annotated with a position
data Positioned a = Positioned a LinePosition Int deriving (Show)

type PosState = (LinePosition, Int)

-- Take tokens and whitespace, and return positioned tokens, with whitespace filtered out
position :: [RawToken] -> [Positioned Token]
position = foldl' go ((Start, 0), []) >>> snd >>> reverse
  where
    eat :: PosState -> RawToken -> (PosState, Maybe (Positioned Token))
    eat (pos, col) = \case
      Newline -> ((Start, 0), Nothing)
      Comment _ -> ((Start, 0), Nothing)
      Blankspace s -> ((pos, col + length s), Nothing)
      RawToken t s -> ((Middle, col + length s), Just (Positioned t pos col))
    go :: (PosState, [Positioned Token]) -> RawToken -> (PosState, [Positioned Token])
    go (p, acc) raw =
      let (p', produced) = eat p raw
      in (p', maybeToList produced <> acc)

-- A layout is either one explicitly declared by the user, or implicitly declared at a certain column
data Layout = Explicit | Implicit Int

-- Represents the state we have access to as we're laying out our tokens
--
-- We have a current stack of layouts, a stream of tokens, and a flag to know if the
-- we're looking to start a layout with the next token.
data LayoutState = LayoutState
  { layouts :: [Layout],
    tokens :: [Token],
    expectingLayout :: Bool
  }

-- The Monadic context we use for laying out tokens.
--
-- We might fail with an error, and otherwise we have access to a context we can modify.
type LayoutM a = ExceptT LexerError (State LayoutState) a

-- Produce a token
yieldToken :: Token -> LayoutM ()
yieldToken t = modify' (\s -> s {tokens = t : tokens s})

-- Push a new layout onto our stack
pushLayout :: Layout -> LayoutM ()
pushLayout l = modify' (\s -> s {layouts = l : layouts s})

-- Pop a layout from our stack.
--
-- This has no effect if our stack is empty.
popLayout :: LayoutM ()
popLayout = modify' (\s -> s {layouts = drop 1 (layouts s)})

-- Get the current layout, if it exists.
currentLayout :: LayoutM (Maybe Layout)
currentLayout = gets layouts |> fmap listToMaybe

-- Compare a level of indentation with the current layout.
--
-- The provided column is greater than no layout, or an explicit layout. And
-- compares with an implicit layout based on its column.
compareIndentation :: Int -> LayoutM Ordering
compareIndentation col =
  let cmp Nothing = GT
      cmp (Just Explicit) = GT
      cmp (Just (Implicit n)) = compare col n
   in fmap cmp currentLayout

-- Run the layout context, producing either an error, or the tokens with the inferred layout tokens.
runLayoutM :: LayoutM a -> Either LexerError [Token]
runLayoutM =
  runExceptT >>> (`runState` LayoutState [] [] True) >>> \case
    (Left e, _) -> Left e
    (Right _, LayoutState _ ts _) -> Right (reverse ts)

-- Take a stream of positioned tokens, and produce either an error, or the tokens
-- with semicolons and braces inserted judiciously.
layout :: [Positioned Token] -> Either LexerError [Token]
layout inputs =
  runLayoutM <| do
    mapM_ step inputs
    closeImplicitLayouts
  where
    startsLayout :: Token -> Bool
    startsLayout = (`elem` [Let, Where, Of])

    step :: Positioned Token -> LayoutM ()
    step (Positioned t linePos col) = do
      expectingLayout' <- gets expectingLayout
      case t of
        CloseBrace -> closeExplicitLayout
        OpenBrace | expectingLayout' -> startExplicitLayout
        _
          | startsLayout t -> modify' (\s -> s {expectingLayout = True})
          | expectingLayout' -> startImplicitLayout col
          | linePos == Start -> continueImplicitLayout col
          | otherwise -> return ()
      yieldToken t

    closeExplicitLayout :: LayoutM ()
    closeExplicitLayout =
      currentLayout >>= \case
        Just Explicit -> popLayout
        _ -> throwError (Unexpected '}')

    startExplicitLayout :: LayoutM ()
    startExplicitLayout = do
      modify' (\s -> s {expectingLayout = False})
      pushLayout Explicit

    startImplicitLayout :: Int -> LayoutM ()
    startImplicitLayout col = do
      modify' (\s -> s {expectingLayout = False})
      -- Regardless of what happens, we're starting a layout...
      compareIndentation col >>= \case
        GT -> do
          yieldToken OpenBrace
          pushLayout (Implicit col)
        -- But if we're not indented further, we're immediately ending that layout.
        -- Furthermore, we might be continuing an implicit layout.
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
      currentLayout >>= \case
        Nothing -> return ()
        Just Explicit -> throwError UnmatchedLayout
        Just (Implicit _) -> do
          yieldToken CloseBrace
          popLayout
          closeImplicitLayouts

-- Lex a specific string, producing a list of tokens if no errors occurred.
lexer :: String -> Either LexerError [Token]
lexer input =
  runLexer rawLexer input >>= (fst >>> position >>> layout)
