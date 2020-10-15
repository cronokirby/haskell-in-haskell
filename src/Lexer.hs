module Lexer where

import Ourlude

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
