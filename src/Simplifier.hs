{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Simplifier
  ( AST (..),
    SimplifierError (..),
    simplifier,
  )
where

import Ourlude
import qualified Parser as P

data AST t = AST deriving (Show)

data SimplifierError = NotImplementedYet deriving (Eq, Show)

simplifier :: P.AST -> Either SimplifierError (AST ())
simplifier _ = Left NotImplementedYet
