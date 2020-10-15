module Ourlude
  ( module Prelude,
    (|>),
    (<|),
    (>>>),
    (<<<),
  )
where

import Prelude

infixl 1 |>

-- A forward composition operator, that I find prettier than (&)
(|>) :: a -> (a -> b) -> b
x |> f = f x
{-# INLINE (|>) #-}

infixr 0 <|

-- A backwards composition operator, that I find prettier than ($)
(<|) :: (a -> b) -> a -> b
(<|) = ($)
{-# INLINE (<|) #-}

infixr 9 <<<

(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
g <<< f = g . f
{-# INLINE (<<<) #-}

infixl 9 >>>

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f
{-# INLINE (>>>) #-}
