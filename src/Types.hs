{-# LANGUAGE FlexibleInstances #-}

module Types
  ( Type (..),
    TypeVar,
    TypeName,
    FreeTypeVars (..),
    Scheme (..),
    closeType,
    asGeneral
  )
where

import qualified Data.Set as Set
import Ourlude

-- A type variable is a lower case name for some variable stub in a type
type TypeVar = String

-- A type name is a valid, upper case, name for a given type
type TypeName = String

infixr 2 :->

-- Our representation of a type
data Type
  = -- The type of strings
    StringT
  | -- The type of integers
    IntT
  | -- The type of booleans
    BoolT
  | -- A custom type, with a given name, and a list of arguments
    CustomType TypeName [Type]
  | -- A type variable
    TVar TypeVar
  | -- A function type
    Type :-> Type
  deriving (Eq, Show)

-- Represents a scheme, i.e., a type quantified over some set of polymorphic variables
data Scheme = Scheme [TypeVar] Type deriving (Eq, Show)

-- A class of types where we can find the free type names inside
--
-- These are the type variables appearing inside of a given type
class FreeTypeVars a where
  ftv :: a -> Set.Set TypeName

instance FreeTypeVars Type where
  ftv IntT = Set.empty
  ftv StringT = Set.empty
  ftv BoolT = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 :-> t2) = ftv t1 <> ftv t2
  ftv (CustomType _ ts) = foldMap ftv ts

instance FreeTypeVars TypeName where
  ftv = Set.singleton

instance FreeTypeVars Scheme where
  ftv (Scheme vars t) = Set.difference (ftv t) (Set.fromList vars)

instance (Ord a, FreeTypeVars a) => FreeTypeVars (Set.Set a) where
  ftv = foldMap ftv

-- Close a type expression over all of the free names appearing inside
closeType :: Type -> Scheme
closeType t = Scheme (ftv t |> Set.toList) t

asGeneral :: Scheme -> Scheme -> Bool
asGeneral (Scheme vars1 _) (Scheme vars2 _) = length vars1 >= length vars2
