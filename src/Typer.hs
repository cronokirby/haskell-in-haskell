{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Typer where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Ourlude
import Simplifier (Definition (..), Name, SchemeExpr (..), TypeExpr (..), TypeName)

-- Represents some kind of constraint we generate during our gathering pharse.
--
-- This provides us with information about how different types are used, and is
-- necessary to be able to infer the correct types later on.
data Constraint
  = -- An assertion that two type expressions are equivalent
    SameType TypeExpr TypeExpr
  | -- An assertation that some type explicitly instantiates some scheme
    ExplicitlyInstantiates TypeExpr SchemeExpr
  | -- An assertion that some type implicitly insntatiates some type, generalized over some names
    ImplicitlyInstantations TypeExpr (Set.Set TypeName) TypeExpr
  deriving (Eq, Show)

-- Represents a substitution of types for type names
data Subst = Subst (Map.Map TypeName TypeExpr) deriving (Eq, Show)

-- A class for types where substitutions can be applied
class Substitutable a where
  subst :: Subst -> a -> a

instance (Ord a, Substitutable a) => Substitutable (Set.Set a) where
  subst = subst >>> Set.map

instance Substitutable TypeName where
  subst (Subst s) a = case Map.findWithDefault (TypeVar a) a s of
    TypeVar tn -> tn
    _ -> a

instance Substitutable TypeExpr where
  subst sub@(Subst s) t = case t of
    IntType -> IntType
    StringType -> StringType
    BoolType -> BoolType
    TypeVar a -> Map.findWithDefault (TypeVar a) a s
    FunctionType t1 t2 -> FunctionType (subst sub t1) (subst sub t2)
    CustomType name ts -> CustomType name (map (subst sub) ts)

instance Substitutable SchemeExpr where
  subst (Subst s) (SchemeExpr vars t) =
    let s' = Subst (foldr Map.delete s vars)
     in SchemeExpr vars (subst s' t)

instance Substitutable Constraint where
  subst s (SameType t1 t2) = SameType (subst s t1) (subst s t2)
  subst s (ExplicitlyInstantiates t sc) =
    ExplicitlyInstantiates (subst s t) (subst s sc)
  subst s (ImplicitlyInstantations t1 vars t2) =
    ImplicitlyInstantations (subst s t1) (subst s vars) (subst s t2)

-- A class of types where we can find the free type names inside
class FreeTypeVars a where
  ftv :: a -> Set.Set TypeName

instance FreeTypeVars TypeExpr where
  ftv IntType = Set.empty
  ftv StringType = Set.empty
  ftv (TypeVar a) = Set.singleton a
  ftv (FunctionType t1 t2) = Set.union (ftv t1) (ftv t2)
  ftv (CustomType _ ts) = foldMap ftv ts

instance FreeTypeVars TypeName where
  ftv = Set.singleton

instance FreeTypeVars SchemeExpr where
  ftv (SchemeExpr vars t) = Set.difference (ftv t) (Set.fromList vars)

instance (Ord a, FreeTypeVars a) => FreeTypeVars (Set.Set a) where
  ftv = foldMap ftv

-- A class for types where we can detect which variables are important
-- in a constraint
class ActiveTypeVars a where
  atv :: a -> Set.Set TypeName

instance ActiveTypeVars Constraint where
  atv (SameType t1 t2) = Set.union (ftv t1) (ftv t2)
  atv (ExplicitlyInstantiates t sc) = Set.union (ftv t) (ftv sc)
  atv (ImplicitlyInstantations t1 vars t2) =
    Set.union (ftv t1) (Set.intersection (ftv vars) (ftv t2))

instance ActiveTypeVars a => ActiveTypeVars [a] where
  atv = foldMap atv

-- Represents a kind of error that can happen while type checking
data TypeError
  = -- There's a mismatch between two different types
    TypeMismatch TypeExpr TypeExpr
  | -- Some type name references itself recursively
    InfiniteType TypeName TypeExpr
  | -- An undefined name was used
    UnboundName Name
  | -- A reference to some type that doesn't exist
    UnknownType TypeName
  | -- A mismatch of a type constructor with expected vs actual args
    MismatchedTypeArgs TypeName Int Int
  | -- A type has been defined multiple times
    MultipleTypeDefinitions TypeName
  | -- A type synonym ends up being cyclical
    CyclicalTypeSynonym TypeName [TypeName]

gatherTypeSynonyms :: [Definition t] -> Map.Map TypeName TypeExpr
gatherTypeSynonyms =
  foldMap <| \case
    TypeSynonym name expr -> Map.singleton name expr
    _ -> Map.empty

typeDependencies :: TypeExpr -> [TypeName]
typeDependencies StringType = []
typeDependencies IntType = []
typeDependencies BoolType = []
typeDependencies (TypeVar _) = []
typeDependencies (FunctionType t1 t2) = typeDependencies t1 ++ typeDependencies t2
typeDependencies (CustomType name exprs) = name : concatMap typeDependencies exprs

data SorterState = SorterState {unseen :: Set.Set TypeName, output :: [TypeName]}

type SorterM a = ReaderT (Set.Set TypeName) (StateT SorterState (Except TypeError)) a

sortTypeSynonyms :: Map.Map TypeName TypeExpr -> Either TypeError [TypeName]
sortTypeSynonyms mp = runSorter sort (SorterState (Map.keysSet mp) [])
  where
    deps :: TypeName -> [TypeName]
    deps k = Map.findWithDefault [] k (Map.map typeDependencies mp)
    runSorter :: SorterM a -> SorterState -> Either TypeError a
    runSorter m st =
      runReaderT m Set.empty |> (`runStateT` st) |> runExcept |> fmap fst
    pick :: Set.Set a -> Maybe a
    pick s | Set.null s = Nothing
    pick s = Just (Set.elemAt 0 s)
    see :: TypeName -> SorterM Bool
    see name = do
      unseen' <- gets unseen
      modify' (\s -> s {unseen = Set.delete name unseen'})
      return (Set.member name unseen')
    out :: TypeName -> SorterM ()
    out name = modify' (\s -> s {output = name : output s})
    withAncestor :: TypeName -> SorterM a -> SorterM a
    withAncestor = local <<< Set.insert
    sort :: SorterM [TypeName]
    sort = do
      unseen' <- gets unseen
      case pick unseen' of
        Nothing -> gets output
        Just n -> do
          dfs n
          sort
    dfs :: TypeName -> SorterM ()
    dfs name = do
      ancestors <- ask
      when (Set.member name ancestors) <| do
        throwError (CyclicalTypeSynonym name (Set.toList ancestors))
      new <- see name
      when new <| do
        withAncestor name (forM_ (deps name) dfs)
        out name
