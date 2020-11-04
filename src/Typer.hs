{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Typer where

import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (delete, find)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Ourlude
import Simplifier (AST (..), Builtin (..), Definition (..), Expr (..), Litteral (..), Name, SchemeExpr (..), TypeExpr (..), TypeName, ValueDefinition (..))

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

gatherCustomTypes :: [Definition t] -> Map.Map TypeName Int
gatherCustomTypes =
  foldMap <| \case
    TypeDefinition name vars _ -> Map.singleton name (length vars)
    _ -> Map.empty

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

-- Represents the information we might have when resolving a type name
data ResolvingInformation
  = -- The name is a synonym for a fully resolved expression
    Synonym TypeExpr
  | -- The name is a custom type with a certain arity
    Custom Int

type ResolutionMap = Map.Map TypeName ResolvingInformation

resolve :: MonadError TypeError m => ResolutionMap -> TypeExpr -> m TypeExpr
resolve _ IntType = return IntType
resolve _ StringType = return StringType
resolve _ BoolType = return BoolType
resolve _ (TypeVar a) = return (TypeVar a)
resolve mp (FunctionType t1 t2) = do
  t1' <- resolve mp t1
  t2' <- resolve mp t2
  return (FunctionType t1' t2')
resolve mp ct@(CustomType name ts) = case Map.lookup name mp of
  Nothing -> throwError (UnknownType name)
  Just (Synonym t) | null ts -> return t
  Just (Synonym _) -> throwError (MismatchedTypeArgs name 0 (length ts))
  Just (Custom arity) | arity == length ts -> return ct
  Just (Custom arity) -> throwError (MismatchedTypeArgs name arity (length ts))

type ResolutionM a = ReaderT (Map.Map TypeName TypeExpr) (StateT ResolutionMap (Except TypeError)) a

createResolutions :: [Definition t] -> Either TypeError ResolutionMap
createResolutions defs = do
  let customInfo = gatherCustomTypes defs
      typeSynMap = gatherTypeSynonyms defs
  names <- sortTypeSynonyms typeSynMap
  runResolutionM (resolveAll names) typeSynMap (Map.map Custom customInfo)
  where
    runResolutionM :: ResolutionM a -> Map.Map TypeName TypeExpr -> ResolutionMap -> Either TypeError ResolutionMap
    runResolutionM m typeSynMap st =
      runReaderT m typeSynMap |> (`execStateT` st) |> runExcept
    resolveAll :: [TypeName] -> ReaderT (Map.Map TypeName TypeExpr) (StateT ResolutionMap (Except TypeError)) ()
    resolveAll [] = return ()
    resolveAll (n : ns) = do
      lookup' <- asks (Map.lookup n)
      case lookup' of
        Nothing -> throwError (UnknownType n)
        Just unresolved -> do
          resolutions' <- get
          resolved <- resolve resolutions' unresolved
          modify' (Map.insert n (Synonym resolved))
      resolveAll ns

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
newtype Subst = Subst (Map.Map TypeName TypeExpr) deriving (Eq, Show)

instance Semigroup Subst where
  (Subst s1) <> (Subst s2) = Subst (Map.map (subst (Subst s1)) s2 <> s1)

instance Monoid Subst where
  mempty = Subst mempty

-- Create a substitution from a single mapping
singleSubst :: TypeName -> TypeExpr -> Subst
singleSubst v t = Subst (Map.singleton v t)

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

-- The environment we use when doing type inference.
--
-- We keep a local environment of bound type names, as well
-- as the information about type synonym resolutions.
data InferEnv = InferEnv
  { bound :: Set.Set TypeName,
    resolutions :: ResolutionMap
  }

-- The context in which we perform type inference.
--
-- We have access to an environment, which we modify locally,
-- as well as a source of fresh type variables, and we can throw errors.
newtype Infer a = Infer (ReaderT InferEnv (StateT Int (Except TypeError)) a)
  deriving (Functor, Applicative, Monad, MonadReader InferEnv, MonadState Int, MonadError TypeError)

-- Run the inference context, provided we have a resolution map
runInfer :: Infer a -> ResolutionMap -> Either TypeError a
runInfer (Infer m) resolutions' =
  runReaderT m (InferEnv Set.empty resolutions')
    |> (`runStateT` 0)
    |> runExcept
    |> fmap fst

-- Generate a fresh type name during inference
fresh :: Infer TypeName
fresh =
  Infer <| do
    count <- get
    put (count + 1)
    return ("#" <> show count)

-- Instantiate a scheme by providing a fresh tyep variable for each parameter
instantiate :: SchemeExpr -> Infer TypeExpr
instantiate (SchemeExpr vars t) = do
  newVars <- forM vars (const fresh)
  let sub = foldMap (uncurry singleSubst) (zip vars (map TypeVar newVars))
  return (subst sub t)

-- Generalize a type into a scheme by closing over all unbound variables
generalize :: Set.Set TypeName -> TypeExpr -> SchemeExpr
generalize free t =
  let as = Set.toList (Set.difference (ftv t) free)
   in SchemeExpr as t

-- Modify inference with access to a bound type variable
withBound :: TypeName -> Infer a -> Infer a
withBound a = local (\r -> r {bound = Set.insert a (bound r)})

-- Represents an ordered collection about assumptions we've gathered so far
newtype Assumptions = Assumptions {assumptions :: [(Name, TypeExpr)]}
  deriving (Semigroup, Monoid)

-- Remove an assumption about a given name
removeAssumption :: Name -> Assumptions -> Assumptions
removeAssumption v (Assumptions as) = Assumptions (filter ((/= v) . fst) as)

-- An assumption about a single type
singleAssumption :: Name -> TypeExpr -> Assumptions
singleAssumption v t = Assumptions [(v, t)]

-- Extend our assumptions witha single extra binding
extendAssumptions :: Name -> TypeExpr -> Assumptions -> Assumptions
extendAssumptions v t as = singleAssumption v t <> as

-- Lookup all of the assumptions we have about a given name
lookupAssumptions :: Name -> Assumptions -> [TypeExpr]
lookupAssumptions target (Assumptions as) =
  [t | (v, t) <- as, v == target]

-- Get the set of all names used inside our assumptions
assumptionNames :: Assumptions -> Set.Set Name
assumptionNames (Assumptions as) = Set.fromList (map fst as)

builtinScheme :: Builtin -> SchemeExpr
builtinScheme Compose =
  SchemeExpr
    ["a", "b", "c"]
    ( FunctionType
        (FunctionType (TypeVar "b") (TypeVar "c"))
        ( FunctionType
            (FunctionType (TypeVar "a") (TypeVar "b"))
            (FunctionType (TypeVar "a") (FunctionType (TypeVar "b") (TypeVar "c")))
        )
    )
builtinScheme Cash =
  SchemeExpr
    ["a", "b"]
    ( FunctionType
        (FunctionType (TypeVar "a") (TypeVar "b"))
        (FunctionType (TypeVar "a") (TypeVar "b"))
    )
builtinScheme b =
  SchemeExpr [] <| case b of
    Add -> FunctionType IntType (FunctionType IntType IntType)
    Sub -> FunctionType IntType (FunctionType IntType IntType)
    Mul -> FunctionType IntType (FunctionType IntType IntType)
    Div -> FunctionType IntType (FunctionType IntType IntType)
    Concat -> FunctionType StringType (FunctionType StringType StringType)
    Less -> FunctionType IntType (FunctionType IntType BoolType)
    LessEqual -> FunctionType IntType (FunctionType IntType BoolType)
    Greater -> FunctionType IntType (FunctionType IntType BoolType)
    GreaterEqual -> FunctionType IntType (FunctionType IntType BoolType)
    EqualTo -> FunctionType IntType (FunctionType IntType BoolType)
    NotEqualTo -> FunctionType IntType (FunctionType IntType BoolType)
    And -> FunctionType BoolType (FunctionType BoolType BoolType)
    Or -> FunctionType BoolType (FunctionType BoolType BoolType)
    Negate -> FunctionType IntType IntType

inferExpr :: Expr () -> Infer (Assumptions, [Constraint], TypeExpr, Expr TypeExpr)
inferExpr expr = case expr of
  LittExpr (litt) ->
    let t = case litt of
          IntLitteral _ -> IntType
          StringLitteral _ -> StringType
          BoolLitteral _ -> BoolType
     in return (mempty, [], t, LittExpr litt)
  ApplyExpr e1 e2 -> do
    (as1, cs1, t1, e1') <- inferExpr e1
    (as2, cs2, t2, e2') <- inferExpr e2
    tv <- TypeVar <$> fresh
    let cs' = [SameType t1 (FunctionType t2 tv)] <> cs1 <> cs2
    return (as1 <> as2, cs', tv, ApplyExpr e1' e2')
  Builtin b -> do
    t <- instantiate (builtinScheme b)
    return (mempty, [], t, Builtin b)
  NameExpr n -> do
    tv <- TypeVar <$> fresh
    return (singleAssumption n tv, [], tv, NameExpr n)
  CaseExpr _ _ -> error "Can't handle case expressions yet"
  LambdaExpr n _ e -> do
    a <- fresh
    let tv = TypeVar a
    (as, cs, t, e') <- withBound a (inferExpr e)
    let inferred = FunctionType tv t
    return (removeAssumption n as, [SameType t' tv | t' <- lookupAssumptions n as] <> cs, inferred, LambdaExpr n t e')
  LetExpr defs e -> do
    (as1, cs1, t, e') <- inferExpr e
    (as2, cs2, defs') <- inferDefs as1 defs
    return (as2, cs1 <> cs2, t, LetExpr defs' e')

inferDefs :: Assumptions -> [ValueDefinition ()] -> Infer (Assumptions, [Constraint], [ValueDefinition TypeExpr])
inferDefs usageAs defs = do
  together <-
    forM defs <| \(NameDefinition n declared _ e) -> do
      (as, cs, t, e') <- inferExpr e
      let extra = case declared of
            Nothing -> []
            Just d -> [ExplicitlyInstantiates t d]
      return (as, extra ++ cs, (n, t), NameDefinition n Nothing t e')
  bound' <- asks bound
  let as = usageAs <> foldMap (\(x, _, _, _) -> x) together
      cs = foldMap (\(_, x, _, _) -> x) together
      defs' = map (\(_, _, _, def) -> def) together
      usages = map (\(_, _, usage, _) -> usage) together
      process (n, t) (as', cs') =
        (removeAssumption n as', [ImplicitlyInstantations t' bound' t | t' <- lookupAssumptions n as'] <> cs')
  let (as', cs') = foldr process (as, cs) usages
  return (as', cs', defs')

solve :: [Constraint] -> Infer Subst
solve [] = return mempty
solve constraints = solve' (nextSolvable constraints)
  where
    chooseOne :: Eq a => [a] -> [(a, [a])]
    chooseOne as = [(a, bs) | a <- as, let bs = delete a as]

    nextSolvable :: [Constraint] -> (Constraint, [Constraint])
    nextSolvable xs = case find solvable (chooseOne xs) of
      Just c -> c
      _ -> error "Couldn't find solvable constraint"
      where
        solvable (SameType _ _, _) = True
        solvable (ExplicitlyInstantiates _ _, _) = True
        solvable (ImplicitlyInstantations _ bound' t2, cs) =
          Set.null (Set.intersection (atv cs) (Set.difference (ftv t2) bound'))

    solve' :: (Constraint, [Constraint]) -> Infer Subst
    solve' (c, cs) = case c of
      SameType t1 t2 -> do
        su1 <- unify t1 t2
        su2 <- solve (map (subst su1) cs)
        return (su2 <> su1)
      ImplicitlyInstantations _ bound' t2 ->
        solve (ExplicitlyInstantiates t2 (generalize bound' t2) : cs)
      ExplicitlyInstantiates t sc -> do
        sc' <- instantiate sc
        solve (SameType t sc' : cs)

unify :: TypeExpr -> TypeExpr -> Infer Subst
unify t1 t2 | t1 == t2 = return mempty
unify (TypeVar n) t = bind n t
unify t (TypeVar n) = bind n t
unify (FunctionType t1 t2) (FunctionType t3 t4) = do
  su1 <- unify t1 t3
  su2 <- unify (subst su1 t2) (subst su1 t4)
  return (su2 <> su1)
unify (CustomType name1 ts1) (CustomType name2 ts2)
  | name1 == name2 && length ts1 == length ts2 =
    let together = zip ts1 ts2
        go acc (t1, t2) = do
          su <- unify (subst acc t1) (subst acc t2)
          return (su <> acc)
     in foldM go mempty together
unify t1 t2 = throwError (TypeMismatch t1 t2)

bind :: TypeName -> TypeExpr -> Infer Subst
bind a t
  | t == TypeVar a = return mempty
  | Set.member a (ftv t) = throwError (InfiniteType a t)
  | otherwise = return (singleSubst a t)

-- An environment mapping variables to schemes
newtype Env = Env (Map.Map TypeName SchemeExpr)

-- An empty environment containing no bindings
emptyEnv :: Env
emptyEnv = Env (Map.empty)

-- Return all of the bindings making up the environment
envBindings :: Env -> [(TypeName, SchemeExpr)]
envBindings (Env mp) = Map.toList mp

-- Get all of the variables bound in an environment
envVars :: Env -> Set.Set TypeName
envVars = envBindings >>> map fst >>> Set.fromList

data TyperInfo = TyperInfo {typerNames :: Set.Set Name, typerSub :: Subst}

newtype Typer a = Typer (Reader TyperInfo a)
  deriving (Functor, Applicative, Monad, MonadReader TyperInfo)

runTyper :: Typer a -> Subst -> a
runTyper (Typer r) sub = runReader r (TyperInfo Set.empty sub)

withTyperNames :: [Name] -> Typer a -> Typer a
withTyperNames names =
  let addTo = Set.union (Set.fromList names)
   in local (\r -> r {typerNames = addTo (typerNames r)})

schemeFor :: TypeExpr -> Typer SchemeExpr
schemeFor t = do
  (TyperInfo typerNames' typerSub') <- ask
  return (generalize typerNames' (subst typerSub' t))

typeExpr :: Expr TypeExpr -> Typer (Expr SchemeExpr)
typeExpr expr = case expr of
  LittExpr litt -> return (LittExpr litt)
  NameExpr n -> return (NameExpr n)
  Builtin b -> return (Builtin b)
  ApplyExpr e1 e2 -> ApplyExpr <$> (typeExpr e1) <*> (typeExpr e2)
  LambdaExpr n t e -> do
    sc@(SchemeExpr names _) <- schemeFor t
    e' <- withTyperNames names (typeExpr e)
    return (LambdaExpr n sc e')
  CaseExpr _ _ -> error "Can't handle cases yet"
  LetExpr defs e -> LetExpr <$> typeDefinitions defs <*> typeExpr e

typeDefinitions :: [ValueDefinition TypeExpr] -> Typer [ValueDefinition SchemeExpr]
typeDefinitions defs =
  forM defs <| \(NameDefinition name ann t e) -> do
    sc <- schemeFor t
    e' <- typeExpr e
    return (NameDefinition name ann sc e')
