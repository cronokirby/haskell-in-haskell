{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Typer (typer, TypeError) where

import Control.Monad
  ( foldM,
    forM,
    forM_,
    unless,
    when,
    zipWithM,
  )
import Control.Monad.Except
  ( Except,
    MonadError (throwError),
    liftEither,
    runExcept,
  )
import Control.Monad.Reader
  ( MonadReader (ask, local),
    ReaderT (..),
    asks,
  )
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
    execStateT,
    gets,
    modify',
  )
import Data.List (delete, find)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Ourlude
import Simplifier
  ( AST (..),
    Builtin (..),
    ConstructorDefinition (..),
    ConstructorName,
    Definition (..),
    Expr (..),
    Litteral (..),
    Name,
    Pattern (..),
    PatternDef (..),
    SchemeExpr (..),
    TypeExpr (..),
    TypeName,
    TypeVar,
    ValName,
    ValueDefinition (..),
  )

-- Map over a list monadically, then squash the results monoidally
foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
foldMapM f = mapM f >>> fmap mconcat

-- Create a function type given a return value an an ordered list of argument types
makeFunctionType :: TypeExpr -> [TypeExpr] -> TypeExpr
makeFunctionType = foldr FunctionType

-- Represents a kind of error that can happen while type checking
data TypeError
  = -- There's a mismatch between two different types
    TypeMismatch TypeExpr TypeExpr
  | -- Some type name references itself recursively
    InfiniteType TypeVar TypeExpr
  | -- An undefined name was used
    UnboundName Name
  | -- A reference to some type that doesn't exist
    UnknownType TypeName
  | -- A mismatch of a type constructor with expected vs actual args
    MismatchedTypeArgs TypeName Int Int
  | -- A type synonym ends up being cyclical
    CyclicalTypeSynonym TypeName [TypeName]
  | -- An inferred scheme is not as general as the declared one
    NotGeneralEnough SchemeExpr SchemeExpr
  deriving (Eq, Show)

asGeneral :: SchemeExpr -> SchemeExpr -> Bool
asGeneral (SchemeExpr vars1 _) (SchemeExpr vars2 _) = length vars1 >= length vars2

{- Type Information

We need to gather certain information about what type synonyms exist, so that
we can resolve type declarations later
-}

-- Gather all of the custom types, along with the number of arguments they contain
gatherCustomTypes :: [Definition t] -> Map.Map TypeName Int
gatherCustomTypes =
  foldMap <| \case
    TypeDefinition name vars _ -> Map.singleton name (length vars)
    _ -> Map.empty

-- Gather all of the type synonyms, at a superficial level
--
-- This will only look at one level of definition, and won't act recursively
gatherTypeSynonyms :: [Definition t] -> Map.Map TypeName TypeExpr
gatherTypeSynonyms =
  foldMap <| \case
    TypeSynonym name expr -> Map.singleton name expr
    _ -> Map.empty

-- Which type definitions does this type reference?
typeDependencies :: TypeExpr -> [TypeName]
typeDependencies StringType = []
typeDependencies IntType = []
typeDependencies BoolType = []
typeDependencies (TypeVar _) = []
typeDependencies (FunctionType t1 t2) = typeDependencies t1 ++ typeDependencies t2
typeDependencies (CustomType name exprs) = name : concatMap typeDependencies exprs

-- This is the state we keep track of while sorting the graph of types
data SorterState = SorterState
  { -- All of the type names we haven't seen yet
    unseen :: Set.Set TypeName,
    -- The current output we've generated so far
    output :: [TypeName]
  }

-- The context in which the topological sorting of the type graph takes place
--
-- We have access to a set of ancestors to keep track of cycles, the current state,
-- which we can modify, and the ability to throw exceptions.
type SorterM a = ReaderT (Set.Set TypeName) (StateT SorterState (Except TypeError)) a

-- Given a mapping from names to shallow types, find a linear ordering of these types
--
-- The types are sorted topologically, based on their dependencies. This means that
-- a type will come after all of its dependencies
sortTypeSynonyms :: Map.Map TypeName TypeExpr -> Either TypeError [TypeName]
sortTypeSynonyms mp = runSorter sort (SorterState (Map.keysSet mp) []) |> fmap reverse
  where
    -- Find the dependencies of a given type name
    --
    -- This acts similarly to a "neighbors" function in a traditional graph
    deps :: TypeName -> [TypeName]
    deps k = Map.findWithDefault [] k (Map.map typeDependencies mp)

    -- Run the sorter, given a seed state
    runSorter :: SorterM a -> SorterState -> Either TypeError a
    runSorter m st =
      runReaderT m Set.empty |> (`runStateT` st) |> runExcept |> fmap fst

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
      case Set.lookupMin unseen' of
        Nothing -> gets output
        Just n -> do
          dfs n
          sort

    dfs :: TypeName -> SorterM ()
    dfs name = do
      ancestors <- ask
      when
        (Set.member name ancestors)
        (throwError (CyclicalTypeSynonym name (Set.toList ancestors)))
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
  deriving (Show)

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
  Just (Synonym t)
    | null ts -> return t
    | otherwise -> throwError (MismatchedTypeArgs name 0 (length ts))
  Just (Custom arity)
    | arity == length ts -> return ct
    | otherwise -> throwError (MismatchedTypeArgs name arity (length ts))

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
    resolveAll =
      mapM_ <| \n -> do
        lookup' <- asks (Map.lookup n)
        case lookup' of
          Nothing -> throwError (UnknownType n)
          Just unresolved -> do
            resolutions' <- get
            resolved <- resolve resolutions' unresolved
            modify' (Map.insert n (Synonym resolved))

-- Represents some kind of constraint we generate during our gathering pharse.
--
-- This provides us with information about how different types are used, and is
-- necessary to be able to infer the correct types later on.
data Constraint
  = -- An assertion that two type expressions are equivalent
    SameType TypeExpr TypeExpr
  | -- An assertation that some type explicitly instantiates some scheme
    ExplicitlyInstantiates TypeExpr SchemeExpr
  | -- An assertion that some type implicitly insntatiates some type, given some bound vars
    ImplicitlyInstantations TypeExpr (Set.Set TypeVar) TypeExpr
  deriving (Eq, Show)

-- Represents a substitution of type variables for actual types
newtype Subst = Subst (Map.Map TypeVar TypeExpr) deriving (Eq, Show)

-- We can combine multiple substitutions together
instance Semigroup Subst where
  (Subst s1) <> (Subst s2) = Subst (Map.map (subst (Subst s1)) s2 <> s1)

-- There is a substitution that doesn't do anything
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
--
-- These are the type variables appearing inside of a given type
class FreeTypeVars a where
  ftv :: a -> Set.Set TypeName

instance FreeTypeVars TypeExpr where
  ftv IntType = Set.empty
  ftv StringType = Set.empty
  ftv BoolType = Set.empty
  ftv (TypeVar a) = Set.singleton a
  ftv (FunctionType t1 t2) = Set.union (ftv t1) (ftv t2)
  ftv (CustomType _ ts) = foldMap ftv ts

instance FreeTypeVars TypeName where
  ftv = Set.singleton

instance FreeTypeVars SchemeExpr where
  ftv (SchemeExpr vars t) = Set.difference (ftv t) (Set.fromList vars)

instance (Ord a, FreeTypeVars a) => FreeTypeVars (Set.Set a) where
  ftv = foldMap ftv

-- A class for types where we can detect which variables are important.
class ActiveTypeVars a where
  atv :: a -> Set.Set TypeName

instance ActiveTypeVars Constraint where
  atv (SameType t1 t2) = Set.union (ftv t1) (ftv t2)
  atv (ExplicitlyInstantiates t sc) = Set.union (ftv t) (ftv sc)
  -- What's different is that the important variables are the ones appearing
  -- in the first type, or the free variables implicitly bound on the right
  atv (ImplicitlyInstantations t1 vars t2) =
    Set.union (ftv t1) (Set.intersection (ftv vars) (ftv t2))

instance ActiveTypeVars a => ActiveTypeVars [a] where
  atv = foldMap atv

-- A map from constructor names to information about that constructor
type ConstructorInfo = Map.Map ConstructorName SchemeExpr

-- Looking at the definitions, gather information about what constructors are present
gatherConstructorInfo :: MonadError TypeError m => ResolutionMap -> [Definition t] -> m ConstructorInfo
gatherConstructorInfo resolutions' defs =
  foldMapM (extractEnv resolutions') defs
  where
    extractEnv :: (MonadError TypeError m) => ResolutionMap -> Definition t -> m ConstructorInfo
    extractEnv _ (TypeSynonym _ _) = return mempty
    extractEnv _ (ValueDefinition _) = return mempty
    extractEnv mp (TypeDefinition tn names constructors) =
      foldMapM constructorType constructors
      where
        constructorType :: (MonadError TypeError m) => ConstructorDefinition -> m ConstructorInfo
        constructorType (ConstructorDefinition n ts) = do
          resolved <- mapM (resolve mp) ts
          let t = CustomType tn (map TypeVar names)
              sc = SchemeExpr names (makeFunctionType t resolved)
          return (Map.singleton n sc)

-- The environment we use when doing type inference.
--
-- We keep a local environment of bound type names, as well
-- as the information about type synonym resolutions.
data InferEnv = InferEnv
  { bound :: Set.Set TypeName,
    resolutions :: ResolutionMap,
    constructorInfo :: ConstructorInfo
  }

-- The context in which we perform type inference.
--
-- We have access to an environment, which we modify locally,
-- as well as a source of fresh type variables, and we can throw errors.
newtype Infer a = Infer (ReaderT InferEnv (StateT Int (Except TypeError)) a)
  deriving (Functor, Applicative, Monad, MonadReader InferEnv, MonadState Int, MonadError TypeError)

-- Run the inference context, provided we have a resolution map
runInfer :: Infer a -> ResolutionMap -> ConstructorInfo -> Either TypeError a
runInfer (Infer m) resolutions' constructorInfo' =
  runReaderT m (InferEnv Set.empty resolutions' constructorInfo')
    |> (`runStateT` 0)
    |> runExcept
    |> fmap fst

-- Generate a fresh type name during inference
fresh :: Infer TypeVar
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
generalize :: Set.Set TypeVar -> TypeExpr -> SchemeExpr
generalize free t =
  let as = Set.toList (Set.difference (ftv t) free)
   in SchemeExpr as t

-- Modify inference with access to a bound type variable
withBound :: TypeVar -> Infer a -> Infer a
withBound a = local (\r -> r {bound = Set.insert a (bound r)})

withManyBound :: Set.Set TypeVar -> Infer a -> Infer a
withManyBound vars = local (\r -> r {bound = Set.union (bound r) vars})

resolveInfer :: TypeExpr -> Infer TypeExpr
resolveInfer t = do
  resolutions' <- asks resolutions
  resolve resolutions' t

lookupConstructor :: ConstructorName -> Infer SchemeExpr
lookupConstructor name = do
  result <- Map.lookup name <$> asks constructorInfo
  case result of
    Nothing -> throwError (UnboundName name)
    Just res -> return res

-- Represents an ordered collection about assumptions we've gathered so far
newtype Assumptions = Assumptions [(Name, TypeExpr)]
  deriving (Show, Semigroup, Monoid)

-- Remove an assumption about a given name
removeAssumption :: Name -> Assumptions -> Assumptions
removeAssumption v (Assumptions as) = Assumptions (filter ((/= v) . fst) as)

-- An assumption about a single type
singleAssumption :: Name -> TypeExpr -> Assumptions
singleAssumption v t = Assumptions [(v, t)]

-- Lookup all of the assumptions we have about a given name
lookupAssumptions :: Name -> Assumptions -> [TypeExpr]
lookupAssumptions target (Assumptions as) =
  [t | (v, t) <- as, v == target]

-- Get the set of all names used inside our assumptions
assumptionNames :: Assumptions -> Set.Set Name
assumptionNames (Assumptions as) = Set.fromList (map fst as)

-- Get the scheme we know a builtin name to conform to
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
    _ -> error "Already handled"

-- Get the type of a given litteral
littType :: Litteral -> TypeExpr
littType (IntLitteral _) = IntType
littType (StringLitteral _) = StringType
littType (BoolLitteral _) = BoolType

-- Run constraint generation over a given expression.
--
-- This returns the assumptions about variables we've encountered,
-- the constraints we've managed to gather, the type of the expression we've inferred,
-- and the typed version of that expression tree.
inferExpr :: Expr () -> Infer (Assumptions, [Constraint], TypeExpr, Expr TypeExpr)
inferExpr expr = case expr of
  LittExpr (litt) ->
    let t = littType litt
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
  CaseExpr e pats -> do
    (as1, cs1, t, e') <- inferExpr e
    -- We infer assumptions and constraints for each case branch, knowing the scrutinee's type
    -- We have to push down, because certain branches impose no constraints on the scrutinee,
    -- like wildcard patterns, for example
    inferred <- forM pats (inferPatternDef t)
    let pats' = map (\(_, _, _, p) -> p) inferred
        (as2, cs2) = foldMap (\(a, c, _, _) -> (a, c)) inferred
    -- We generate constraints making sure each branch has the same return type,
    -- and the same scrutinee type
    ret <- TypeVar <$> fresh
    let cs3 = map (\(_, _, branchRet, _) -> SameType ret branchRet) inferred
    return (as2 <> as1, cs3 <> cs2 <> cs1, ret, CaseExpr e' pats')
  LambdaExpr n _ e -> do
    a <- fresh
    let tv = TypeVar a
    (as, cs, t, e') <- withBound a (inferExpr e)
    let inferred = FunctionType tv t
    return
      ( removeAssumption n as,
        [SameType t' tv | t' <- lookupAssumptions n as] <> cs,
        inferred,
        LambdaExpr n tv e'
      )
  LetExpr defs e -> do
    (as1, cs1, t, e') <- inferExpr e
    (as2, cs2, defs') <- inferDefs as1 defs
    return (as2, cs1 <> cs2, t, LetExpr defs' e')

-- Run inference over a pattern definition, given the scrutinee's type
inferPatternDef :: TypeExpr -> PatternDef () -> Infer (Assumptions, [Constraint], TypeExpr, PatternDef TypeExpr)
inferPatternDef scrutinee (PatternDef pat e) = do
  tv <- TypeVar <$> fresh
  (cs1, valMap, boundSet) <- inspectPattern tv pat
  (as, cs2, t, e') <- withManyBound boundSet (inferExpr e)
  return
    ( adjustValAssumptions valMap as,
      SameType tv scrutinee : cs1 <> cs2 <> valConstraints valMap as,
      t,
      PatternDef pat e'
    )
  where
    inspectPattern :: TypeExpr -> Pattern -> Infer ([Constraint], Map.Map ValName TypeExpr, Set.Set TypeVar)
    inspectPattern scrutinee' pat' = case pat' of
      WildcardPattern -> return ([], Map.empty, Set.empty)
      NamePattern n -> return ([], Map.singleton n scrutinee', Set.empty)
      LitteralPattern litt -> return ([SameType scrutinee (littType litt)], Map.empty, Set.empty)
      ConstructorPattern name pats -> do
        patVars <- forM pats (const fresh)
        let patTypes = TypeVar <$> patVars
        (cs, valMap, boundSet) <- mconcat <$> zipWithM inspectPattern patTypes pats
        let patType = makeFunctionType scrutinee' patTypes
        constructor <- lookupConstructor name
        return (ExplicitlyInstantiates patType constructor : cs, valMap, Set.fromList patVars <> boundSet)

    adjustValAssumptions :: Map.Map ValName TypeExpr -> Assumptions -> Assumptions
    adjustValAssumptions mp as = foldr removeAssumption as (Map.keys mp)

    valConstraints :: Map.Map ValName TypeExpr -> Assumptions -> [Constraint]
    valConstraints mp as =
      foldMap (\(n, t) -> [SameType t t' | t' <- lookupAssumptions n as]) (Map.toList mp)

inferDefs :: Assumptions -> [ValueDefinition ()] -> Infer (Assumptions, [Constraint], [ValueDefinition TypeExpr])
inferDefs usageAs defs = do
  together <-
    forM defs <| \(NameDefinition n declared _ e) -> do
      (as, cs, t, e') <- inferExpr e
      extra <- case declared of
        Nothing -> return []
        Just (SchemeExpr names d) -> do
          resolved <- resolveInfer d
          return [ExplicitlyInstantiates t (SchemeExpr names resolved)]
      return (as, extra ++ cs, (n, t), NameDefinition n declared t e')
  bound' <- asks bound
  let as = usageAs <> foldMap (\(x, _, _, _) -> x) together
      cs = foldMap (\(_, x, _, _) -> x) together
      defs' = map (\(_, _, _, def) -> def) together
      usages = map (\(_, _, usage, _) -> usage) together
      process (n, t) (as', cs') =
        (removeAssumption n as', [ImplicitlyInstantations t' bound' t | t' <- lookupAssumptions n as'] <> cs')
  let (as', cs') = foldr process (as, cs) usages
  return (as', cs', defs')

-- Solve a list of constraints, by producing a valid substitution of type variables
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
      ImplicitlyInstantations t1 bound' t2 ->
        solve (ExplicitlyInstantiates t1 (generalize bound' t2) : cs)
      ExplicitlyInstantiates t sc -> do
        sc' <- instantiate sc
        solve (SameType t sc' : cs)

-- Try and unify two type expressions togethe
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

-- Try and bind a variable to a given type expression
bind :: TypeVar -> TypeExpr -> Infer Subst
bind a t
  | t == TypeVar a = return mempty
  | Set.member a (ftv t) = throwError (InfiniteType a t)
  | otherwise = return (singleSubst a t)

-- Represents the contextual information we have while typing our syntax tree
--
-- We introduce locally scoped context as we traverse the tree, introducing
-- lexically scoped type variables.
data TyperInfo = TyperInfo
  { -- The type variables we know to be bound in this ocntext
    typerVars :: Set.Set TypeVar,
    -- The substitution we have access to
    typerSub :: Subst
  }

-- The context we have while assigning types to our syntax tree
--
-- We have access to a typing context, as mentioned earlier, and we can also throw errors.
-- The main error that can occurr while typing is that the scheme we find isn't
-- as general as the one declared for a given type.
newtype Typer a = Typer (ReaderT TyperInfo (Except TypeError) a)
  deriving (Functor, Applicative, Monad, MonadReader TyperInfo, MonadError TypeError)

-- Run a typer computation, given a substitution
runTyper :: Typer a -> Subst -> Either TypeError a
runTyper (Typer r) sub = runReaderT r (TyperInfo Set.empty sub) |> runExcept

-- Introduce new type variables to run a typer computation
withTyperNames :: [TypeVar] -> Typer a -> Typer a
withTyperNames vars =
  let addTo = Set.union (Set.fromList vars)
   in local (\r -> r {typerVars = addTo (typerVars r)})

-- Get the scheme for a given type expression, using our typing context
schemeFor :: TypeExpr -> Typer SchemeExpr
schemeFor t = do
  typerVars' <- asks typerVars
  typerSub' <- asks typerSub
  return (generalize typerVars' (subst typerSub' t))

-- Assign types to a given expression
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
  CaseExpr e patDefs -> CaseExpr <$> typeExpr e <*> forM patDefs typePatternDef
  LetExpr defs e -> LetExpr <$> typeDefinitions defs <*> typeExpr e

-- Assign types to a pattern definition
typePatternDef :: PatternDef TypeExpr -> Typer (PatternDef SchemeExpr)
typePatternDef (PatternDef pat expr) = PatternDef pat <$> typeExpr expr

-- Assign types to a series of definitions
typeDefinitions :: [ValueDefinition TypeExpr] -> Typer [ValueDefinition SchemeExpr]
typeDefinitions defs =
  forM defs <| \(NameDefinition name ann t e) -> do
    sc <- schemeFor t
    e' <- typeExpr e
    case ann of
      Just d | not (asGeneral sc d) -> throwError (NotGeneralEnough sc d)
      _ -> return ()
    return (NameDefinition name ann sc e')

-- Take the value definitions out of all of the definitions we've been given
pickValueDefinitions :: [Definition t] -> [ValueDefinition t]
pickValueDefinitions = map pick >>> catMaybes
  where
    pick (ValueDefinition v) = Just v
    pick _ = Nothing

-- Infer and check the types for a series of value definitions
inferTypes :: [Definition ()] -> Infer [ValueDefinition SchemeExpr]
inferTypes defs = do
  constructors <- asks constructorInfo
  let valDefs = pickValueDefinitions defs
  (as, cs, defs') <- inferDefs mempty valDefs
  let unbound = Set.difference (assumptionNames as) (Map.keysSet constructors)
  unless (Set.null unbound) (throwError (UnboundName (Set.elemAt 0 unbound)))
  let cs' = [ExplicitlyInstantiates t s | (x, s) <- Map.toList constructors, t <- lookupAssumptions x as]
  sub <- solve (cs' <> cs)
  liftEither <| runTyper (typeDefinitions defs') sub

pluckTypeDefinitions :: [Definition a] -> [Definition b]
pluckTypeDefinitions = map go >>> catMaybes
  where
    go (ValueDefinition _) = Nothing
    go (TypeDefinition n vs cs) = Just (TypeDefinition n vs cs)
    go (TypeSynonym t1 t2) = Just (TypeSynonym t1 t2)

-- Run the type checker on a given AST, producing just the value definitions, annotated
typer :: AST () -> Either TypeError (AST SchemeExpr)
typer (AST defs) = do
  resolutions' <- createResolutions defs
  constructorInfo' <- gatherConstructorInfo resolutions' defs
  valDefs' <- runInfer (inferTypes defs) resolutions' constructorInfo'
  let allDefs = pluckTypeDefinitions defs ++ map ValueDefinition valDefs'
  return (AST allDefs)
