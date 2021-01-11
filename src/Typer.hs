{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Typer (typer, TypeError) where

import Control.Monad
  ( foldM,
    forM,
    unless,
  )
import Control.Monad.Except
  ( Except,
    MonadError (throwError),
    liftEither,
    runExcept,
  )
import Control.Monad.Reader
  ( MonadReader (local),
    ReaderT (..),
    asks,
  )
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
  )
import Data.List (delete, find, nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Ourlude
import Simplifier
  ( AST (..),
    Builtin (..),
    ConstructorInfo (..),
    ConstructorName,
    Expr (..),
    HasTypeInformation (..),
    Literal (..),
    Name,
    Pattern (..),
    TypeInformation (..),
    TypeName,
    TypeVar,
    ValName,
    ValueDefinition (..),
    lookupConstructorOrFail,
  )
import Types (FreeTypeVars (..), Scheme (..), Type (..), asGeneral)

-- Represents a kind of error that can happen while type checking
data TypeError
  = -- There's a mismatch between two different types
    TypeMismatch Type Type
  | -- Some type name references itself recursively
    InfiniteType TypeVar Type
  | -- An undefined name was used
    UnboundName Name
  | -- An inferred scheme is not as general as the declared one
    NotGeneralEnough Scheme Scheme
  deriving (Eq, Show)

-- Represents some kind of constraint we generate during our gathering pharse.
--
-- This provides us with information about how different types are used, and is
-- necessary to be able to infer the correct types later on.
data Constraint
  = -- An assertion that two type expressions are equivalent
    SameType Type Type
  | -- An assertation that some type explicitly instantiates some scheme
    ExplicitlyInstantiates Type Scheme
  | -- An assertion that some type implicitly insntatiates some type, given some bound vars
    ImplicitlyInstantations Type (Set.Set TypeVar) Type
  deriving (Eq, Show)

-- Represents a substitution of type variables for actual types
newtype Subst = Subst (Map.Map TypeVar Type) deriving (Eq, Show)

-- We can combine multiple substitutions together
instance Semigroup Subst where
  (Subst s1) <> (Subst s2) = Subst (Map.map (subst (Subst s1)) s2 <> s1)

-- There is a substitution that doesn't do anything
instance Monoid Subst where
  mempty = Subst mempty

-- Create a substitution from a single mapping
singleSubst :: TypeName -> Type -> Subst
singleSubst v t = Subst (Map.singleton v t)

-- A class for types where substitutions can be applied
class Substitutable a where
  subst :: Subst -> a -> a

instance (Ord a, Substitutable a) => Substitutable (Set.Set a) where
  subst = subst >>> Set.map

instance Substitutable TypeName where
  subst (Subst s) a = case Map.findWithDefault (TVar a) a s of
    TVar tn -> tn
    _ -> a

instance Substitutable Type where
  subst sub@(Subst s) t = case t of
    IntT -> IntT
    StringT -> StringT
    BoolT -> BoolT
    TVar a -> Map.findWithDefault (TVar a) a s
    t1 :-> t2 -> subst sub t1 :-> subst sub t2
    CustomType name ts -> CustomType name (map (subst sub) ts)

instance Substitutable Scheme where
  subst (Subst s) (Scheme vars t) =
    let s' = Subst (foldr Map.delete s vars)
     in Scheme vars (subst s' t)

instance Substitutable Constraint where
  subst s (SameType t1 t2) = SameType (subst s t1) (subst s t2)
  subst s (ExplicitlyInstantiates t sc) =
    ExplicitlyInstantiates (subst s t) (subst s sc)
  subst s (ImplicitlyInstantations t1 vars t2) =
    ImplicitlyInstantations (subst s t1) (subst s vars) (subst s t2)

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

-- The environment we use when doing type inference.
--
-- We keep a local environment of bound type names, as well
-- as the information about type synonym resolutions.
data InferEnv = InferEnv
  { bound :: Set.Set TypeName,
    typeInfo :: TypeInformation
  }

-- The context in which we perform type inference.
--
-- We have access to an environment, which we modify locally,
-- as well as a source of fresh type variables, and we can throw errors.
newtype Infer a = Infer (ReaderT InferEnv (StateT Int (Except TypeError)) a)
  deriving (Functor, Applicative, Monad, MonadReader InferEnv, MonadState Int, MonadError TypeError)

instance HasTypeInformation Infer where
  typeInformation = asks typeInfo

-- Run the inference context, provided we have a resolution map
runInfer :: Infer a -> TypeInformation -> Either TypeError a
runInfer (Infer m) info =
  runReaderT m (InferEnv Set.empty info)
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
instantiate :: Scheme -> Infer Type
instantiate (Scheme vars t) = do
  newVars <- forM vars (const fresh)
  let sub = foldMap (uncurry singleSubst) (zip vars (map TVar newVars))
  return (subst sub t)

-- Generalize a type into a scheme by closing over all unbound variables
generalize :: Set.Set TypeVar -> Type -> Scheme
generalize free t =
  let as = Set.toList (Set.difference (ftv t) free)
   in Scheme as t

-- Modify inference with access to a bound type variable
withBound :: TypeVar -> Infer a -> Infer a
withBound a = local (\r -> r {bound = Set.insert a (bound r)})

withManyBound :: Set.Set TypeVar -> Infer a -> Infer a
withManyBound vars = local (\r -> r {bound = Set.union (bound r) vars})

-- Represents an ordered collection about assumptions we've gathered so far
newtype Assumptions = Assumptions [(Name, Type)]
  deriving (Show, Semigroup, Monoid)

-- Remove an assumption about a given name
removeAssumption :: Name -> Assumptions -> Assumptions
removeAssumption v (Assumptions as) = Assumptions (filter ((/= v) . fst) as)

-- An assumption about a single type
singleAssumption :: Name -> Type -> Assumptions
singleAssumption v t = Assumptions [(v, t)]

-- Lookup all of the assumptions we have about a given name
lookupAssumptions :: Name -> Assumptions -> [Type]
lookupAssumptions target (Assumptions as) =
  [t | (v, t) <- as, v == target]

-- Get the set of all names used inside our assumptions
assumptionNames :: Assumptions -> Set.Set Name
assumptionNames (Assumptions as) = Set.fromList (map fst as)

-- Get the scheme we know a builtin name to conform to
builtinScheme :: Builtin -> Scheme
builtinScheme Compose =
  Scheme
    ["a", "b", "c"]
    ((TVar "b" :-> TVar "c") :-> (TVar "a" :-> TVar "b") :-> (TVar "a" :-> TVar "c"))
builtinScheme Cash =
  Scheme
    ["a", "b"]
    ( (TVar "a" :-> TVar "b") :-> TVar "a" :-> TVar "b"
    )
builtinScheme b =
  Scheme [] <| case b of
    Add -> IntT :-> IntT :-> IntT
    Sub -> IntT :-> IntT :-> IntT
    Mul -> IntT :-> IntT :-> IntT
    Div -> IntT :-> IntT :-> IntT
    Concat -> StringT :-> StringT :-> StringT
    Less -> IntT :-> IntT :-> BoolT
    LessEqual -> IntT :-> IntT :-> BoolT
    Greater -> IntT :-> IntT :-> BoolT
    GreaterEqual -> IntT :-> IntT :-> BoolT
    EqualTo -> IntT :-> IntT :-> BoolT
    NotEqualTo -> IntT :-> IntT :-> BoolT
    And -> BoolT :-> BoolT :-> BoolT
    Or -> BoolT :-> BoolT :-> BoolT
    Negate -> IntT :-> IntT
    _ -> error "Already handled"

-- Get the type of a given literal
littType :: Literal -> Type
littType (IntLiteral _) = IntT
littType (StringLiteral _) = StringT
littType (BoolLiteral _) = BoolT

-- Run constraint generation over a given expression.
--
-- This returns the assumptions about variables we've encountered,
-- the constraints we've managed to gather, the type of the expression we've inferred,
-- and the typed version of that expression tree.
inferExpr :: Expr () -> Infer (Assumptions, [Constraint], Type, Expr Type)
inferExpr expr = case expr of
  Error err -> do
    tv <- TVar <$> fresh
    return (mempty, [], tv, Error err)
  LitExpr litt ->
    let t = littType litt
     in return (mempty, [], t, LitExpr litt)
  ApplyExpr e1 e2 -> do
    (as1, cs1, t1, e1') <- inferExpr e1
    (as2, cs2, t2, e2') <- inferExpr e2
    tv <- TVar <$> fresh
    let cs' = [SameType t1 (t2 :-> tv)] <> cs1 <> cs2
    return (as1 <> as2, cs', tv, ApplyExpr e1' e2')
  Builtin b -> do
    t <- instantiate (builtinScheme b)
    return (mempty, [], t, Builtin b)
  NameExpr n -> do
    tv <- TVar <$> fresh
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
    ret <- TVar <$> fresh
    let cs3 = map (\(_, _, branchRet, _) -> SameType ret branchRet) inferred
    return (as2 <> as1, cs3 <> cs2 <> cs1, ret, CaseExpr e' pats')
  LambdaExpr n _ e -> do
    a <- fresh
    let tv = TVar a
    (as, cs, t, e') <- withBound a (inferExpr e)
    return
      ( removeAssumption n as,
        [SameType t' tv | t' <- lookupAssumptions n as] <> cs,
        tv :-> t,
        LambdaExpr n tv e'
      )
  LetExpr defs e -> do
    (as1, cs1, t, e') <- inferExpr e
    (as2, cs2, defs') <- inferDefs as1 defs
    return (as2, cs1 <> cs2, t, LetExpr defs' e')

-- Run inference over a pattern definition, given the scrutinee's type
inferPatternDef :: Type -> (Pattern, Expr ()) -> Infer (Assumptions, [Constraint], Type, (Pattern, Expr Type))
inferPatternDef scrutinee (pat, e) = do
  tv <- TVar <$> fresh
  (cs1, valMap, boundSet) <- inspectPattern tv pat
  (as, cs2, t, e') <- withManyBound boundSet (inferExpr e)
  return
    ( adjustValAssumptions valMap as,
      SameType tv scrutinee : cs1 <> cs2 <> valConstraints valMap as,
      t,
      (pat, e')
    )
  where
    inspectPattern :: Type -> Pattern -> Infer ([Constraint], Map.Map ValName Type, Set.Set TypeVar)
    inspectPattern scrutinee' pat' = case pat' of
      Wildcard -> return ([], Map.empty, Set.empty)
      LiteralPattern litt -> return ([SameType scrutinee (littType litt)], Map.empty, Set.empty)
      ConstructorPattern cstr pats -> do
        patVars <- forM pats (const fresh)
        let patTypes = map TVar patVars
            patType = foldr (:->) scrutinee' patTypes
            valMap = zip pats patTypes |> Map.fromList
        constructor <- constructorType <$> lookupConstructorOrFail cstr
        return ([ExplicitlyInstantiates patType constructor], valMap, Set.fromList patVars)

    adjustValAssumptions :: Map.Map ValName Type -> Assumptions -> Assumptions
    adjustValAssumptions mp as = foldr removeAssumption as (Map.keys mp)

    valConstraints :: Map.Map ValName Type -> Assumptions -> [Constraint]
    valConstraints mp as =
      foldMap (\(n, t) -> [SameType t t' | t' <- lookupAssumptions n as]) (Map.toList mp)

inferDefs :: Assumptions -> [ValueDefinition ()] -> Infer (Assumptions, [Constraint], [ValueDefinition Type])
inferDefs usageAs defs = do
  together <-
    forM defs <| \(ValueDefinition n declared _ e) -> do
      (as, cs, t, e') <- inferExpr e
      let extra = foldMap (\sc -> [ExplicitlyInstantiates t sc]) declared
      return (as, extra ++ cs, (n, t), ValueDefinition n declared t e')
  bound' <- asks bound
  let as = usageAs <> foldMap (\(x, _, _, _) -> x) together
      cs = foldMap (\(_, x, _, _) -> x) together
      defs' = map (\(_, _, _, def) -> def) together
      usages = map (\(_, _, usage, _) -> usage) together
      process (n, t) (as', cs') =
        (removeAssumption n as', [ImplicitlyInstantations t' bound' t | t' <- lookupAssumptions n as'] <> cs')
  let (as', cs') = foldr process (as, cs) usages
  return (as', cs', defs')

{- Constraint Solving -}

-- Solve a list of constraints, by producing a valid substitution of type variables
solve :: [Constraint] -> Infer Subst
solve [] = return mempty
solve constraints = solve' (nextSolvable True constraints)
  where
    chooseOne :: Eq a => [a] -> [(a, [a])]
    chooseOne as = [(a, bs) | a <- as, let bs = delete a as]

    nextSolvable :: Bool -> [Constraint] -> (Constraint, [Constraint])
    nextSolvable trySorting xs = case find solvable (chooseOne xs) of
      Just c -> c
      Nothing | trySorting -> nextSolvable False (nub xs)
      Nothing -> error ("Couldn't find solvable constraint inside of:\n" ++ show xs)
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
unify :: Type -> Type -> Infer Subst
unify t1 t2 | t1 == t2 = return mempty
unify (TVar n) t = bind n t
unify t (TVar n) = bind n t
unify (t1 :-> t2) (t3 :-> t4) = do
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
bind :: TypeVar -> Type -> Infer Subst
bind a t
  | t == TVar a = return mempty
  | Set.member a (ftv t) = throwError (InfiniteType a t)
  | otherwise = return (singleSubst a t)

{- Type Annotation -}

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
schemeFor :: Type -> Typer Scheme
schemeFor t = do
  typerVars' <- asks typerVars
  typerSub' <- asks typerSub
  return (generalize typerVars' (subst typerSub' t))

-- Assign types to a given expression
typeExpr :: Expr Type -> Typer (Expr Scheme)
typeExpr expr = case expr of
  Error err -> return (Error err)
  LitExpr litt -> return (LitExpr litt)
  NameExpr n -> return (NameExpr n)
  Builtin b -> return (Builtin b)
  ApplyExpr e1 e2 -> ApplyExpr <$> typeExpr e1 <*> typeExpr e2
  LambdaExpr n t e -> do
    sc@(Scheme names _) <- schemeFor t
    e' <- withTyperNames names (typeExpr e)
    return (LambdaExpr n sc e')
  CaseExpr e patDefs -> CaseExpr <$> typeExpr e <*> forM patDefs typePatternDef
  LetExpr defs e -> LetExpr <$> typeDefinitions defs <*> typeExpr e

-- Assign types to a pattern definition
typePatternDef :: (Pattern, Expr Type) -> Typer (Pattern, Expr Scheme)
typePatternDef (pat, expr) = (pat,) <$> typeExpr expr

-- Assign types to a series of definitions
typeDefinitions :: [ValueDefinition Type] -> Typer [ValueDefinition Scheme]
typeDefinitions defs =
  forM defs <| \(ValueDefinition name ann t e) -> do
    sc <- schemeFor t
    e' <- typeExpr e
    case ann of
      Just d | not (asGeneral sc d) -> throwError (NotGeneralEnough sc d)
      _ -> return ()
    return (ValueDefinition name ann sc e')

-- Infer and check the types for a series of value definitions
inferTypes :: [ValueDefinition ()] -> Infer [ValueDefinition Scheme]
inferTypes defs = do
  constructors <- allConstructors
  (as, cs, defs') <- inferDefs mempty defs
  let unbound = Set.difference (assumptionNames as) (Map.keysSet constructors)
  unless (Set.null unbound) (throwError (UnboundName (Set.elemAt 0 unbound)))
  let cs' = [ExplicitlyInstantiates t s | (x, s) <- Map.toList constructors, t <- lookupAssumptions x as]
  sub <- solve (cs' <> cs)
  liftEither <| runTyper (typeDefinitions defs') sub
  where
    allConstructors :: Infer (Map.Map ConstructorName Scheme)
    allConstructors = typeInformation |> fmap (constructorMap >>> Map.map constructorType)

-- Run the type checker on a given AST, producing just the value definitions, annotated
typer :: AST () -> Either TypeError (AST Scheme)
typer (AST info defs) = AST info <$> runInfer (inferTypes defs) info
