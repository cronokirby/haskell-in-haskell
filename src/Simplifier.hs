{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Simplifier
  ( Pattern (..),
    Litteral (..),
    TypeExpr (..),
    SchemeExpr (..),
    ConstructorDefinition (..),
    SimplifierError (..),
    AST (..),
    Expr (..),
    PatternDef (..),
    ValueDefinition (..),
    Name,
    ValName,
    TypeVar,
    ConstructorName,
    TypeName,
    Builtin (..),
    ResolutionError,
    HasTypeInformation (..),
    ResolutionM (..),
    TypeInformation (..),
    ConstructorInfo (..),
    resolveM,
    lookupConstructor,
    simplifier,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.Except (Except, MonadError (..), liftEither, runExcept)
import Control.Monad.Reader (ReaderT (..), ask, asks, local)
import Control.Monad.State (StateT (..), execStateT, get, gets, modify')
import Data.Function (on)
import Data.List (foldl', groupBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Ourlude
import Parser (ConstructorDefinition (..), ConstructorName, Litteral (..), Name, Pattern (..), TypeExpr (..), TypeName, TypeVar, ValName)
import qualified Parser as P

data AST t = AST TypeInformation [ValueDefinition t] deriving (Eq, Show)

data ValueDefinition t = ValueDefinition ValName (Maybe SchemeExpr) t (Expr t) deriving (Eq, Show)

data SchemeExpr = SchemeExpr [TypeVar] TypeExpr deriving (Eq, Show)

closeTypeExpr :: TypeExpr -> SchemeExpr
closeTypeExpr t = SchemeExpr (names t) t
  where
    names StringType = []
    names IntType = []
    names BoolType = []
    names (CustomType _ typs) = typs >>= names
    names (TypeVar n) = [n]
    names (t1 :-> t2) = names t1 ++ names t2

data Expr t
  = LetExpr [ValueDefinition t] (Expr t)
  | CaseExpr (Expr t) [PatternDef t]
  | LittExpr Litteral
  | Builtin Builtin
  | NameExpr Name
  | ApplyExpr (Expr t) (Expr t)
  | LambdaExpr ValName t (Expr t)
  deriving (Eq, Show)

data Builtin
  = Add
  | Sub
  | Mul
  | Div
  | Compose
  | Concat
  | Cash
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | EqualTo
  | NotEqualTo
  | And
  | Or
  | Negate
  deriving (Eq, Show)

data PatternDef t = PatternDef Pattern (Expr t) deriving (Eq, Show)

data SimplifierError
  = -- Multiple type annotations are present for the same value
    MultipleTypeAnnotations ValName [SchemeExpr]
  | -- Different pattern lengths have been observed for the same function definition
    DifferentPatternLengths ValName [Int]
  | -- An annotation doesn't have a corresponding definition
    UnimplementedAnnotation ValName
  | -- An error that can occurr while resolving a reference to a type
    SimplifierResolution ResolutionError
  | -- A type variable is not bound in a constructor
    ConstructorUnboundTypeVar ConstructorName TypeVar
  deriving (Eq, Show)

-- An error that can occurr while resolving a reference to a type
data ResolutionError
  = -- A reference to some type that doesn't exist
    UnknownType TypeName
  | -- A mismatch of a type constructor with expected vs actual args
    MismatchedTypeArgs TypeName Int Int
  | -- A type synonym ends up being cyclical
    CyclicalTypeSynonym TypeName [TypeName]
  | -- We tried to lookup a constructor that doesn't exist
    UnknownConstructor ConstructorName
  deriving (Eq, Show)

{- Gathering Type Information -}

-- The information we have about a given constructor
data ConstructorInfo = ConstructorInfo
  { -- The arity i.e. number of arguments that the constructor takes
    --
    -- This information is in the type, but much more convenient to have readily available
    constructorArity :: Int,
    -- The type of this constructor, as a function
    constructorType :: SchemeExpr
  }
  deriving (Eq, Show)

-- A ConstructorMap is a map from constructor names to information about them
type ConstructorMap = Map.Map ConstructorName ConstructorInfo

-- Represents the information we might have when resolving a type name
data ResolvingInformation
  = -- The name is a synonym for a fully resolved expression
    Synonym TypeExpr
  | -- The name is a custom type with a certain arity
    Custom Int
  deriving (Eq, Show)

type ResolutionMap = Map.Map TypeName ResolvingInformation

-- This is a record of all information you might want to have about the types
-- that have been declared in the program.
data TypeInformation = TypeInformation
  { -- A map of all of the type synonyms, fully resolved to a given type
    resolutions :: ResolutionMap,
    -- A map from each constructor's name to the information we have about that constructor
    constructorMap :: ConstructorMap
  }
  deriving (Eq, Show)

-- A class for monadic contexts with access to type information
class Monad m => HasTypeInformation m where
  -- Access the type information available in this context
  typeInformation :: m TypeInformation

-- A class for monadic contexts in which resolution errors can be thrown
class HasTypeInformation m => ResolutionM m where
  throwResolution :: ResolutionError -> m a

resolve :: ResolutionMap -> TypeExpr -> Either ResolutionError TypeExpr
resolve _ IntType = return IntType
resolve _ StringType = return StringType
resolve _ BoolType = return BoolType
resolve _ (TypeVar a) = return (TypeVar a)
resolve mp (t1 :-> t2) = do
  t1' <- resolve mp t1
  t2' <- resolve mp t2
  return (t1' :-> t2')
resolve mp ct@(CustomType name ts) = case Map.lookup name mp of
  Nothing -> Left (UnknownType name)
  Just (Synonym t)
    | null ts -> return t
    | otherwise -> Left (MismatchedTypeArgs name 0 (length ts))
  Just (Custom arity)
    | arity == length ts -> return ct
    | otherwise -> Left (MismatchedTypeArgs name arity (length ts))

-- Resolve a type in a context where we can throw resolution errors, and have access
-- to type information
resolveM :: ResolutionM m => TypeExpr -> m TypeExpr
resolveM expr = do
  resolutions' <- resolutions <$> typeInformation
  either throwResolution return (resolve resolutions' expr)

-- Try and lookup the information about a given constructor, failing with a resolution error
-- if that constructor doesn't exist
lookupConstructor :: ResolutionM m => ConstructorName -> m ConstructorInfo
lookupConstructor name = do
  mp <- constructorMap <$> typeInformation
  let info = Map.lookup name mp
  maybe (throwResolution (UnknownConstructor name)) return info

gatherConstructorMap :: [P.Definition] -> ConstructorMap
gatherConstructorMap =
  foldMap <| \case
    P.TypeDefinition name typeVars definitions ->
      let root = CustomType name (map TypeVar typeVars)
       in foldMap (makeMap typeVars root) definitions
    _ -> Map.empty
  where
    makeMap :: [TypeVar] -> TypeExpr -> ConstructorDefinition -> ConstructorMap
    makeMap typeVars ret (P.ConstructorDefinition cstr types) =
      let arity = length types
          scheme = SchemeExpr typeVars (foldr (:->) ret types)
          info = ConstructorInfo arity scheme
       in Map.singleton cstr info

{- Resolving all of the type synonyms -}

-- Which type definitions does this type reference?
typeDependencies :: TypeExpr -> [TypeName]
typeDependencies StringType = []
typeDependencies IntType = []
typeDependencies BoolType = []
typeDependencies (TypeVar _) = []
typeDependencies (t1 :-> t2) = typeDependencies t1 ++ typeDependencies t2
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
type SorterM a = ReaderT (Set.Set TypeName) (StateT SorterState (Except ResolutionError)) a

-- Given a mapping from names to shallow types, find a linear ordering of these types
--
-- The types are sorted topologically, based on their dependencies. This means that
-- a type will come after all of its dependencies
sortTypeSynonyms :: Map.Map TypeName TypeExpr -> Either ResolutionError [TypeName]
sortTypeSynonyms mp = runSorter sort (SorterState (Map.keysSet mp) []) |> fmap reverse
  where
    -- Find the dependencies of a given type name
    --
    -- This acts similarly to a "neighbors" function in a traditional graph
    deps :: TypeName -> [TypeName]
    deps k = Map.findWithDefault [] k (Map.map typeDependencies mp)

    -- Run the sorter, given a seed state
    runSorter :: SorterM a -> SorterState -> Either ResolutionError a
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

-- Gather all of the custom types, along with the number of arguments they contain
gatherCustomTypes :: [P.Definition] -> Map.Map TypeName Int
gatherCustomTypes =
  foldMap <| \case
    P.TypeDefinition name vars _ -> Map.singleton name (length vars)
    _ -> Map.empty

-- Gather all of the type synonyms, at a superficial level
--
-- This will only look at one level of definition, and won't act recursively
gatherTypeSynonyms :: [P.Definition] -> Map.Map TypeName TypeExpr
gatherTypeSynonyms =
  foldMap <| \case
    P.TypeSynonym name expr -> Map.singleton name expr
    _ -> Map.empty

type MakeResolutionM a = ReaderT (Map.Map TypeName TypeExpr) (StateT ResolutionMap (Except ResolutionError)) a

gatherResolutions :: [P.Definition] -> Either ResolutionError ResolutionMap
gatherResolutions defs = do
  let customInfo = gatherCustomTypes defs
      typeSynMap = gatherTypeSynonyms defs
  names <- sortTypeSynonyms typeSynMap
  runResolutionM (resolveAll names) typeSynMap (Map.map Custom customInfo)
  where
    runResolutionM :: MakeResolutionM a -> Map.Map TypeName TypeExpr -> ResolutionMap -> Either ResolutionError ResolutionMap
    runResolutionM m typeSynMap st =
      runReaderT m typeSynMap |> (`execStateT` st) |> runExcept

    resolveAll :: [TypeName] -> MakeResolutionM ()
    resolveAll =
      mapM_ <| \n -> do
        lookup' <- asks (Map.lookup n)
        case lookup' of
          Nothing -> throwError (UnknownType n)
          Just unresolved -> do
            resolutions' <- get
            resolved <- liftEither (resolve resolutions' unresolved)
            modify' (Map.insert n (Synonym resolved))

-- Gather all of the type information we need from the parsed definitions
gatherTypeInformation :: [P.Definition] -> Either SimplifierError TypeInformation
gatherTypeInformation defs = do
  resolutions' <- mapLeft SimplifierResolution <| gatherResolutions defs
  let constructorMap' = gatherConstructorMap defs
  return (TypeInformation resolutions' constructorMap')

{- Converting the actual AST and Expression Tree -}

convertExpr :: P.Expr -> Either SimplifierError (Expr ())
-- We replace binary expressions with the corresponding bultin functions
convertExpr (P.BinExpr op e1 e2) = do
  let b = case op of
        P.Add -> Add
        P.Sub -> Sub
        P.Mul -> Mul
        P.Div -> Div
        P.Compose -> Compose
        P.Concat -> Concat
        P.Cash -> Cash
        P.Less -> Less
        P.LessEqual -> LessEqual
        P.Greater -> Greater
        P.GreaterEqual -> GreaterEqual
        P.EqualTo -> EqualTo
        P.NotEqualTo -> NotEqualTo
        P.And -> And
        P.Or -> Or
  e1' <- convertExpr e1
  e2' <- convertExpr e2
  return (ApplyExpr (ApplyExpr (Builtin b) e1') e2')
-- Negation is replaced by a built in function as well
convertExpr (P.NegateExpr e) = ApplyExpr (Builtin Negate) <$> convertExpr e
convertExpr (P.WhereExpr e defs) =
  convertExpr (P.LetExpr defs e)
convertExpr (P.IfExpr cond thenn elsse) = do
  cond' <- convertExpr cond
  thenn' <- convertExpr thenn
  elsse' <- convertExpr elsse
  return
    ( CaseExpr
        cond'
        [ PatternDef (LitteralPattern (BoolLitteral True)) thenn',
          PatternDef (LitteralPattern (BoolLitteral False)) elsse'
        ]
    )
convertExpr (P.NameExpr name) = Right (NameExpr name)
convertExpr (P.LittExpr litt) = Right (LittExpr litt)
convertExpr (P.LambdaExpr names body) = do
  body' <- convertExpr body
  return (foldr (`LambdaExpr` ()) body' names)
convertExpr (P.ApplyExpr f exprs) = do
  f' <- convertExpr f
  exprs' <- traverse convertExpr exprs
  return (foldl' ApplyExpr f' exprs')
convertExpr (P.CaseExpr expr patterns) = do
  let transformPat (P.PatternDef p e) = PatternDef p <$> convertExpr e
  patterns' <- traverse transformPat patterns
  expr' <- convertExpr expr
  return (CaseExpr expr' patterns')
convertExpr (P.LetExpr defs e) = do
  defs' <- convertValueDefinitions defs
  e' <- convertExpr e
  return (LetExpr defs' e')

data PatternTree = Leaf (Expr ()) | Branch [(Pattern, PatternTree)] | Empty deriving (Eq, Show)

-- Calculate the depth of a given tree of patterns
--
-- This is useful to know the number of lambda arguments we might need
treeDepth :: PatternTree -> Int
treeDepth (Leaf _) = 0
treeDepth (Branch bs) = map (snd >>> treeDepth) bs |> maximum |> (+ 1)
treeDepth _ = error "Impossible case reached in treeDepth"

-- True if a given pattern is equivalent, or captures more values than another
covers :: Pattern -> Pattern -> Bool
covers WildcardPattern WildcardPattern = True
covers WildcardPattern (NamePattern _) = True
covers WildcardPattern _ = True
covers (NamePattern _) (NamePattern _) = True
covers (NamePattern _) WildcardPattern = True
covers (NamePattern _) _ = True
covers (LitteralPattern l1) (LitteralPattern l2) | l1 == l2 = True
covers (ConstructorPattern c1 pats1) (ConstructorPattern c2 pats2) =
  c1 == c2 && length pats1 == length pats2 && all (uncurry covers) (zip pats1 pats2)
covers _ _ = False

-- This adds a full block pattern to the pattern tree
addBranches :: ([Pattern], Expr ()) -> PatternTree -> PatternTree
addBranches _ (Leaf expr) = Leaf expr
addBranches ([], expr) Empty = Leaf expr
addBranches (p : ps, expr) Empty = Branch [(p, addBranches (ps, expr) Empty)]
addBranches (p : ps, expr) (Branch bs) =
  -- We trickle down the case generation when we cover that pattern,
  -- but we only need to create a new branch if no pattern completely covers us
  let trickle (pat, tree) =
        if covers p pat
          then (pat, addBranches (ps, expr) tree)
          else (pat, tree)
      trickled = map trickle bs
      extra = (p, addBranches (ps, expr) Empty)
      bs' =
        if any (\(pat, _) -> covers pat p) bs
          then trickled
          else extra : trickled
   in Branch bs'
addBranches _ _ = error "Impossible case in addBranches"

lambdaNames :: [Name]
lambdaNames = map (\x -> "$" ++ show x) [(0 :: Integer) ..]

-- Convert a pattern tree into a correct expression.
--
-- This creates a lambda expression, and then creates the necessary
-- nested cases.
convertTree :: PatternTree -> Expr ()
convertTree tree =
  foldr
    (`LambdaExpr` ())
    (fold lambdaNames tree)
    (take (treeDepth tree) lambdaNames)
  where
    fold :: [Name] -> PatternTree -> Expr ()
    fold _ (Leaf expr) = expr
    fold (n : ns) (Branch bs) =
      let makeCase (pat, tree') = PatternDef pat (fold ns tree')
       in CaseExpr (NameExpr n) (map makeCase (reverse bs))
    fold _ _ = error "Impossible case in convertTree"

-- This converts value definitions by gathering the different patterns into a single lambda expression,
-- and adding the optional type annotation if it exists.
-- This will emit errors if any discrepencies are encountered.
convertValueDefinitions :: [P.ValueDefinition] -> Either SimplifierError [ValueDefinition ()]
convertValueDefinitions = groupBy ((==) `on` getName) >>> traverse gather
  where
    getTypeAnnotations ls =
      (catMaybes <<< (`map` ls)) <| \case
        P.TypeAnnotation _ typ -> Just typ
        _ -> Nothing
    squashPatterns :: [P.ValueDefinition] -> Either SimplifierError [([Pattern], Expr ())]
    squashPatterns ls = do
      let strip (P.NameDefinition _ pats body) = do
            body' <- convertExpr body
            return (Just (pats, body'))
          strip _ = return Nothing
      stripped <- traverse strip ls
      return (catMaybes stripped)
    getName :: P.ValueDefinition -> Name
    getName (P.TypeAnnotation name _) = name
    getName (P.NameDefinition name _ _) = name
    gather :: [P.ValueDefinition] -> Either SimplifierError (ValueDefinition ())
    gather [] = error "groupBy returned empty list"
    gather information = do
      let name = getName (head information)
          annotations = getTypeAnnotations information
      schemeExpr <- case map closeTypeExpr annotations of
        [] -> Right Nothing
        [single] -> Right (Just single)
        tooMany -> Left (MultipleTypeAnnotations name tooMany)
      pats <- squashPatterns information
      let patLengths = map (fst >>> length) pats
      case patLengths of
        [] -> Left (UnimplementedAnnotation name)
        (l : ls) | all (== l) ls -> Right ()
        ls -> Left (DifferentPatternLengths name ls)
      let tree = foldl' (flip addBranches) Empty pats
          expr = convertTree tree
      return (ValueDefinition name schemeExpr () expr)

convertDefinitions :: [P.Definition] -> Either SimplifierError [ValueDefinition ()]
convertDefinitions = map pluckValueDefinition >>> catMaybes >>> convertValueDefinitions
  where
    pluckValueDefinition :: P.Definition -> Maybe P.ValueDefinition
    pluckValueDefinition (P.ValueDefinition v) = Just v
    pluckValueDefinition _ = Nothing

simplifier :: P.AST -> Either SimplifierError (AST ())
simplifier (P.AST defs) = do
  info <- gatherTypeInformation defs
  defs' <- convertDefinitions defs
  return (AST info defs')
