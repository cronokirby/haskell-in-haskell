{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module STG
  ( STG (..),
    Binding (..),
    Atom (..),
    Litteral (..),
    ValName,
    LambdaForm (..),
    Expr(..),
    Alts(..),
    stg,
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Reader
import Control.Monad.State
import Data.List (find)
import qualified Data.Set as Set
import Ourlude
import Simplifier
  ( AST (..),
    ConstructorName,
    Litteral (..),
    ValName,
    ValueDefinition (..),
  )
import qualified Simplifier as S
import Types (Scheme (..), Type (..))

-- Represents an actual primitive value
data Primitive
  = -- A primitive int value
    PrimInt Int
  | -- A primitive string value
    PrimString String
  deriving (Eq, Show)

-- Primitive builtin operations
--
-- This differs from our previous built-ins, in the sense that
-- these operate *exclusively* on primitive operations.
data Builtin
  = Add
  | Sub
  | Mul
  | Div
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | EqualTo
  | NotEqualTo
  | Negate
  | Concat
  | ExitWithInt
  | ExitWithString
  deriving (Eq, Show)

-- Represents a unit of data simple enough to be passed directly
--
-- With STG, we want to limit the complexity of expressions that appear
-- inside other applications, to make compilation much easier.
data Atom
  = -- A litteral value
    PrimitiveAtom Primitive
  | -- A reference to a name
    NameAtom ValName
  deriving (Eq, Show)

-- Represents a tag for an alternative of an algebraic data type
type Tag = Int

-- Represents an expression in our STG language
data Expr
  = -- A primitive value
    Primitive Primitive
  | -- Apply a name (function) to a potentially empty sequence of atoms
    Apply ValName [Atom]
  | -- Panic with a given error
    --
    -- This is something we introduce here, to be able to provide an expression
    -- for the default branch of a case
    Error String
  | -- Apply an ADT constructor to a sequence of atoms
    --
    -- The constructor must be fully saturated, i.e. its arity
    -- should match the number of atoms here.
    Constructor Tag [Atom]
  | -- Apply a fully saturated builtin to a given sequence of atoms
    Builtin Builtin [Atom]
  | -- Inspect an expression, and handle the different cases.
    --
    -- We don't take an atom here, because building up a thunk we
    -- immediately evaluate would be a bit pointless
    Case Expr Alts
  | -- A series of bidnings appearing before an expression
    Let [Binding] Expr
  deriving (Eq, Show)

-- Represents different branches of shallow alternatives.
--
-- As opposed to patterns, alternatives only look at a single level
-- of data.
--
-- Alternatives always have a default expression.
--
-- We split alternatives for different litterals, mainly to simplify
-- code generation. If we used a single alternative for all litterals,
-- we'd lose information about what types are involved, making code
-- generation more cumbersome
data Alts
  = -- Potential branches for integer litterals, then a default expression
    IntAlts [(Int, Expr)] (Maybe Expr)
  | -- Potential branches for string litterals, then a default case
    StringAlts [(String, Expr)] (Maybe Expr)
  | -- Match a primitive integer against a value
    IntPrim ValName Expr
  | -- Match a primitive string against a value
    StringPrim ValName Expr
  | -- Potential branches for constructor tags, introducing names,
    -- and then we end, as usual, with a default case
    ConstrAlts [((Tag, [ValName]), Expr)] (Maybe Expr)
  deriving (Eq, Show)

-- A flag telling us when a thunk is updateable
--
-- By default, all thunks are updateable, and marking thunks
-- as non-updateable is an optimization for certain thinks
-- which we know to already be fully evaluated, or which aren't
-- used more than once.
data Updateable = N | U deriving (Eq, Show)

-- Represents a lambda expression
--
-- We first have a list of free variables occurring in the body,
-- the updateable flag, then the list of parameters, and then the body
data LambdaForm = LambdaForm [ValName] Updateable [ValName] Expr deriving (Eq, Show)

-- Represents a binding from a name to a lambda form
--
-- In STG, bindings always go through lambda forms, representing
-- the connection with heap allocated thunks.
data Binding = Binding ValName LambdaForm deriving (Eq, Show)

-- Represents an STG program, which is just a list of top level bindings, and an entry
data STG = STG [Binding] LambdaForm deriving (Eq, Show)

-- The kind of error that can happen when generating STG
data STGError
  = NoEntryPoint
  | IncorrectEntryPointType Scheme
  deriving (Eq, Show)

-- The information we have access to when compiling to STG
data STGMInfo = STGMInfo
  { -- The names we know to appear at the top level
    topLevelNames :: Set.Set ValName,
    -- The information about types and constructors
    typeInfo :: S.TypeInformation
  }

-- The Context in which we generate STG code
--
-- We have access to a source of fresh variable names.
newtype STGM a = STGM (ReaderT STGMInfo (State Int) a)
  deriving (Functor, Applicative, Monad, MonadState Int, MonadReader STGMInfo)

instance S.HasTypeInformation STGM where
  typeInformation = asks typeInfo

-- All resolution errors will have been caught in the type checker
instance S.ResolutionM STGM where
  throwResolution err = error ("Resolution Error in STG: " ++ show err)

runSTGM :: STGM a -> STGMInfo -> a
runSTGM (STGM m) info =
  m |> (`runReaderT` info) |> (`runState` 0) |> fst

-- Create a fresh name
fresh :: STGM ValName
fresh = do
  x <- get
  put (x + 1)
  return ("$$" ++ show x)

-- Lookup the tag associated with a constructor's name
constructorTag :: ConstructorName -> STGM Tag
constructorTag name = S.lookupConstructor name |> fmap S.constructorNumber

gatherApplications :: S.Expr Scheme -> (S.Expr Scheme, [S.Expr Scheme])
gatherApplications expression = go expression []
  where
    go (S.ApplyExpr f e) acc = go f (e : acc)
    go e acc = (e, acc)

litteralToAtom :: Litteral -> STGM ([Binding], Atom)
litteralToAtom (StringLitteral s) = do
  name <- fresh
  let l = LambdaForm [] U [] (Constructor 0 [PrimitiveAtom (PrimString s)])
  return ([Binding name l], NameAtom name)
litteralToAtom (IntLitteral i) = do
  name <- fresh
  let l = LambdaForm [] U [] (Constructor 0 [PrimitiveAtom (PrimInt i)])
  return ([Binding name l], NameAtom name)
litteralToAtom (BoolLitteral b) = do
  name <- fresh
  let l = LambdaForm [] N [] (Constructor (if b then 1 else 0) [])
  return ([Binding name l], NameAtom name)

atomize :: S.Expr Scheme -> STGM ([Binding], Atom)
atomize expression = case expression of
  S.LittExpr l -> litteralToAtom l
  S.NameExpr n -> do
    wasConstructor <- S.isConstructor n
    if not wasConstructor
      then return ([], NameAtom n)
      else saturateConstructorAsAtom n
  e -> do
    name <- fresh
    l <- exprToLambda e
    return ([Binding name l], NameAtom name)

atomToExpr :: Atom -> Expr
atomToExpr (PrimitiveAtom l) = Primitive l
atomToExpr (NameAtom n) = Apply n []

makeLet :: [Binding] -> Expr -> Expr
makeLet [] e = e
makeLet bindings e = Let bindings e

data ConstructorResult = AlreadyFull Tag [Atom] | NeededFilling [Binding] ValName

saturateConstructor :: Bool -> ConstructorName -> [Atom] -> STGM ConstructorResult
saturateConstructor alwaysSaturate name atoms = do
  (S.ConstructorInfo arity _ tag) <- S.lookupConstructor name
  let diff = arity - length atoms
  if not alwaysSaturate && diff == 0
    then return (AlreadyFull tag atoms)
    else do
      lambdaNames <- replicateM diff fresh
      let root = Constructor tag (atoms ++ map NameAtom lambdaNames)
      lambda <- attachFreeNames (LambdaForm [] N lambdaNames root)
      bindingName <- fresh
      return (NeededFilling [Binding bindingName lambda] bindingName)

saturateConstructorAsExpr :: ConstructorName -> [Atom] -> [Binding] -> STGM Expr
saturateConstructorAsExpr name atoms bindings =
  convert <$> saturateConstructor False name atoms
  where
    convert (AlreadyFull tag as) = makeLet bindings (Constructor tag as)
    convert (NeededFilling newBindings n) = makeLet (bindings <> newBindings) (Apply n [])

saturateConstructorAsAtom :: ConstructorName -> STGM ([Binding], Atom)
saturateConstructorAsAtom name =
  (\(NeededFilling b n) -> (b, NameAtom n)) <$> saturateConstructor True name []

builtinName :: S.Builtin -> ValName
builtinName = \case
  S.Add -> "prim_+"
  S.Sub -> "prim_-"
  S.Mul -> "prim_*"
  S.Div -> "prim_/"
  S.Concat -> "prim_++"
  S.Less -> "prim_<"
  S.LessEqual -> "prim_<="
  S.Greater -> "prim_>"
  S.GreaterEqual -> "prim_>="
  S.EqualTo -> "prim_=="
  S.NotEqualTo -> "prim_/="
  S.Or -> "prim_||"
  S.And -> "prim_&&"
  S.Compose -> "prim_."
  S.Cash -> "prim_$"
  S.Negate -> "prim_neg"

-- Convert an expression into an STG expression
convertExpr :: S.Expr Scheme -> STGM Expr
convertExpr =
  gatherApplications >>> \case
    (e, []) -> handle e
    (f, args) -> case f of
      -- Builtins, which are all operators, will be fully saturated from a parsing perspective
      S.Builtin b -> do
        (bindings, atoms) <- gatherAtoms args
        return (makeLet bindings (Apply (builtinName b) atoms))
      S.NameExpr n -> do
        (bindings, atoms) <- gatherAtoms args
        wasConstructor <- S.isConstructor n
        if not wasConstructor
          then return (makeLet bindings (Apply n atoms))
          else saturateConstructorAsExpr n atoms bindings
      e -> do
        (argBindings, atoms) <- gatherAtoms args
        (eBindings, atom) <- atomize e
        return <| case atom of
          PrimitiveAtom _ -> error "Primitives cannot be functions"
          NameAtom n -> makeLet (argBindings ++ eBindings) (Apply n atoms)
  where
    handle :: S.Expr Scheme -> STGM Expr
    handle (S.LittExpr l) =
      return <| case l of
        StringLitteral s -> Constructor 0 [PrimitiveAtom (PrimString s)]
        IntLitteral i -> Constructor 0 [PrimitiveAtom (PrimInt i)]
        BoolLitteral b -> Constructor (if b then 1 else 0) []
    handle (S.NameExpr n) = do
      wasConstructor <- S.isConstructor n
      if not wasConstructor
        then return (Apply n [])
        else saturateConstructorAsExpr n [] []
    handle (S.Error s) = return (Error s)
    handle (S.LetExpr defs e) = do
      defs' <- convertValueDefinitions defs
      e' <- convertExpr e
      return (makeLet defs' e')
    handle lambda@S.LambdaExpr {} = do
      (bindings, atom) <- atomize lambda
      return (makeLet bindings (atomToExpr atom))
    handle S.ApplyExpr {} = error "Apply Expressions shouldn't appear here"
    handle S.Builtin {} = error "Unary builtin operation"
    handle (S.CaseExpr _ []) = return (Error "Empty Case Expression")
    handle (S.CaseExpr e branches) = convertExpr e >>= convertBranches branches

    gatherAtoms :: [S.Expr Scheme] -> STGM ([Binding], [Atom])
    gatherAtoms = mapM atomize >>> fmap gatherBindings

    gatherBindings :: [([b], a)] -> ([b], [a])
    gatherBindings l = (concatMap fst l, map snd l)

convertBranches :: [(S.Pattern, S.Expr Scheme)] -> Expr -> STGM Expr
convertBranches branches scrut = case head branches of
  (S.LitteralPattern (S.IntLitteral _), _) -> do
    branches' <- findPatterns (\(S.LitteralPattern (S.IntLitteral i)) -> return i) branches
    default' <- findDefaultExpr branches
    boundName <- fresh
    let primCase = Case (Apply boundName []) (IntAlts branches' default')
    return (Case scrut (ConstrAlts [((0, [boundName]), primCase)] Nothing))
  (S.LitteralPattern (S.BoolLitteral _), _) -> do
    branches' <- findPatterns (\(S.LitteralPattern (S.BoolLitteral b)) -> return b) branches
    default' <- findDefaultExpr branches
    let constrBranches = map (first (\b -> (if b then 1 else 0, []))) branches'
    return (Case scrut (ConstrAlts constrBranches default'))
  (S.LitteralPattern (S.StringLitteral _), _) -> do
    branches' <- findPatterns (\(S.LitteralPattern (S.StringLitteral s)) -> return s) branches
    default' <- findDefaultExpr branches
    boundName <- fresh
    let primCase = Case (Apply boundName []) (StringAlts branches' default')
    return (Case scrut (ConstrAlts [((0, [boundName]), primCase)] Nothing))
  (S.ConstructorPattern _ _, _) -> do
    branches' <- findPatterns (\(S.ConstructorPattern cstr names) -> (,names) <$> constructorTag cstr) branches
    default' <- findDefaultExpr branches
    return (Case scrut (ConstrAlts branches' default'))
  (S.Wildcard, e) -> convertExpr e
  where
    findDefaultExpr :: [(S.Pattern, S.Expr Scheme)] -> STGM (Maybe Expr)
    findDefaultExpr = find (fst >>> (== S.Wildcard)) >>> traverse (snd >>> convertExpr)

    findPatterns :: (S.Pattern -> STGM a) -> [(S.Pattern, S.Expr Scheme)] -> STGM [(a, Expr)]
    findPatterns conv = takeWhile (fst >>> (/= S.Wildcard)) >>> traverse (\(pat, e) -> liftA2 (,) (conv pat) (convertExpr e))

-- Gather the free names appearing in an expression
attachFreeNames :: LambdaForm -> STGM LambdaForm
attachFreeNames lambda@(LambdaForm _ u names expr) = do
  topLevel <- asks topLevelNames
  let free = Set.difference (inLambda lambda) topLevel |> Set.toList
  return (LambdaForm free (if null names then u else N) names expr)
  where
    inLambda :: LambdaForm -> Set.Set ValName
    inLambda (LambdaForm _ _ ns e) =
      Set.difference (inExpr e) (Set.fromList ns)

    inExpr :: Expr -> Set.Set ValName
    inExpr (Apply n atoms) = Set.singleton n <> foldMap inAtom atoms
    inExpr (Constructor _ atoms) = foldMap inAtom atoms
    inExpr (Builtin _ atoms) = foldMap inAtom atoms
    inExpr (Let bindings e) =
      let free = inExpr e <> foldMap inBinding bindings
       in Set.difference free (bindingNames bindings)
    inExpr (Case scrut alts) = inExpr scrut <> inAlts alts
    inExpr _ = Set.empty

    inAlts :: Alts -> Set.Set ValName
    inAlts (IntAlts branches def) = foldMap (snd >>> inExpr) branches <> foldMap inExpr def
    inAlts (StringAlts branches def) = foldMap (snd >>> inExpr) branches <> foldMap inExpr def
    inAlts (ConstrAlts branches def) = foldMap inCstr branches <> foldMap inExpr def
    inAlts (IntPrim boundName expr') = Set.delete boundName (inExpr expr')
    inAlts (StringPrim boundName expr') = Set.delete boundName (inExpr expr')

    inCstr :: ((Tag, [ValName]), Expr) -> Set.Set ValName
    inCstr ((_, names'), e) = Set.difference (inExpr e) (Set.fromList names')

    inAtom :: Atom -> Set.Set ValName
    inAtom PrimitiveAtom {} = Set.empty
    inAtom (NameAtom n) = Set.singleton n

    bindingNames :: [Binding] -> Set.Set ValName
    bindingNames = map (\(Binding n _) -> n) >>> Set.fromList

    inBinding :: Binding -> Set.Set ValName
    inBinding (Binding n lf) =
      Set.delete n (inLambda lf)

-- Convert an expression to a lambda form
--
-- This will always create a lambda, although with no
-- arguments if the expression we're converting wasn't
-- a lambda to begin with
exprToLambda :: S.Expr Scheme -> STGM LambdaForm
exprToLambda expr = do
  let (names, e) = gatherLambdas expr
  e' <- convertExpr e
  attachFreeNames (LambdaForm [] U names e')
  where
    gatherLambdas :: S.Expr Scheme -> ([ValName], S.Expr Scheme)
    gatherLambdas (S.LambdaExpr name _ e) =
      let (names, e') = gatherLambdas e
       in (name : names, e')
    gatherLambdas e = ([], e)

-- Convert a definition into an STG binding
convertDef :: ValueDefinition Scheme -> STGM Binding
convertDef (ValueDefinition name _ _ e) =
  Binding name <$> exprToLambda e

-- Convert the value definitions composing a program into STG
convertValueDefinitions :: [ValueDefinition Scheme] -> STGM [Binding]
convertValueDefinitions = mapM convertDef

convertAST :: AST Scheme -> STGM (Either STGError STG)
convertAST (AST _ defs) =
  find (\(S.ValueDefinition n _ _ _) -> n == "main") defs |> \case
    Nothing -> return (Left NoEntryPoint)
    Just (S.ValueDefinition n _ (Scheme [] IntT) _) -> do
      bindings <- gatherBindings
      return (Right (STG bindings (entry ExitWithInt n)))
    Just (S.ValueDefinition n _ (Scheme [] StringT) _) -> do
      bindings <- gatherBindings
      return (Right (STG bindings (entry ExitWithString n)))
    Just (S.ValueDefinition _ _ s _) -> return (Left (IncorrectEntryPointType s))
  where
    entry b n = LambdaForm [] U [] (Case (Apply n []) (ConstrAlts [((0, ["#v"]), Builtin b [NameAtom "#v"])] Nothing))

    gatherBindings =
      defs |> convertValueDefinitions |> fmap (builtins ++)

gatherInformation :: AST Scheme -> STGMInfo
gatherInformation (AST info defs) =
  let topLevel = gatherTopLevel defs |> Set.fromList
   in STGMInfo (builtinNames <> topLevel) info
  where
    gatherTopLevel :: [S.ValueDefinition t] -> [ValName]
    gatherTopLevel = map (\(S.ValueDefinition n _ _ _) -> n)

builtinNames :: Set.Set ValName
builtinNames = builtins |> map (\(Binding n _) -> n) |> Set.fromList

-- Run the STG compilation step
stg :: AST Scheme -> Either STGError STG
stg ast =
  let info = gatherInformation ast
   in runSTGM (convertAST ast) info

-- The builtin operations we've created in this language
builtins :: [Binding]
builtins =
  [ Binding "$add" (unboxedIntBuiltin Add),
    Binding "$sub" (unboxedIntBuiltin Sub),
    Binding "$mul" (unboxedIntBuiltin Mul),
    Binding "$div" (unboxedIntBuiltin Div),
    Binding "$concat" (unboxedStringBuiltin Concat),
    Binding "$less" (boolBuiltin Less),
    Binding "$less_equal" (boolBuiltin LessEqual),
    Binding "$greater" (boolBuiltin Greater),
    Binding "$greater_equal" (boolBuiltin GreaterEqual),
    Binding "$equal" (boolBuiltin EqualTo),
    Binding "$not_equal" (boolBuiltin NotEqualTo),
    Binding
      "$or"
      ( LambdaForm
          []
          N
          ["$0", "$1"]
          ( Case
              (Apply "$0" [])
              ( ConstrAlts
                  [ ((1, []), Constructor 1 []),
                    ((0, []), Apply "$1" [])
                  ]
                  Nothing
              )
          )
      ),
    Binding
      "$and"
      ( LambdaForm
          []
          N
          ["$0", "$1"]
          ( Case
              (Apply "$0" [])
              ( ConstrAlts
                  [ ((0, []), Constructor 0 []),
                    ((1, []), Apply "$1" [])
                  ]
                  Nothing
              )
          )
      ),
    Binding
      "$compose"
      ( LambdaForm
          []
          N
          ["$0", "$1", "$2"]
          ( Let
              [ Binding
                  "$3"
                  ( LambdaForm ["$1", "$2"] U [] (Apply "$1" [NameAtom "$2"])
                  )
              ]
              (Apply "$0" [NameAtom "$3"])
          )
      ),
    Binding "$cash" (LambdaForm [] N ["$0", "$1"] (Apply "$0" [NameAtom "$1"])),
    Binding
      "$neg"
      ( LambdaForm
          []
          N
          ["$0"]
          ( Case
              (Apply "$0" [])
              ( ConstrAlts
                  [ ( (0, ["#0"]),
                      makeIntBox (Builtin Negate [NameAtom "#0"])
                    )
                  ]
                  Nothing
              )
          )
      )
  ]
  where
    rawBuiltin :: (Expr -> Expr) -> Builtin -> LambdaForm
    rawBuiltin f b =
      LambdaForm
        []
        N
        ["$0", "$1"]
        ( Case
            (Apply "$0" [])
            ( ConstrAlts
                [ ( (0, ["#0"]),
                    Case
                      (Apply "$1" [])
                      ( ConstrAlts
                          [ ( (0, ["#1"]),
                              f (Builtin b [NameAtom "#0", NameAtom "#1"])
                            )
                          ]
                          Nothing
                      )
                  )
                ]
                Nothing
            )
        )

    makeIntBox :: Expr -> Expr
    makeIntBox e =
      Case e (IntPrim "#v" (Constructor 0 [NameAtom "#v"]))

    makeStringBox :: Expr -> Expr
    makeStringBox e =
      Case e (IntPrim "#v" (Constructor 0 [NameAtom "#v"]))

    unboxedIntBuiltin :: Builtin -> LambdaForm
    unboxedIntBuiltin = rawBuiltin makeIntBox

    unboxedStringBuiltin :: Builtin -> LambdaForm
    unboxedStringBuiltin = rawBuiltin makeStringBox

    boolBuiltin :: Builtin -> LambdaForm
    boolBuiltin =
      rawBuiltin <| \e ->
        Case
          e
          ( IntAlts
              [(0, Constructor 0 []), (1, Constructor 1 [])]
              Nothing
          )
