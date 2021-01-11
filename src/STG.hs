{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module STG
  ( STG (..),
    Binding (..),
    BoxType (..),
    Builtin (..),
    Atom (..),
    Literal (..),
    ValName,
    LambdaForm (..),
    Expr (..),
    Tag,
    Alts (..),
    Primitive (..),
    Updateable (..),
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
    Literal (..),
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
  = -- A literal value
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
  | -- Box some atom into a heap value
    Box BoxType Atom
  | -- Apply a fully saturated builtin to a given sequence of atoms
    Builtin Builtin [Atom]
  | -- Inspect an expression, and handle the different cases.
    --
    -- We don't take an atom here, because building up a thunk we
    -- immediately evaluate would be a bit pointless
    Case Expr [ValName] Alts
  | -- A series of bindings appearing before an expression
    Let [Binding] Expr
  deriving (Eq, Show)

data BoxType
  = IntBox
  | StringBox
  deriving (Eq, Show)

-- Represents different branches of shallow alternatives.
--
-- As opposed to patterns, alternatives only look at a single level
-- of data.
--
-- Alternatives always have a default expression.
--
-- We split alternatives for different literals, mainly to simplify
-- code generation. If we used a single alternative for all literals,
-- we'd lose information about what types are involved, making code
-- generation more cumbersome
data Alts
  = -- Potential branches for boxed integer literals, then a default expression
    IntAlts [(Int, Expr)] (Maybe Expr)
  | -- Potential branches for boxed string literals, then a default case
    StringAlts [(String, Expr)] (Maybe Expr)
  | -- Bind a primitive, based on what type it boxes into
    BindPrim BoxType ValName Expr
  | -- Unbox some primitive
    --
    -- This can only be done to use that primitive with some builtin function,
    -- this name cannot otherwise be used like a normal name!
    --
    -- If you need to pattern match against an integer or a string, IntAlts and StringAlts
    -- will do the necessary unboxing (if necessary, already)
    Unbox BoxType ValName Expr
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

class FreeNames a where
  freeNames :: a -> Set.Set ValName

instance FreeNames a => FreeNames [a] where
  freeNames = foldMap freeNames

instance FreeNames a => FreeNames (Maybe a) where
  freeNames Nothing = mempty
  freeNames (Just a) = freeNames a

instance FreeNames Atom where
  freeNames (NameAtom n) = Set.singleton n
  freeNames _ = mempty

instance FreeNames Expr where
  freeNames (Apply name atoms) = Set.singleton name <> freeNames atoms
  freeNames (Constructor _ atoms) = freeNames atoms
  freeNames (Builtin _ atoms) = freeNames atoms
  freeNames (Case e _ alts) = freeNames e <> freeNames alts
  freeNames (Let bindings e) =
    let (names, insideBindings) = foldMap (\(Binding name e') -> (Set.singleton name, freeNames e')) bindings
     in Set.difference (freeNames e <> insideBindings) names
  freeNames _ = mempty

instance FreeNames Alts where
  freeNames (IntAlts alts e) = freeNames e <> freeNames (map snd alts)
  freeNames (StringAlts alts e) = freeNames e <> freeNames (map snd alts)
  freeNames (ConstrAlts alts e) =
    let inAlts = foldMap (\((_, names), e') -> Set.difference (freeNames e') (Set.fromList names)) alts
     in freeNames e <> inAlts
  freeNames (BindPrim _ n e) = Set.delete n (freeNames e)
  freeNames (Unbox _ n e) = Set.delete n (freeNames e)

instance FreeNames LambdaForm where
  freeNames (LambdaForm _ _ names e) = Set.difference (freeNames e) (Set.fromList names)

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
    constructorInfo :: S.ConstructorMap
  }

-- The Context in which we generate STG code
--
-- We have access to a source of fresh variable names.
newtype STGM a = STGM (ReaderT STGMInfo (State Int) a)
  deriving (Functor, Applicative, Monad, MonadState Int, MonadReader STGMInfo)

instance S.HasConstructorMap STGM where
  constructorMap = asks constructorInfo


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
constructorTag name = S.lookupConstructorOrFail name |> fmap S.constructorNumber

getFreeNames :: FreeNames a => a -> STGM [ValName]
getFreeNames a =
  asks (topLevelNames >>> Set.difference (freeNames a) >>> Set.toList)

makeLambdaForm :: [ValName] -> Expr -> STGM LambdaForm
makeLambdaForm names expr = do
  free <- getFreeNames (LambdaForm [] N names expr)
  let u = updateable names expr
  return (LambdaForm free u names expr)
  where
    updateable :: [ValName] -> Expr -> Updateable
    -- If we have arguments, then we can't be evaluated further
    updateable (_ : _) _ = N
    -- Primitives can't be evaluated any further
    updateable [] (Primitive _) = N
    -- Errors are not updateable, since they exit the program
    updateable [] (Error _) = N
    updateable [] (Constructor _ _) = N
    updateable [] (Box _ _) = N
    updateable _ _ = U

makeCase :: Expr -> Alts -> STGM Expr
makeCase scrut alts = do
  free <- getFreeNames alts
  return (Case scrut free alts)

gatherApplications :: S.Expr Scheme -> (S.Expr Scheme, [S.Expr Scheme])
gatherApplications expression = go expression []
  where
    go (S.ApplyExpr f e) acc = go f (e : acc)
    go e acc = (e, acc)

literalToAtom :: Literal -> STGM ([Binding], Atom)
literalToAtom (StringLiteral s) = do
  name <- fresh
  l <- makeLambdaForm [] (Box StringBox (PrimitiveAtom (PrimString s)))
  return ([Binding name l], NameAtom name)
literalToAtom (IntLiteral i) = do
  name <- fresh
  l <- makeLambdaForm [] (Box IntBox (PrimitiveAtom (PrimInt i)))
  return ([Binding name l], NameAtom name)
literalToAtom (BoolLiteral b) = do
  name <- fresh
  l <- makeLambdaForm [] (Constructor (if b then 1 else 0) [])
  return ([Binding name l], NameAtom name)

atomize :: S.Expr Scheme -> STGM ([Binding], Atom)
atomize expression = case expression of
  S.LitExpr l -> literalToAtom l
  S.NameExpr n -> do
    wasConstructor <- S.isConstructor n
    if not wasConstructor
      then return ([], NameAtom n)
      else saturateConstructorAsAtom n
  e -> do
    name <- fresh
    l <- exprToLambda e
    return ([Binding name (removeBindingName name l)], NameAtom name)

atomToExpr :: Atom -> Expr
atomToExpr (PrimitiveAtom l) = Primitive l
atomToExpr (NameAtom n) = Apply n []

makeLet :: [Binding] -> Expr -> Expr
makeLet [] e = e
makeLet bindings e = Let bindings e

data ConstructorResult = AlreadyFull Tag [Atom] | NeededFilling [Binding] ValName

saturateConstructor :: Bool -> ConstructorName -> [Atom] -> STGM ConstructorResult
saturateConstructor alwaysSaturate name atoms = do
  (S.ConstructorInfo arity _ tag) <- S.lookupConstructorOrFail name
  let diff = arity - length atoms
  if not alwaysSaturate && diff == 0
    then return (AlreadyFull tag atoms)
    else do
      lambdaNames <- replicateM diff fresh
      let root = Constructor tag (atoms ++ map NameAtom lambdaNames)
      bindingName <- fresh
      lambda <- makeLambdaForm lambdaNames root
      return (NeededFilling [Binding bindingName (removeBindingName bindingName lambda)] bindingName)

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
  S.Add -> "$add"
  S.Sub -> "$sub"
  S.Mul -> "$mul"
  S.Div -> "$div"
  S.Concat -> "$concat"
  S.Less -> "$less"
  S.LessEqual -> "$less_equal"
  S.Greater -> "$greater"
  S.GreaterEqual -> "$greater_equal"
  S.EqualTo -> "$equal"
  S.NotEqualTo -> "$not_equal"
  S.Or -> "$or"
  S.And -> "$and"
  S.Compose -> "$compose"
  S.Cash -> "$cash"
  S.Negate -> "$neg"

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
    handle (S.LitExpr l) =
      return <| case l of
        StringLiteral s -> Box StringBox (PrimitiveAtom (PrimString s))
        IntLiteral i -> Box IntBox (PrimitiveAtom (PrimInt i))
        BoolLiteral b -> Constructor (if b then 1 else 0) []
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
  (S.LiteralPattern (S.IntLiteral _), _) -> do
    branches' <- findPatterns (\(S.LiteralPattern (S.IntLiteral i)) -> return i) branches
    default' <- findDefaultExpr branches
    makeCase scrut (IntAlts branches' default')
  (S.LiteralPattern (S.BoolLiteral _), _) -> do
    branches' <- findPatterns (\(S.LiteralPattern (S.BoolLiteral b)) -> return b) branches
    default' <- findDefaultExpr branches
    let constrBranches = map (first (\b -> (if b then 1 else 0, []))) branches'
    makeCase scrut (ConstrAlts constrBranches default')
  (S.LiteralPattern (S.StringLiteral _), _) -> do
    branches' <- findPatterns (\(S.LiteralPattern (S.StringLiteral s)) -> return s) branches
    default' <- findDefaultExpr branches
    makeCase scrut (StringAlts branches' default')
  (S.ConstructorPattern _ _, _) -> do
    branches' <- findPatterns (\(S.ConstructorPattern cstr names) -> (,names) <$> constructorTag cstr) branches
    default' <- findDefaultExpr branches
    makeCase scrut (ConstrAlts branches' default')
  (S.Wildcard, e) -> convertExpr e
  where
    findDefaultExpr :: [(S.Pattern, S.Expr Scheme)] -> STGM (Maybe Expr)
    findDefaultExpr = find (fst >>> (== S.Wildcard)) >>> traverse (snd >>> convertExpr)

    findPatterns :: (S.Pattern -> STGM a) -> [(S.Pattern, S.Expr Scheme)] -> STGM [(a, Expr)]
    findPatterns conv = takeWhile (fst >>> (/= S.Wildcard)) >>> traverse (\(pat, e) -> liftA2 (,) (conv pat) (convertExpr e))

removeBindingName :: ValName -> LambdaForm -> LambdaForm
removeBindingName name (LambdaForm free u names expr) =
  LambdaForm (filter (/= name) free) u names expr

-- Convert an expression to a lambda form
--
-- This will always create a lambda, although with no
-- arguments if the expression we're converting wasn't
-- a lambda to begin with
exprToLambda :: S.Expr Scheme -> STGM LambdaForm
exprToLambda expr = do
  let (names, e) = gatherLambdas expr
  e' <- convertExpr e
  makeLambdaForm names e'
  where
    gatherLambdas :: S.Expr Scheme -> ([ValName], S.Expr Scheme)
    gatherLambdas (S.LambdaExpr name _ e) =
      let (names, e') = gatherLambdas e
       in (name : names, e')
    gatherLambdas e = ([], e)

-- Convert a definition into an STG binding
convertDef :: ValueDefinition Scheme -> STGM Binding
convertDef (ValueDefinition name _ _ e) =
  Binding name <<< removeBindingName name <$> exprToLambda e

-- Convert the value definitions composing a program into STG
convertValueDefinitions :: [ValueDefinition Scheme] -> STGM [Binding]
convertValueDefinitions = mapM convertDef

convertAST :: AST Scheme -> STGM (Either STGError STG)
convertAST (AST _ defs) =
  find (\(S.ValueDefinition n _ _ _) -> n == "main") defs |> \case
    Nothing -> return (Left NoEntryPoint)
    Just (S.ValueDefinition n _ (Scheme [] IntT) _) -> do
      bindings <- gatherBindings
      entry <- makeEntry IntBox ExitWithInt n
      return (Right (STG bindings entry))
    Just (S.ValueDefinition n _ (Scheme [] StringT) _) -> do
      bindings <- gatherBindings
      entry <- makeEntry StringBox ExitWithString n
      return (Right (STG bindings entry))
    Just (S.ValueDefinition _ _ s _) -> return (Left (IncorrectEntryPointType s))
  where
    makeEntry boxType b n = do
      theCase <- makeCase (Apply n []) (Unbox boxType "#v" (Builtin b [NameAtom "#v"]))
      return (LambdaForm [] N [] theCase)

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
              ["$1"]
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
              ["$1"]
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
              []
              ( Unbox IntBox "#0" (makeIntBox (Builtin Negate [NameAtom "#0"]))
              )
          )
      )
  ]
  where
    rawBuiltin :: BoxType -> (Expr -> Expr) -> Builtin -> LambdaForm
    rawBuiltin boxType f b =
      LambdaForm
        []
        N
        ["$0", "$1"]
        ( Case
            (Apply "$0" [])
            ["$1"]
            ( Unbox
                boxType
                "#0"
                ( Case
                    (Apply "$1" [])
                    ["#0"]
                    (Unbox boxType "#1" (f (Builtin b [NameAtom "#0", NameAtom "#1"])))
                )
            )
        )

    makeIntBox :: Expr -> Expr
    makeIntBox e =
      Case e [] (BindPrim IntBox "#v" (Box IntBox (NameAtom "#v")))

    makeStringBox :: Expr -> Expr
    makeStringBox e =
      Case e [] (BindPrim StringBox "#v" (Box StringBox (NameAtom "#v")))

    unboxedIntBuiltin :: Builtin -> LambdaForm
    unboxedIntBuiltin = rawBuiltin IntBox makeIntBox

    unboxedStringBuiltin :: Builtin -> LambdaForm
    unboxedStringBuiltin = rawBuiltin StringBox makeStringBox

    boolBuiltin :: Builtin -> LambdaForm
    boolBuiltin =
      rawBuiltin IntBox <| \e ->
        Case
          e
          []
          ( IntAlts
              [(0, Constructor 0 []), (1, Constructor 1 [])]
              Nothing
          )
