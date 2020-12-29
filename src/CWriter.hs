{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module CWriter (writeC) where

import Cmm hiding (cmm)
import Control.Monad.Reader
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (intercalate)
import Ourlude

-- | A type for CCode.
--
-- We could use something more efficient than a string, but this is ok
-- for our explanation purposes
type CCode = String

-- | Represents a sequence of function names
--
-- This is a useful intermediate step to help us convert our tree of functions
-- into flat C functions.
data IdentPath = IdentPath [FunctionName] deriving (Eq, Show)

instance Semigroup IdentPath where
  IdentPath names <> IdentPath names' = IdentPath (names <> names')

instance Monoid IdentPath where
  mempty = IdentPath []

-- | Add a function name after the other identifiers
consPath :: FunctionName -> IdentPath -> IdentPath
consPath name = (IdentPath [name] <>)

-- | Display a path as a piece of C code
displayPath :: IdentPath -> CCode
displayPath (IdentPath names) =
  names |> reverse |> map convertName |> intercalate "_"
  where
    convertName :: FunctionName -> CCode
    convertName = \case
      PlainFunction name -> foldMap convertChar name
      CaseFunction index -> "case_" ++ show index
      Entry -> "_entry"
      where
        convertChar :: Char -> String
        convertChar = \case
          '$' -> "_S_"
          '\'' -> "_t_"
          '_' -> "__"
          x -> pure x

-- | The type we use to store global function information
type Globals = IntMap IdentPath

-- | The context we have access to while generating C code
data Context = Context
  { -- | The path to the current function that we're working on
    currentFunction :: IdentPath,
    -- | A map from function indices to full identifiers
    globals :: Globals
  }

-- | A context we can use at the start of our traversal
startingContext :: Context
startingContext = Context mempty mempty

-- | A computational context we use when generating C code
newtype CWriter a = CWriter (Reader Context a)
  deriving (Functor, Applicative, Monad, MonadReader Context)

-- | Run a CWriter computation, using the starting context
runCWriter :: CWriter a -> a
runCWriter (CWriter m) = runReader m startingContext

-- | Execute some computation inside of a named function
insideFunction :: FunctionName -> CWriter a -> CWriter a
insideFunction name =
  local (\r -> r {currentFunction = consPath name (currentFunction r)})

-- | Execute some computation, with access to certain globals
withGlobals :: Globals -> CWriter a -> CWriter a
withGlobals globals = local (\r -> r {globals = globals})

-- | Traverse our IR representation, gathering all global functions
--
-- We do this, since each global function has a unique index. This allows us to gather
-- all the global functions used throughout the program in advance.
gatherGlobals :: Cmm -> CWriter (IntMap IdentPath)
gatherGlobals (Cmm functions entry) = gatherInFunctions (entry : functions)
  where
    gatherInFunctions :: [Function] -> CWriter (IntMap IdentPath)
    gatherInFunctions = foldMapM gatherInFunction

    gatherInFunction :: Function -> CWriter (IntMap IdentPath)
    gatherInFunction Function {..} =
      insideFunction functionName <| do
        current <- asks currentFunction
        let thisMapping = maybe mempty (`IntMap.singleton` current) isGlobal
        thoseMappings <- gatherInFunctions subFunctions
        return (thisMapping <> thoseMappings)

writeC :: Cmm -> CCode
writeC cmm =
  let globals = runCWriter (gatherGlobals cmm)
   in show globals
