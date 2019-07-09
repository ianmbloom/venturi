{-# LANGUAGE ScopedTypeVariables #-}

module Language.Venturi.Calculus.Substitute
  ( ReaderEnv(..)
  , TraverseEnv(..)
  , TravEnv(..)
  , traverseEnv
  , trackChange
  , subst
  , foldEnv
  , isRecursive

  )
where

import Prelude hiding ((<>))

import Language.Venturi.Nat
import Language.Venturi.Calculus
import Language.Venturi.Calculus.Traverse
import Language.Venturi.Calculus.Fold

import Language.Venturi.Calculus.Tools
import Language.Venturi.Core.Frill
import Language.Venturi.Debug
import Language.Venturi.Core.Pretty
import Var
import Type
import Outputable
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad
import Control.Applicative
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

data TravEnv v e = TravEnv
  { travVars   :: M.Map v (Maybe (Lc v e))
  , travCalls  :: [Name]
  }

insertVar :: Ord v => v -> Maybe (Lc v e) -> TravEnv v e -> TravEnv v e
insertVar var term travEnv = travEnv {travVars = M.insert var term (travVars travEnv)}

nextCall :: Name -> TravEnv v e -> TravEnv v e
nextCall name travEnv = travEnv {travCalls = name:(travCalls travEnv)}

trackChange :: ReaderT (TravEnv v e) (State Bool) ()
trackChange = do put True

type ReaderEnv v e = Reader (TravEnv v e)
type TraverseEnv v e = TraverseLc (ReaderT (TravEnv v e) (State Bool)) v e v e

traverseEnv :: forall v e
               .  (Ord v, Outputable v, Outputable e)
               => TraverseEnv v e
               -> Lc v e -> (Lc v e, Bool)
traverseEnv transform term =
    runState (runReaderT (traverseLc return return go term) (TravEnv M.empty [])) False
    where
    go :: TraverseEnv v e -> TraverseEnv v e
    go goTraverse term =
        transform term >>=
        (\term -> case term of
             Lam v body     -> local (insertVar v Nothing) $ goTraverse term
             Call name body -> local (nextCall name) $ goTraverse term
             _ ->  goTraverse term
        )

foldEnv :: forall v e a
        .  (Ord v, Outputable v, Outputable e, Outputable a)
        => ((Lc v e -> ReaderEnv v e a) ->  Lc v e -> ReaderEnv v e a)
        -> (a -> a -> a)
        -> a
        -> Lc v e
        -> a
foldEnv transform op identity term =
    runReader (foldLc goEnv op identity term) (TravEnv M.empty [])
    where
    goEnv :: (Lc v e -> ReaderEnv v e a) -> Lc v e -> ReaderEnv v e a
    goEnv goFold term =
      do  case term of
              Lam v     body -> local (insertVar v Nothing) $ transform goFold term
              Call name body -> local (nextCall name) $ transform goFold term
              _ -> transform goFold term

subst :: forall v e
      .  (LcVar v e, Outputable v, Outputable e)
      => v
      -> (Lc v e -> Lc v e)
      ->  Lc v e -> Lc v e
subst match op term =
    runIdentity $ traverseLc return return subVar term
    where
    subVar :: TraverseLc Identity v e v e -> TraverseLc Identity v e v e
    subVar goTraverse term =
        case term of
            Var v ->
                do  if match `matchVar` v
                    then return $ op term
                    else goTraverse term
            _ -> goTraverse term

isRecursive :: (Ord v, Outputable v, Outputable e)
            => Lc v e -> Bool
isRecursive term = foldEnv checkRecur (||) False term
  where
    checkRecur :: (Lc v e -> ReaderEnv v e Bool) -> Lc v e -> ReaderEnv v e Bool
    checkRecur goFold term =
      case term of
          Recur i -> do calls <- travCalls <$> ask
                        if i == natLength calls
                        then return True
                        else return False
          _ -> goFold term
