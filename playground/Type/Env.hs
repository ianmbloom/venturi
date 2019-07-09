{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------
-- Adapted from original source code in Write You A Haskell
-- by Stephen Diehl
-------------------------------------------------------------------------------

module Language.Venturi.Type.Env
  ( Name(..)
  , Env(..)
  , lookupScheme
  , buildSchemes
  , extendScheme
  , removeScheme
  , empty
  ) where

import Prelude hiding (lookup)

import Language.Venturi.Term
import Language.Venturi.Type
import Language.Venturi.Binds

import Language.Venturi.Extension.List

import Data.Monoid
import Data.Foldable hiding (toList)
import qualified Data.Map as M

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

type Name = Either LName VName

data Env b = TypeEnv { schemes          :: Binds Name Scheme
                     }
                     deriving (Eq, Show)

overSchemes :: (Binds Name Scheme -> Binds Name Scheme) -> Env b -> Env b
overSchemes f env = env {schemes = f (schemes env)}

lookupScheme :: Env b -> Name -> Maybe Scheme
lookupScheme env key = reckon key (schemes env)

buildSchemes :: [(Name, Scheme)] -> Binds Name Scheme
buildSchemes tys = build tys

extendScheme :: Env b -> (Name, Scheme) -> Env b
extendScheme env (x, s) = overSchemes (flip extend (x,s)) env

removeScheme :: Env b -> Name -> Env b
removeScheme env name = overSchemes (flip remove name) env

getLetVar :: Let (I n) TyVar -> (Name, Scheme)
getLetVar (Let lName (Term tv _)) = (Left lName, Forall [] $ TVar tv)

empty :: Env b
empty = TypeEnv vacant
