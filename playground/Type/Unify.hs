{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Venturi.Type.Unify
  ( Unifiable(..)
  , solver
  )
where

import Language.Venturi.Type
import Language.Venturi.Type.Env
import Language.Venturi.Type.Constraint
import Language.Venturi.Type.Substitutable
import Language.Venturi.Type.Infer
import Language.Venturi.Type.Monad

import Language.Venturi.Pretty

import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MM

import Control.Monad.Except
import Control.Monad.State

import Data.Foldable
import Data.Maybe

import Language.Venturi.Debug

--------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

class Unifiable a where
  unify :: a -> a -> Infer b Subst

instance Unifiable [Type] where
  unify []       []       = return emptySubst
  unify (t1:ts1) (t2:ts2) = do su1 <- unify t1 t2
                               su2 <- unify (apply su1 ts1) (apply su1 ts2)
                               return (su2 `compose` su1)
  unify t1       t2       = throwError $ UnificationMismatch t1 t2

instance Unifiable (Maybe Type) where
  unify (Just t1) (Just t2) = unify t1 t2
  unify _         _         = return emptySubst

instance Unifiable Type where
  unify t1 t2 | t1 == t2            = return emptySubst
  unify (TVar v)      t             = v `bind` t
  unify t             (TVar v)      = v `bind` t
  unify (TAp t1 t2)   (TAp t3 t4)   = unify [t1, t2] [t3, t4]
  unify t1 t2 = throwError $ UnificationFail t1 t2

foldrM1 :: (Monad m) => (a -> a -> m a) -> [a] -> m a
foldrM1 f [x]    = return x
foldrM1 f (x:xs) = foldrM f x xs

foldlM1 :: (Monad m) => (a -> a -> m a) -> [a] -> m a
foldlM1 f [x]    = return x
foldlM1 f (x:xs) = foldlM f x xs

-- Unification solver
solver :: Unifier -> Infer b Subst
solver (su, cs) =
  case {-trace ("solver:\nsu:\n" ++ prettyCompact (ppr su) ++ "\ncs:\n" ++ prettyCompact (stack $ map ppr cs) ++ "\n") $-} cs of
    [] -> return su
    (constraint : cs0) -> do
        su1 <- case constraint of
                Unify t1 t2 -> unify t1 t2
        solver (su1 `compose` su, apply su1 cs0)

bind ::  TyVar -> Type -> Infer b Subst
bind a t | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ M.singleton a t)

occursCheck :: TyVar -> Type -> Bool
occursCheck a t = a `Set.member` ftv t
