{-# LANGUAGE ScopedTypeVariables #-}

module Language.Venturi.Calculus.Traverse
  ( TraverseLc(..)
  , traverseLc
  )
where

import Language.Venturi.Calculus
import Language.Venturi.Core.ToVarLc
import Language.Venturi.Debug
import Var
import Type
import Outputable
import Control.Monad.State
import Control.Monad

type TraverseLc m v e u f = Lc v e -> m (Lc u f)

traverseLc :: forall m v e u f
           .  (Monad m, Outputable v, Outputable e)
           => (v -> m u) -> (e -> m f)
           -> (TraverseLc m v e u f -> TraverseLc m v e u f)
           -> TraverseLc m v e u f
traverseLc lamF varF transform term = go term
  where
    go :: TraverseLc m v e u f
    go term = transform goTraverse term

    goTraverse :: TraverseLc m v e u f
    goTraverse term =
        case {-trP "goTraverse"-} term of
            Lam  v body     -> do body' <- go body
                                  v' <- lamF v
                                  return $ Lam v' body'
            Var  v          -> do v' <- varF v
                                  return $ Var v'
            App  f x        -> do f' <- go f
                                  x' <- go x
                                  return $ App f' x'
            Case scrut as o -> do scrut' <- go scrut
                                  as' <- mapM goAlt as
                                  o'  <- mapM go o
                                  return $ Case scrut' as' o'
            Select choice   -> return $ Select choice
            Call name body  -> do body' <- go body
                                  return $ Call name body'
            Recur i         -> return $ Recur i

    goAlt :: LcAlt v e -> m (LcAlt u f)
    goAlt (Alt choice body) = do body' <- go body
                                 return $ Alt choice body'
