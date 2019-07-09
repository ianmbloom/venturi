{-# LANGUAGE ScopedTypeVariables #-}

module Language.Venturi.Calculus.Fold
  ( foldLc
  )
where

import Language.Venturi.Calculus
import Language.Venturi.Debug
import Outputable
import Control.Monad

foldLc :: forall m v e t a
        . (Monad m, Outputable v, Outputable e, Outputable a)
        => ((Lc v e -> m a) -> Lc v e -> m a)
        -> (a -> a -> a)
        -> a
        -> Lc v e -> m a
foldLc transform op identity term = go term
    where
    opM :: m a -> m a -> m a
    opM = liftM2 op
    go term = transform goFold term
    goFold :: Lc v e -> m a
    goFold term =
        case term of
            Lam  v body     -> go body
            Var  v          -> return identity
            App  f x        -> go f `opM` go x
            Case scrut as o -> let as' = foldl opM (return identity) (map goAlt as)
                                   scrut' = go scrut
                                   other' = maybe (return identity) go o
                               in  scrut' `opM` as' `opM` other'
            Select choice   -> return identity
            Call name body  -> go body
            Recur i         -> return identity
    goAlt :: LcAlt v e -> m a
    goAlt (Alt _ body) = go body
