{-# LANGUAGE ScopedTypeVariables #-}

module Language.Venturi.Calculus.Reduce
  ( TraverseSpine(..)
  , SpineState(..)
  , traverseSpine
  , reduce
  , reduceWith
  , putSpine
  )
where

import Language.Venturi.Calculus
import Language.Venturi.Calculus.Pretty
import Language.Venturi.Core.Pretty
import Language.Venturi.Calculus.Tools
import Language.Venturi.Calculus.Traverse
import Language.Venturi.Calculus.Fold
import Language.Venturi.Calculus.Substitute
import Language.Venturi.Debug
import Control.Monad.State
import Control.Monad.Identity
import Outputable

data SpineState v e = Ss
  { sSpine :: [Lc v e]
  , sChangeOccured :: Bool
  }

type TraverseSpine v e = TraverseLc (State (SpineState v e)) v e v e

rebuildSpine :: (Outputable v, Outputable e, Monad m)
             => (Lc v e -> StateT (SpineState v e) m (Lc v e))
rebuildSpine f = do (Ss spine change) <- get
                    put (Ss [] change)
                    return $ mkApps f spine

noSpine :: (Monad m, Outputable v, Outputable e)
        => StateT (SpineState v e) m (Lc v e)
        -> StateT (SpineState v e) m (Lc v e)
noSpine f = do (Ss spine change) <- get
               put (Ss [] change)
               f' <- f
               (Ss _ subChange) <- get
               put (Ss spine (change || subChange))
               return f'

trackChange :: (Monad m, Outputable v, Outputable e)
            => StateT (SpineState v e) m ()
trackChange = do (Ss spine change) <- get
                 put (Ss spine True)
                 return ()

pushSpine :: forall v e m
          .  (Monad m, Outputable v, Outputable e)
          => Lc v e
          -> StateT (SpineState v e) m (Lc v e)
          -> StateT (SpineState v e) m (Lc v e)
pushSpine v f = do (Ss spine change) <- get
                   put (Ss (v:spine) change)
                   f

putSpine :: forall v e m
         .  (Monad m, Outputable v, Outputable e)
         => [Lc v e]
         -> StateT (SpineState v e) m (Lc v e)
         -> StateT (SpineState v e) m (Lc v e)
putSpine spine f = do (Ss _ change) <- get
                      put (Ss spine change)
                      f

traverseSpine :: forall v e
              .  (Outputable v, Outputable e)
              => (TraverseSpine v e -> TraverseSpine v e)
              -> Lc v e -> (Lc v e, Bool)
traverseSpine transform term =
    let (term', Ss spine change) = runState (go term) (Ss [] False)
    in  (term', change)
    where
    go :: TraverseSpine v e
    go term = transform goTraverse term

    goTraverse :: TraverseSpine v e
    goTraverse term =
        case term of
            Lam  v body     -> do body' <- noSpine (go body)
                                  return $ Lam v body'
            Var  v          -> do return $ Var v
            App  f x        -> do x' <- noSpine (go x)
                                  f' <- pushSpine x' (go f)
                                  rebuildSpine f'
            Case scrut as o -> do scrut' <- noSpine (go scrut)
                                  as'    <- mapM goAlt as
                                  o'     <- mapM (noSpine . go) o
                                  return $ Case scrut' as' o'
            Select choice   -> return $ Select choice
            Call name body  -> do body' <- noSpine (go body)
                                  return $ Call name body'
            Recur i         -> return $ Recur i

    goAlt :: LcAlt v e -> State (SpineState v e) (LcAlt v e)
    goAlt (Alt choice body) = do body' <- go body
                                 return $ Alt choice body'

reduce :: forall v e
       .  (Outputable v, Outputable e, LcVar v e)
       => Lc v e -> Lc v e
reduce = fst . reduceWith isVar

reduceWith :: forall v e
           .  (Outputable v, Outputable e, LcVar v e)
           => (Lc v e -> Bool) -> Lc v e -> (Lc v e, Bool)
reduceWith isRedex term = traverseSpine goRedex term
    where
    goRedex :: TraverseSpine v e -> TraverseSpine v e
    goRedex goNext term =
        goNext term >>= (\term' ->
        case term' of
            Lam v body -> do  spine <- gets sSpine
                              case spine of
                                  [] -> return term'
                                  (x:xs) -> if isRedex x || numOccurances v body <= 1
                                            then putSpine xs $ return (subst v (const x) body)
                                            else return term'
            _ -> return term'
        ) >>= after

    after :: TraverseSpine v e
    after term =
        do  (Ss spine change) <- get
            put (Ss [] change)
            return $ mkApps term spine

numOccurances :: (Outputable v, Outputable e, LcVar v e) => v -> Lc v e -> Int
numOccurances v term = runIdentity (foldLc isOccurance (+) 0 term)
    where
    isOccurance goFold term =
        case term of
            Var var -> if matchVar v var then 1 else 0
            _       -> goFold term
