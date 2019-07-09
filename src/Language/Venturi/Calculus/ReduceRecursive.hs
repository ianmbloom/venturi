{-# LANGUAGE ScopedTypeVariables #-}

module Language.Venturi.Calculus.ReduceRecursive
  ( reduceRecursive
  )
where

import Language.Venturi.Nat
import Language.Venturi.Calculus
import Language.Venturi.Calculus.Pretty
import Language.Venturi.Calculus.Traverse
import Language.Venturi.Calculus.Substitute
import Language.Venturi.Core.Frill
import Language.Venturi.Core.ToVarLc
import Language.Venturi.Calculus.Tools
import Language.Venturi.Debug
import Language.Venturi.Core.Pretty
import Var
import Type
import Control.Monad.State
import Data.Maybe (fromMaybe, catMaybes)
import Outputable
import Control.Monad.Identity
import Control.Monad.Reader

type ReduceLc v e = TraverseLc (State [Lc v e]) v e v e

splitIfs :: [Bool] -> [a] -> ([a],[a])
splitIfs []      rest  = (rest,[])
splitIfs (c:cs) (x:xs) = let (trues,falses) = splitIfs cs xs
                         in  if c then (x:trues, falses) else (trues, x:falses)
splitIfs (c:cs) []     = ([],[])

mkAppLam :: v -> Lc v e -> Lc v e -> Lc v e
mkAppLam var arg term = App (Lam var term) arg

mkAppLams :: [v] -> [Lc v e] -> Lc v e -> Lc v e
mkAppLams vars args term = foldl (flip (uncurry mkAppLam)) term (zip vars args)

reduceRecursive :: forall v e
                .  (Ord v, Outputable v, Outputable e, LcVar v e)
                => Lc v e -> Lc v e
reduceRecursive term = evalState (traverseLc return return go term) []
    where
    go :: ReduceLc v e -> ReduceLc v e
    go goNext term = goCall term
        where
        goCall :: ReduceLc v e
        goCall term  =
            case term of
                Call name body ->
                    if isRecursive body
                    then
                        do  spine <- get
                            let (vars, rest) = collectLams body
                                removeList = recurringApplications vars rest
                                rest' = removeRecurApp removeList rest
                                (insideVars,  outsideVars ) = splitIfs removeList vars
                                (insideSpine, outsideSpine) = splitIfs removeList spine
                                newCall newBody = flip mkApps outsideSpine
                                                $ Call name
                                                $ mkLams outsideVars
                                                $ mkAppLams insideVars insideSpine newBody
                            put []
                            newCall <$> go goNext rest'
                    else goNext term
                App f x ->
                    do modify (x:)
                       go goNext f
                _ -> return term

deleteIf :: Bool -> a -> Maybe a
deleteIf cond x = if cond then Nothing else Just x

removeRecurApp :: forall v e
               .  (Ord v, LcVar v e, Outputable v, Outputable e)
               => [Bool] -> Lc v e -> Lc v e
removeRecurApp removeList term =
  fst $ traverseEnv removeRA term
  where
  removeRA :: TraverseEnv v e
  removeRA term =
      case followSpine term of
          Recur i ->
              do  calls <- reader travCalls
                  if natLength calls == i
                  then return $ mkApps (Recur i) (catMaybes (zipWith deleteIf removeList (spineArgs term)))
                  else return term
          _ -> return term

recurringApplications :: forall v e . (Ord v, LcVar v e, Outputable v, Outputable e)
                       => [v] -> Lc v e -> [Bool]
recurringApplications vars term =
    foldEnv appRecursionList (zipWith (&&)) (repeat True) term
    where
    appRecursionList :: (Lc v e -> ReaderEnv v e [Bool])
                     ->  Lc v e -> ReaderEnv v e [Bool]
    appRecursionList goFold term =
        case followSpine term of
            Recur i ->
                do  calls <- reader travCalls
                    if natLength calls == i
                    then return $ zipWith sameVar vars (spineArgs term)
                    else goFold term
            _ -> goFold term
