{-# LANGUAGE ScopedTypeVariables #-}

module Language.Venturi.Calculus.ReduceDeBruijn
  ( ReaderDj(..)
  , TraverseDj(..)
  , traverseDj
  , subDj
  , abstract
  , foldDj
  , pprDj
  , trDj
  )
where


import Prelude hiding ((<>))

import Language.Venturi.Calculus
import Language.Venturi.Calculus.Tools
import Language.Venturi.Calculus.Pretty
import Language.Venturi.Calculus.Traverse
import Language.Venturi.Calculus.Fold
import Language.Venturi.Calculus.ToDeBruijn
import Language.Venturi.Core.Frill
import Language.Venturi.Debug
import Language.Venturi.Core.Pretty
import Var
import Type
import Outputable
import Control.Monad.Reader
import Control.Monad
import Control.Applicative
import Data.Maybe (fromMaybe)

nextVar  :: (Nat, Nat) -> (Nat,Nat)
nextVar  (var, callLevel) = (S var, callLevel)
prevVar  :: (Nat, Nat) -> (Nat,Nat)
prevVar  (S var, callLevel) = (var, callLevel)
nextCall :: (Nat, Nat) -> (Nat,Nat)
nextCall (var, callLevel) = (var, S callLevel)
prevCall :: (Nat, Nat) -> (Nat,Nat)
prevCall (var, S callLevel) = (var, callLevel)

type ReaderDj = Reader (Nat, Nat)
type TraverseDj v e = TraverseLc ReaderDj v (DeBruijn e) v (DeBruijn e)

traverseDj :: forall v e
           .  (Outputable v, Outputable e)
           => Nat -> Nat
           -> TraverseDj v e
           -> DLc v e -> DLc v e
traverseDj varLevel callLevel transform term =
    runReader (traverseLc return return go term) (varLevel, callLevel)
    where
    go :: TraverseDj v e -> TraverseDj v e
    go goTraverse term =
        (case term of
             Lam v body     -> local nextVar  $ goTraverse term
             Call name body -> local nextCall $ goTraverse term
             _ ->  goTraverse term
        ) >>= transform

foldDj :: forall v e a
       .  (Outputable v, Outputable e, Outputable a)
       => Nat -> Nat
       -> ((Lc v (DeBruijn e) -> ReaderDj a) ->  Lc v (DeBruijn e) -> ReaderDj a)
       -> (a -> a -> a)
       -> a
       -> Lc v (DeBruijn e)
       -> a
foldDj var callLevel transform op identity term =
    runReader (foldLc goDebruijn op identity term) (var, callLevel)
    where
    goDebruijn :: (Lc v (DeBruijn e) -> ReaderDj a) -> Lc v (DeBruijn e) -> ReaderDj a
    goDebruijn goFold term =
      do  case term of
              Lam v     body -> local nextVar  $ transform goFold term
              Call name body -> local nextCall $ transform goFold term
              _ -> transform goFold term

subDj :: forall v e
      .  (Outputable v, Outputable e)
      => Nat
      -> (DLc v e -> DLc v e)
      ->  DLc v e -> DLc v e
subDj var op term =
    traverseDj var Z subVar term
    where
    subVar :: TraverseDj v e
    subVar term =
        case term of
            Var (Bound v) ->
                do  (varLevel, _) <- ask
                    if v == varLevel
                    then return $ op term
                    else return term
            _ -> return term

abstract :: forall v e
         .  (Outputable v, Outputable e)
         => DLc v e -> DLc v e
abstract term =
    traverseDj Z Z abstractVar term
    where
    abstractVar :: TraverseDj v e
    abstractVar term =
        case term of
            Var (Bound v) -> do (varLevel, _) <- ask
                                let v' = if v >= varLevel then S v else v
                                return $ Var (Bound v')
            _ -> return term

isRecursive :: (Outputable v, Outputable e)
            => Lc v (DeBruijn e) -> Bool
isRecursive term = foldDj Z Z checkRecur (||) False term
  where
    checkRecur :: (Lc v (DeBruijn e) -> ReaderDj Bool) -> Lc v (DeBruijn e) -> ReaderDj Bool
    checkRecur goFold term =
      case term of
          Recur i -> do (_, level) <- ask
                        if i == level
                        then return True
                        else return False
          _ -> goFold term

addVar v (vars, calls) = (v:vars, calls)
addCall c (vars, calls) = (vars, c:calls)

pprDj :: (Outputable v, Outputable e)
      => Lc v (DeBruijn e) -> SDoc
pprDj = go ([], [])
  where
    go :: (Outputable v, Outputable e)
       => ([v],[Name]) -> Lc v (DeBruijn e) -> SDoc
    go s term = case term  of
        Lam name body -> hang (text "λ" <+> ppr name <+> text "->") 4 (go (addVar name s) body)
        Var v -> pprDeBruijn s v
        App f x -> if isComplexApp term
                   then hang (text "@") 4 (go s f $+$ go s x)
                   else text "@:" <+> hsep (map (parens . go s) $ flattenSpine term)
        Case scrut alts other -> hang (text "case" <+> go s scrut) 4
                                 (vcat (map (goAlt s) alts ++
                                        maybe [] (\d -> [text "otherwise ->" <+> go s d]) other
                                       ))
        Select choice -> text "S" <> ppr choice
        Call name term -> hang (text "C" <+> ppr name) 4 (go (addCall name s) term)
        Recur i -> text "R" <+> (pprNatIndex i $ snd s)
    goAlt s (Alt choice term) = hang (text "A" <> ppr choice <+> text "->") 4 (go s term)

pprDeBruijn :: (Outputable v, Outputable e)
            => ([v],[Name]) -> DeBruijn e -> SDoc
pprDeBruijn s v =
    case v of
        Bound i -> let varName = (pprNatIndex i $ fst s)
                   in  text "D" <> ppr i <+> varName
        Unbound e -> ppr v

trDj message = trPW pprDj (docShow message)
