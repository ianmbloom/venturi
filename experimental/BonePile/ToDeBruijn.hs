module Language.Venturi.Calculus.ToDeBruijn
  ( toDeBruijn
  , DeBruijn(..)
  , DLc(..)
  , DLcAlt(..)
  )
where

import Prelude hiding ((<>))
import Var
import Outputable
import TyCoRep
import Language.Venturi.Calculus
import Language.Venturi.Core.Frill
import Language.Venturi.Core.ToVarLc
import Language.Venturi.Calculus.Tools
import Language.Venturi.Core.Pretty
import Language.Venturi.Unique
import Control.Monad.State

data DeBruijn v = Bound Nat | Unbound v
type DLc v e = Lc v (DeBruijn e)
type DLcAlt v e = LcAlt v (DeBruijn e)

emptyStack :: [Var]
emptyStack = []

pushStack :: Var -> [Var] -> [Var]
pushStack = (:)

toDeBruijn :: VarLc -> DLc Var Frill
toDeBruijn term = go emptyStack term
  where
    go :: [Var] -> VarLc -> DLc Var Frill
    go stack term =
      case term of
        Lam  v body     -> Lam v (go (pushStack v stack) body)
        Var  v          -> Var (goFrill stack v)
        App  f x        -> App (go stack f) (go stack x)
        Case scrut as o -> Case (go stack scrut) (map (goAlt stack) as) (fmap (go stack) o)
        Select choice   -> Select choice
        Call name call  -> Call name (go stack call)
        Recur i         -> Recur i
    goFrill :: [Var] -> Frill -> DeBruijn Frill
    goFrill stack var = case var of
        FrillVar v ->
            case natElem v stack of
                Just n  -> Bound n
                Nothing -> Unbound (FrillVar v)
        FrillType (TyVarTy v) ->
            case natElem v stack of
                Just n  -> Bound n
                Nothing -> Unbound (FrillType (TyVarTy v))
        x -> Unbound x

    goAlt :: [Var] -> VarLcAlt -> DLcAlt Var Frill
    goAlt stack (Alt c body) = Alt c (go stack body)

instance Outputable v => Outputable (DeBruijn v) where
    ppr (Bound n) = text "[" <> ppr (natToInt n) <> text "]"
    ppr (Unbound v) = text "[" <> text "u" <+> ppr v <> text "]"
