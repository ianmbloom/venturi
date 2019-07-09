{-# LANGUAGE ScopedTypeVariables #-}

module Language.Venturi.Calculus.ApplyDataConTys
  ( applyDataConTys
  )
where

import Language.Venturi.Calculus
import Language.Venturi.Calculus.Tools
import Language.Venturi.Calculus.Traverse
import Language.Venturi.Calculus.Reduce
import Language.Venturi.Core.Frill
import Language.Venturi.Unique
import Language.Venturi.Debug

import Control.Monad.State
import Outputable
import Type

splitJust :: (a -> Maybe b) -> [a] -> ([b],[a])
splitJust f (x:xs) = case f x of
                       Just a -> mapFst (a:) (splitJust f xs)
                       Nothing -> ([],x:xs)
splitJust f [] = ([],[])

applyDataConTys :: Lc Var Frill -> Lc Var Frill
applyDataConTys = fst . traverseSpine applyTypesToSelect
    where
    isTypeMaybe :: Lc Var Frill -> Maybe Type
    isTypeMaybe term =
        case term of
            Var (FrillType ty) -> Just ty
            _                  -> Nothing

    applyTypesToSelect :: TraverseSpine Var Frill -> TraverseSpine Var Frill
    applyTypesToSelect goTraverse term =
        case term of
            Select choice -> do spine <- gets sSpine
                                let (tys, rest) = splitJust isTypeMaybe spine
                                putSpine rest $ goTraverse $ Select $ appendTypesToChoice choice tys
            _ -> goTraverse term
