{-# LANGUAGE ScopedTypeVariables #-}

module Language.Venturi.Calculus.KnownCase
  ( knownCase
  , caseOfCase
  )
where

import Language.Venturi.Calculus
import Language.Venturi.Calculus.Tools
import Language.Venturi.Calculus.Substitute
import Outputable
import Data.List (find)

knownCase :: forall v e
          .  (Ord v, Outputable v, Outputable e)
          => Lc v e -> (Lc v e, Bool)
knownCase term =
    traverseEnv replaceKnown term
    where
    replaceKnown :: TraverseEnv v e
    replaceKnown term = case term of
        Case scrut as other -> case followSpine scrut of
            Select choice -> case find (isAlt choice) as of
                                Just alt -> do trackChange
                                               return $ transferSpine (fromAlt alt) scrut
                                Nothing -> error "other cases not handled by knownCase"
                                    --case other of
                                    --Just o -> return o
                                    --Nothing -> return term
            _ -> return term
        _ -> return term

caseOfCase :: forall v e
           .  (Ord v, Outputable v, Outputable e)
           => Lc v e -> (Lc v e, Bool)
caseOfCase term =
    traverseEnv moveCases term
    where
    moveCases :: TraverseEnv v e
    moveCases term =
        case term of
            Case (Case scrut as0 other0) as1 other1 ->
                return $ Case scrut (map (caseInside as1 other1) as0) (fmap (const (error "other case not handled by caseofcase" {-const (Case scrut as1 other1)-})) other0)
            _ -> return term

caseInside as other (Alt choice term) = Alt choice $ Case term as other

isAlt pick (Alt choice term) = True

transferSpine to (App f x) = App (transferSpine to f) x
transferSpine to _ = to
