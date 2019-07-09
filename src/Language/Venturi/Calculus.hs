{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Venturi.Calculus
    ( Name(..)
    , Lc(..)
    , LcAlt(..)
    , fromAlt
    , mapAlt
    , Direction(..)
    , Choice(..)
    , appendTypesToChoice
    , LcVar(..)
    )
where

-- knotITraverse ::  (i -> t a -> f b) -> t a -> f (t b)
import Language.Venturi.Nat
import Outputable
import Prelude hiding ((<>))
import Literal
import Type
import TysPrim

newtype Name = Nm {unNm :: String} deriving (Eq, Ord)
instance Show Name where
  show = unNm

data Direction = L | R

data Choice = Choice
    { choiceTag   :: Int
    , choiceCount :: Int
    , choiceName  :: Name
    , choiceTypes :: [Type]
    }

appendTypesToChoice :: Choice -> [Type] -> Choice
appendTypesToChoice (Choice tag count string tys) new = Choice tag count string (tys ++ new)

data LcAlt v e = Alt Choice (Lc v e)

fromAlt :: LcAlt v e -> Lc v e
fromAlt (Alt choice expr) = expr

mapAlt :: (Lc v e -> Lc v e) -> LcAlt v e -> LcAlt v e
mapAlt f (Alt choice expr) = Alt choice (f expr)

data Lc v e where
    Lam    :: v -> Lc v e -> Lc v e
    Var    :: e -> Lc v e
    App    :: Lc v e -> Lc v e -> Lc v e
    Case   :: Lc v e -> [LcAlt v e] -> Maybe (Lc v e) -> Lc v e
    Select :: Choice -> Lc v e
    Call   :: Name -> Lc v e -> Lc v e
    Recur  :: Nat -> Lc v e

class LcVar v e where
  matchVar :: v -> e -> Bool
