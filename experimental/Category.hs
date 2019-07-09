{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Venturi.Compile.Category
  ( ProdT(..)
  , (:*:)
  , pattern (:*:)
  , ArrT(..)
  , (:->:)
  , pattern (:->:)
  , SumT(..)
  , (:+:)
  , pattern (:+:)
  , CategoryC(..)
  , CartesianC(..)
  , CocartesianC(..)
  , ClosedC(..)
  , ConstC(..)
  , constC
  )
where

import GHC.Types (Constraint)

data ProdT a b = ProdT a b
type (a :*: b) = ProdT a b
pattern (:*:) a b = ProdT a b
data ArrT a b = ArrT a b
type (a :->: b) = ArrT a b
pattern (:->:) a b = ArrT a b
data SumT a b = SumT a b
type (a :+: b) = SumT a b
pattern (:+:) a b = SumT a b


class CategoryC k where
    type Ok k a :: Constraint
    type Ok k a = (() :: Constraint)-- default vacuous constraint
    idC      :: Ok  k a     => a `k` a
    composeC :: Ok3 k a b c => (b `k` c) -> (a `k` b) -> (a `k` c)

type Ok2 k a b   = (Ok  k a  , Ok k b)
type Ok3 k a b c = (Ok2 k a b, Ok k c)

class CategoryC k => CartesianC k where
    prodC :: Ok3 k a c d => (a `k` c) -> (a `k` d) -> (a `k` (c :*: d))
    exlC  :: Ok2 k a b   => (a :*: b) `k` a
    exrC  :: Ok2 k a b   => (a :*: b) `k` b

class CategoryC k => CocartesianC k where
    inlC    :: Ok2 k a b   => a `k` (a :+: b)
    inrC    :: Ok2 k a b   => b `k` (a :+: b)
    switchC :: Ok3 k a c d => (c `k` a) -> (d `k` a) -> ((c :+: d) `k` a)

data One

class CategoryC k => TerminalC k where
    itC :: a `k` One

class (CartesianC k, CocartesianC k) => ClosedC k where
    applyC   :: Ok2 k a b   => ((a :->: b) :*: a) `k` b
    curryC   :: Ok3 k a b c => ((a :*: b) `k` c) -> (a `k` (b :->: c))
    uncurryC :: Ok3 k a b c => (a `k` (b :->: c)) -> ((a :*: b) `k` c)

class (CartesianC k, CocartesianC k) => DistribC k where
    distlC :: Ok3 k a u v => (a :*: (u :+: v)) `k` ((a :*: u) :+: (a :*: v))
    distrC :: Ok3 k a u v => ((a :*: u) :+: (a :*: v)) `k` (a :*: (u :+: v))

class TerminalC k => ConstC k b where
    unitArrowC :: b -> (One `k` b)

constC :: (Ok k a, Ok k One, Ok k b, ConstC k b) => b -> (a `k` b)
constC b = unitArrowC b `composeC` itC

class CartesianC k => BoolCatC k where
    notC :: Bool `k` Bool
    andC, orC :: (ProdT Bool Bool) `k` Bool

class NumCat k a where
    negateC :: a `k` a
    addC, mulC :: (ProdT a a) `k` a
