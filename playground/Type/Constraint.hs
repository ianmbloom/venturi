{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Venturi.Type.Constraint
  ( Constraint(..)
  , TypeReport(..)
  , Unifier(..)
  , Subst(..)
  , subLookup
  )
where

import Language.Venturi.Type
import Language.Venturi.Term

import qualified Data.Map as M

data Constraint = Unify Type Type
                  deriving (Eq, Show)

data TypeReport = Report [Constraint] [Constraint] Subst Type Scheme

type Unifier = (Subst, [Constraint])

newtype Subst = Subst (M.Map TyVar Type)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

subLookup :: TyVar -> Subst -> Type
subLookup tv (Subst mapping) = M.findWithDefault (TVar tv) tv mapping
