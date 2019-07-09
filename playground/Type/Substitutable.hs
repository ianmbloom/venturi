{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Venturi.Type.Substitutable
  ( Substitutable(..)
  , emptySubst
  , compose
  )
where

import Language.Venturi.Type
import Language.Venturi.Type.Env
import Language.Venturi.Type.Constraint
import Language.Venturi.Binds
import Language.Venturi.Debug
import qualified Data.Set as Set
import qualified Data.Map.Strict as M

class Substitutable t where
    apply  :: Subst -> t -> t
    ftv    :: t -> Set.Set TyVar

first :: (a -> c) -> (a, b) -> (c, b)
first f (a,b) = (f a, b)

second :: (b -> c) -> (a, b) -> (a, c)
second f (a,b) = (a, f b)

instance Substitutable Type where
    apply s@(Subst mapping) t =
        case t of
            TVar tv   -> case M.lookup tv mapping of
                            Just x  -> x
                            Nothing -> t
            TAp t1 t2 -> TAp (apply s t1) (apply s t2)
            _         -> t
    ftv t =
        case t of
            TAp t1 t2 -> ftv t1 `Set.union` ftv t2
            TVar a    -> Set.singleton a
            _         -> Set.empty

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable Scheme where
    apply (Subst s) (Forall as t)  = Forall as $ apply s' t
                          where s' = Subst $ foldr M.delete s as
    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
   apply s (Unify t1 t2) = Unify (apply s t1) (apply s t2)
   ftv     (Unify t1 t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable (Maybe a) where
  apply s = fmap (apply s)
  ftv Nothing  = Set.empty
  ftv (Just a) = ftv a


instance Show b => Substitutable (Env b) where
  apply s (TypeEnv schemes ) = TypeEnv (fmap (apply s) schemes)
  ftv (TypeEnv (Binds schemes)) = ftv $ M.elems schemes

-- newtype Subst = Subst (M.Map TVar Type)
-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ M.map (apply (Subst s1)) s2 `M.union` s1

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
(@@) = compose

{-
  apply s           (t1 `TArr` t2) = apply s t1 `TArr` apply s t2
  apply (Subst s) t@(TVar a)       = case M.lookup t s of
                                        Just x -> x
                                        Nothing -> t
  apply s           (TPack tv ps)  = TPack tv (map (apply s) ps)
  apply _         t                = t
-}
