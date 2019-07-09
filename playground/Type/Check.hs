{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Venturi.Type.Check
  ( tVarAll
  , typeAll
  , runCheckAll
  )
where

import Language.Venturi.Unique
import Language.Venturi.Binds
import Language.Venturi.Term
import Language.Venturi.Term.Labeled
import Language.Venturi.Type
import Language.Venturi.Kind
import Language.Venturi.Type.Env (Env(..))
import qualified Language.Venturi.Type.Env as Env
import Language.Venturi.Type.Constraint
import Language.Venturi.Type.Substitutable
import Language.Venturi.Type.Infer
import Language.Venturi.Type.Unify
import Language.Venturi.Type.Monad
import Language.Venturi.Extension.List
import Language.Venturi.Debug

import Language.Venturi.Pretty

import qualified Data.Map.Strict as M
import Data.List (nub, find)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normType body)
  where
    ord = zip (nub $ fv body) (map mkTVar letters)

    fv (TVar a ) = [a]
    fv (TAp a b) = fv a ++ fv b
    fv t         = []

    normVar a = case Prelude.lookup a ord of
                    Just x -> x
                    Nothing -> error "type variable not in signature"

    normType (TVar a ) = TVar (normVar a)
    normType (TAp a b) = TAp (normType a) (normType b)
    normType t = t

generalize :: Show b => Env b -> Type -> Scheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

-- | Canonicalize and return the polymorphic toplevel type.
-- closeOver :: Type -> Scheme
-- closeOver = normalize . generalize Env.empty

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

mkTVar :: String -> TyVar
mkTVar letter = TyVar Star (Uq letter 0)

assignTypeVar :: String -> a -> UniqueSupplyT Identity TyVar
assignTypeVar hint _ = TyVar Star <$> unique hint

tVarAll :: Show t => [Let I t] -> ([Let I TyVar], UniqueSupply)
tVarAll terms = tr "tVarAll" $ runIdentity $ runUnique (mapM (relabel assignTypeVar) $ tc "terms" terms) emptyUniqueSupply

typeAll :: Env b -> Subst -> [Let I TyVar] -> [Let I Type]
typeAll env subst terms = runReader (mapM (relabel (assignType env)) terms) subst

assignType :: Env b -> String -> TyVar -> Reader Subst Type
assignType env _ tv = reader . subLookup $ tv

-- | Solve for the toplevel type of an expression in a given environment

letPair :: Let I TyVar -> (Env.Name, Scheme)
letPair (Let name (Term tv _)) = (Left name, Forall [] (TVar tv))

runCheckAll :: Show t => [Let I t] -> Either TypeError [Let I Type]
runCheckAll ts = let (varTs, supply) = tVarAll ts
                     env = TypeEnv $ merge (Env.buildSchemes $ map letPair varTs) (mapKeys Left builtInSchemes)
                 in  evalInfer env supply (check varTs)

check :: [Let I TyVar] -> Infer BuiltIn [Let I Type]
check ts = do (tys, constraints) <- unzip <$> mapM inferLet ts
              subst <- solver (emptySubst, trStack "constraints" $ concat constraints)
              env <- ask
              return $ typeAll env subst ts
