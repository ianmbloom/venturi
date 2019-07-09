{-# LANGUAGE ScopedTypeVariables #-}

module Language.Venturi.Calculus.Tools
  ( mapVar
  , mapName
  , makeUnique
  , mapFrillType
  , mapFrillVar
  , substituteType
  , mkApps
  , mkLams
  , followSpine
  , spineArgs
  , flattenSpine
  , isComplexApp
  , isApp
  , isVar
  , sameVar
  , collectLams
  , maybeCons
  , mapFst
  , mapSnd
  , growList
  )
where

import Var
import Type
import Language.Venturi.Calculus
import Language.Venturi.Core.Frill
import Language.Venturi.Calculus.Traverse
import Language.Venturi.Unique
import Control.Monad.Identity
import Data.Maybe (fromMaybe)
import Outputable

mapVar :: forall v a b . (Outputable v, Outputable a, Outputable b)
       => (a -> b) -> Lc v a -> Lc v b
mapVar f term = runIdentity (traverseLc return (return . f) id term)

mapName :: forall v u e . (Outputable v, Outputable u, Outputable e)
        => (v -> u) -> Lc v e -> Lc u e
mapName f term = runIdentity (traverseLc (return . f) return id term)

makeUnique :: forall e . (Outputable e)
           => Lc String e -> Lc Unique e
makeUnique term = runIdentity $ evalUnique (traverseLc unique return id term)

mapFrillType :: (Type -> Type) -> Frill -> Frill
mapFrillType f frill =
    case frill of
        FrillType ty -> FrillType (f ty)
        x -> x

mapFrillVar :: (Var -> Var) -> Frill -> Frill
mapFrillVar f frill =
    case frill of
        FrillVar var -> FrillVar (f var)
        FrillType ty -> FrillType ty
        FrillLit lit -> FrillLit lit
        FrillCoerce co -> FrillCoerce co

substituteType :: Var -> Type -> Type -> Type
substituteType match sub ty = let s = zipTvSubst [match] [sub] in substTy s ty


mkApps :: Lc v e -> [Lc v e] -> Lc v e
mkApps f args = foldl App f args

mkLams :: [v] -> Lc v e -> Lc v e
mkLams args body = foldr Lam body args

followSpine :: Lc v e -> Lc v e
followSpine (App f x) = followSpine f
followSpine x = x

spineArgs :: Lc v e -> [Lc v e]
spineArgs (App f x) = spineArgs f ++ [x]
spineArgs x = []

flattenSpine :: Lc v e -> [Lc v e]
flattenSpine (App f x) = flattenSpine f ++ [x]
flattenSpine x = [x]

isComplexApp term = case term of
   App f x -> (isComplexApp f || isComplexApp x) || (isApp x)
   Lam    {} -> True
   Var    {} -> False
   Case   {} -> True
   Select {} -> False
   Call   {} -> True
   Recur  {} -> True

isApp (App {}) = True
isApp _ = False

isVar (Var {}) = True
isVar _ = False

sameVar c (Var v) = c `matchVar` v
sameVar c _ = False

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
maybeCons :: Maybe a -> [a] -> [a]
maybeCons (Just a) list = a:list
maybeCons Nothing  list = list

mapFst :: (a->b) -> (a, c) -> (b, c)
mapFst f (a,c) = (f a, c)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

collectLams :: Lc v e -> ([v], Lc v e)
collectLams (Lam v body) = mapFst (v:) $ collectLams body
collectLams body         = ([], body)

growList :: [a] -> [a] -> [[a]]
growList grow (s:ss) = let pre = grow ++ [s]
                       in  pre:growList pre ss
growList grow [] = []
