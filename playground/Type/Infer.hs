{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE FlexibleContexts           #-}

module Language.Venturi.Type.Infer
  ( inferLet
  )
where

import Language.Venturi.Unique
import Language.Venturi.Term
import Language.Venturi.Type.Monad
import Language.Venturi.Type
import Language.Venturi.Kind
import Language.Venturi.Type.Env (Env(..))
import qualified Language.Venturi.Type.Env as Env
import Language.Venturi.Type.Constraint
import Language.Venturi.Type.Substitutable
import Language.Venturi.Pretty
import Language.Venturi.Debug
import Language.Venturi.Extension.List

import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

-- | Extend type environment
inEnv :: (Env.Name, Scheme) -> Infer b a -> Infer b a
inEnv (x, sc) m = do
  let scope e = (Env.removeScheme e x) `Env.extendScheme` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupType :: Env.Name -> Infer b Type
lookupType name =
    do  env <- ask
        case Env.lookupScheme env name of
            Just s  -> instantiate s
            Nothing -> throwError $ UnboundVariable name

instantiate :: Scheme -> Infer b Type
instantiate (Forall as t) =
    do  as' <- mapM (fmap TVar . const (fresh "t")) as
        let s = Subst $ M.fromList $ zip as as'
        return $ apply s t

inferLet :: Let I TyVar -> Infer BuiltIn (Type, [Constraint])
inferLet (Let name term) = inferTerm term

inferTerm :: Term I TyVar -> Infer BuiltIn (Type, [Constraint])
inferTerm (Term tv term') = inferTerm' tv term'

freshTyVar :: VName -> Infer BuiltIn Type
freshTyVar vName =
  do v <- fresh (show vName)
     return $ TVar v

inferTerm' :: TyVar -> Term' I TyVar -> Infer BuiltIn (Type, [Constraint])
inferTerm' tv term' =
    case term' of
        Lam vName body ->
            do  newTVarForLambda <- freshTyVar vName
                (typeOfBody, constraintsFromBody) <- inEnv (Right vName, Forall [] newTVarForLambda) (inferTerm body)
                let arrowToBody = newTVarForLambda `fn` typeOfBody
                let newConstraint = Unify (TVar tv) arrowToBody
                return (arrowToBody, newConstraint:constraintsFromBody)
        App f x ->
            do  (typeOfF, constraintsFromF) <- inferTerm f
                (typeOfX, constraintsFromX) <- inferTerm x
                let newConstraint = Unify typeOfF (typeOfX `fn` TVar tv)
                return (TVar tv, newConstraint:constraintsFromF ++ constraintsFromX)
        Case alts ->
            undefined
            --do  (typesFromAlts, altConstraints) <- unzip <$> mapM (inferAlt tv) alts
            --    let variantConstraints = Unify (TVar tv) (TVariant typesFromAlts)
            --    return (TVar tv, variantConstraints ++ concat altConstraints)
        Named i -> inferI tv i

inferAlt :: TyVar -> Alt I TyVar -> Infer BuiltIn (Type, [Constraint])
inferAlt _ (Alt pat term) =
    case pat of
      Con name -> do (altType, termConstraints) <- inferTerm term
                     ty <- lookupType (Left name)
                     return (ty `fn` altType, termConstraints)
      _         -> error "non constructor patterns not implemented"

inferName :: Env.Name -> Infer BuiltIn (Type, [Constraint])
inferName name =
  do ty <- lookupType name
     return (ty, [])

inferI :: TyVar -> I -> Infer BuiltIn (Type, [Constraint])
inferI tv named =
    case named of
        Var  vName -> inferName (Right vName)
        Call lName -> inferName (Left  lName)
        Ext  e     -> inferExt e
        Con  lName -> inferName (Left  lName)

inferExt :: Ext -> Infer BuiltIn (Type, [Constraint])
inferExt ext =
    case ext of
        EFunction name -> inferName (Left name)
        LInt    {} -> return (tInt   , [])
        LUInt   {} -> return (tUInt  , [])
        LFloat  {} -> return (tFloat , [])
        LDouble {} -> return (tDouble, [])
        LChar   {} -> return (tChar  , [])
