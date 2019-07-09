{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Venturi.Core.ToVarLc

  ( buildEnv
  , toVarLc
  , Env(..)
  , VarLc(..)
  , VarLcAlt(..)
  )
where

import Prelude hiding ((<>))

import GHC hiding (pprExpr, convert, typeKind)
import Var
import CoreSyn
import DataCon
import Id
import Coercion
import Literal
import Type

import qualified Language.Venturi.Calculus as S
import Language.Venturi.Core.Frill

import Data.Maybe (isJust, catMaybes, fromJust)
import Data.List (find, elemIndex)
import qualified Data.Map as M
import Control.Monad (join)
import Language.Venturi.Nat
import Language.Venturi.Debug
import Language.Venturi.Core.Pretty
import Outputable

type VarLcAlt = S.LcAlt Var Frill
type VarLc = S.Lc Var Frill

data Env a = Env
    { envMap    :: M.Map Var a
    , envCallStack :: [Var]
    }

mkEnv :: [(Var, a)] -> Env a
mkEnv list =
    Env { envMap     = M.fromList list
        , envCallStack = []
        }

pushCallStack :: Env a -> Var -> Env a
pushCallStack (Env m c) var = Env m (var:c)

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

checkCallStack :: Env a -> Var -> Bool
checkCallStack env var = count var (envCallStack env) > 0

plusEnv :: Env a -> [(Var, a)] -> Env a
plusEnv (Env map0 stack) map1 = Env (map0 `M.union` M.fromList map1) stack

lookupEnv :: Env a -> Var -> Maybe a
lookupEnv env = flip M.lookup (envMap env)

buildEnv :: CoreModule -> Env (Expr Var)
buildEnv coreModule = mkEnv . flattenBinds $ cm_binds coreModule

addBinds env newBinds = plusEnv env . flattenBinds $ [newBinds]

toVarLc :: Env (Expr Var) -> Expr Var -> VarLc
toVarLc env term = go env term
    where
    go :: Env (Expr Var) ->  Expr Var -> VarLc
    go env term =
       --tcP ("go" ++ (showSDocUnsafe . ppr $ term)) $
       case term of
           Var  v               -> goVar env v
           Lit  lit             -> S.Var (FrillLit lit)
           App  f x             -> S.App (go env f) (go env x)
           Lam  v body          -> S.Lam v (go env body)
           Let  newBinds body   -> go (addBinds env newBinds) body
           Case scrut v ty alts -> goCase env scrut alts
           Cast body coercion   -> go env body
           Tick tickId body     -> go env body
           Type ty              -> S.Var (FrillType ty)
           Coercion coercion    -> S.Var (FrillCoerce coercion)

    goVar :: Env (Expr Var) -> Var -> VarLc
    goVar env var =
        case isDataConId_maybe var of
            Just dataCon -> S.Select (dataConToChoice dataCon [])
            Nothing -> case lookupEnv env var of
                           Nothing -> S.Var (FrillVar var)
                           Just e  -> case natElem var (envCallStack env) of
                                           Just i ->  S.Recur i
                                           Nothing -> S.Call (S.Nm . pprShow $ var) $ go (pushCallStack env var) e

    goCase :: Env (Expr Var) -> Expr Var -> [Alt Var] -> VarLc
    goCase env scrut alts =
        if isLiteralAlt (head alts)
        then buildLiteralAlts env scrut alts
        else uncurry (S.Case (go env scrut)) (goAlts env alts [])

    goAlts :: Env (Expr Var) -> [Alt Var] -> [VarLcAlt] -> ([VarLcAlt], Maybe VarLc)
    goAlts env ((DataAlt dataCon, altVars, body):rest) bs =
              let varLams = mkLams altVars
                  tyVars = dataConUnivTyVars dataCon
                  choice = dataConToChoice dataCon tyVars
                  newAlt = S.Alt choice (go env (varLams $ body))
              in  goAlts env rest (bs ++ [newAlt])
    goAlts env ((DEFAULT, [], body):_) bs = (bs, Just (go env body))
    goAlts env [] bs = (bs, Nothing)

isLiteralAlt :: Alt Var -> Bool
isLiteralAlt (altCon, _, _) =
    case altCon of
        LitAlt  {} -> True
        _ -> False

buildLiteralAlts :: Env (Expr Var) -> Expr Var -> [Alt Var] -> VarLc
buildLiteralAlts env scrut alts = error "buildLiteralAlts is undefined"

dataConToChoice :: DataCon -> [Var] -> S.Choice
dataConToChoice dataCon tyVars =
  let tycon = dataConTyCon dataCon
      tag   = dataConTagZ dataCon
      -- dataConInstArgTys :: DataCon -> [Type] -> [Type]
      args  = dataConInstArgTys dataCon
      count = length (tyConDataCons tycon)
      name  = S.Nm . pprShow $ dataCon
      vars  = map mkTyVarTy tyVars
  in  S.Choice tag count name []


instance S.LcVar Var Frill where
   matchVar var (FrillVar fv) = var == fv
   matchVar var (FrillType ft) = case getTyVar_maybe ft of
                                   Just tyVar -> var == tyVar
                                   Nothing -> False
   matchVar var _ = False
