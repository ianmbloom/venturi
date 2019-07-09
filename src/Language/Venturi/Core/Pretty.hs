module Language.Venturi.Core.Pretty
  ( pprGhcModule
  , pprBind
  , pprBinder
  , pprExpr
  , pprVar
  , pprAlt
  , pprCo
  , pprShow
  , docShow
  , pprCoreRule
  )
where

import qualified GHC      as C
import qualified CoreSyn  as C
import qualified TyCoRep  as C
import qualified Var      as C
import qualified Literal  as C
import qualified IdInfo   as C

import Outputable
import Prelude hiding ((<>))

pprShow :: Outputable a => a -> String
pprShow = showSDocUnsafe . ppr

docShow :: SDoc -> String
docShow = showSDocUnsafe

pprGhcModule :: C.CoreModule -> SDoc
pprGhcModule coreModule =
  nest 4 $ vcat (map (pprBind 0) (C.cm_binds coreModule))

pprBind :: Rational -> C.Bind C.Var -> SDoc
pprBind p bind =
    case bind of
      C.NonRec v term -> pprBinder p (v, term)
      C.Rec rs -> vcat (map (pprBinder p) rs)

pprBinder :: Rational -> (C.Var, C.Expr C.Var) -> SDoc
pprBinder p (v, term) =
  ppr v <+> text "=" <+> pprExpr p term

pprExpr p term =
    case term of
        C.Var v -> text "Var" <+> pprVar p v
        C.Lit lit -> text "Lit" <+> ppr lit
        C.App f x -> hang (text "App") 4 (pprExpr p f $+$ pprExpr p x)
        C.Lam v body -> hang (text "\\" <+> ppr v <+> text "->") 4 (pprExpr p body)
        C.Let binding body -> hang (text "Let") 4 (pprBind p binding) $+$ hang (text "in") 4 (pprExpr p body)
        C.Case scrut v ty alts -> hang (text "Case" <+> pprExpr p scrut <+> text "of") 4 (vcat (map (pprAlt p) alts))
        C.Cast term coercion -> hang (text "Cast" <+> pprExpr p term) 4 (text "~" $+$ pprCo p coercion)
        C.Tick tickId term -> hang (text "Tick" <+> ppr tickId) 4 (pprExpr p term)
        C.Type ty -> text "Type" <+> ppr ty
        C.Coercion coercion -> text "Coerc" <+> pprCo p coercion

pprVar p var = ppr (C.varName var) {- <+> text "::" <+> ppr (C.varUnique var) <+>ppr (C.varType var)
               <> if C.isId var then pprRuleInfo (C.ruleInfo . C.idInfo $ var) else empty -}

pprAlt p (altCon, vs, term) =
    case altCon of
        C.DataAlt dataCon -> pprPrec p dataCon <+> sep (map (pprPrec p) vs) <+> text "->" <+> pprExpr p term
        C.LitAlt literal  -> pprPrec p literal <+> sep (map (pprPrec p) vs) <+> text "->" <+> pprExpr p term
        C.DEFAULT         -> sep (map (pprPrec p) vs) <+> text "->" <+> pprExpr p term


pprCo p coercion =
    case coercion of
        C.Refl role ty                  -> text "Refl" <+> pprPrec p role <+> pprPrec p ty
        C.TyConAppCo role tycon cs      -> hang (text "TyConAppCo" <+> pprPrec p role <+> pprPrec p tycon) 4 (vcat (map (pprCo p) cs))
        C.AppCo c cn                    -> text "AppCo" <+> pprPrec p c <+> pprPrec p cn
        C.ForAllCo tv kco co            -> text "ForAllCo" <+> pprPrec p tv <+> pprPrec p kco <+> pprCo p co
        C.FunCo rol co coN              -> text "FunCo" <+> pprPrec p rol<+> pprCo p co <+> pprPrec p coN
        C.CoVarCo coVar                 -> text "CoVarCo" <+> pprPrec p coVar
        C.AxiomInstCo coaxium bIndex cs -> hang (text "AxiomInstCo" <+> pprPrec p coaxium <+> pprPrec p bIndex) 4 (vcat (map (pprCo p) cs))
        C.AxiomRuleCo coaxiumRule cs    -> hang (text "AxiomRuleCo" <+> pprPrec p coaxiumRule)                  4 (vcat (map (pprCo p) cs))
        C.UnivCo u role ty1 ty2         -> text "UnivCo" <+> pprPrec p u <+> pprPrec p role <+> pprPrec p ty1 <+> pprPrec p ty2
        C.SymCo co                      -> text "SymCo" <+> pprCo p co
        C.TransCo co1 co2               -> text "TransCo" <+> pprCo p co1 <+> pprCo p co2
        C.NthCo  role co                -> text "NthCo" <+> pprPrec p role <+> pprCo p co
        C.LRCo   lr coN                 -> text "LRCo" <+> pprPrec p lr <+> pprPrec p coN
        C.InstCo co coN                 -> text "InstCo" <+> pprCo p co <+> pprPrec p coN
        C.CoherenceCo co kco            -> text "CoherenceCo" <+> pprCo p co <+> pprPrec p kco
        C.KindCo co                     -> text "KindCo" <+> pprCo p co
        C.SubCo coN                     -> text "SubCo" <+> pprPrec p coN
        C.HoleCo co                     -> text "HoleCo" <+> pprPrec p co

pprRuleInfo ruleInfo = vcat (map pprCoreRule . C.ruleInfoRules $ ruleInfo)

pprCoreRule (C.Rule name activation fn rough bndrs args rhs auto origin orphan local) = hang (text "Rule:" <+> ppr name) 4 (vcat (map (pprVar 0) bndrs
                                                                                                                             ++ map (pprExpr 0) args
                                                                                                                             ++ [pprExpr 0 rhs]))
pprCoreRule (C.BuiltinRule name fn nargs try) = text "BuiltinRule:" <+> ppr name
