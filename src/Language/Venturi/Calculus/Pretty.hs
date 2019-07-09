module Language.Venturi.Calculus.Pretty
  ( pprLcTop
  )
where

import Prelude hiding ((<>))
import Language.Venturi.Nat
import Language.Venturi.Calculus
import Language.Venturi.Calculus.Tools
import Outputable

instance Outputable Nat where
  ppr = ppr . natToInt

pprNatIndex :: (Outputable a) => Nat -> [a] -> SDoc
pprNatIndex i xs = case natIndex i xs of
                      Just x -> ppr x
                      Nothing -> text "UNKNOWN" <+> ppr i <+> hsep (map ppr xs)

instance Outputable Choice where
  pprPrec i choice = ppr (choiceTag choice) <+> ppr (choiceName choice) -- <+> hsep (map ppr (choiceTypes choice))

instance Outputable Direction where
    pprPrec i direction =
        case direction of
            L -> text "◀︎"
            R -> text "▶︎"

instance Outputable Name where
  ppr = text . unNm

instance (Outputable v, Outputable e) => Outputable (Lc v e) where
    pprPrec i term = case term of
        Lam name body -> hang (text "λ" <+> ppr name <+> text "→") 4 (ppr body)
        Var i -> text "𝗩" <+> ppr i
        App f x -> if isComplexApp term
                   then hang (text "@") 4 (ppr f $+$ ppr x)
                   else text "@:" <+> hsep (map (parens . ppr) $ flattenSpine term)
        Case scrut alts other -> hang (text "case" <+> ppr scrut) 4
                                 (vcat (map ppr alts ++
                                        maybe [] (\d -> [text "otherwise ->" <+> ppr d]) other
                                       ))
        Select choice -> text "S" <> ppr choice
        Call name term -> hang (text "Ⓒ" <+> ppr name) 4 (ppr term)
        Recur i -> text "↺" <+> ppr i

instance (Outputable v, Outputable e) => Outputable (LcAlt v e) where
    pprPrec i (Alt choice term) = hang (text "A" <> ppr choice <+> text "->") 4 (ppr term)

pprLcTop :: (Outputable v, Outputable e) => Lc v e -> SDoc
pprLcTop term =
    case term of
        Lam  v body     -> text "Lam" <+> ppr v
        Var  v          -> text "Var" <+> ppr v
        App  f x        -> text "App"
        Case scrut as o -> text "Case"
        Select choice   -> text "Select" <+> ppr choice
        Call name body  -> text "Call" <+> ppr name
        Recur i         -> text "Recur" <+> ppr i
