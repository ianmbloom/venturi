{-# LANGUAGE FlexibleInstances #-}

module Language.Venturi.Core.Frill
  ( Frill(..)
  , typeOfFrill
  )
where

import Prelude hiding ((<>))
import Language.Venturi.Calculus
import Coercion
import Literal
import Type
import Var
import Outputable

data Frill
    = FrillLit    Literal
    | FrillType   Type
    | FrillCoerce Coercion
    | FrillVar    Var

instance Outputable Frill where
  pprPrec i frill =
      case frill of
          FrillLit e    -> ppr e
          FrillType t   -> text "Type" <+> ppr t
          FrillCoerce c -> text "⎡" <> ppr c <> text "⎤"
          FrillVar v    -> text "⦿" <+> ppr v <+> text "::" <+> ppr (typeOfFrill frill)

typeOfFrill :: Frill -> Type
typeOfFrill frill = case frill of
    FrillLit lit -> literalType lit
    FrillType ty -> typeKind ty
    FrillCoerce coercion -> coercionType coercion
    FrillVar var -> varType var
