{-# LANGUAGE GADTs #-}

module Language.Venturi.Compile.Pseudo
  ( {-PseudoProgram(..)
  , lcToPseudo-}
  )
where

import Prelude hiding ((<>))
import Language.Venturi.Calculus
import Language.Venturi.Calculus.Tools
import Language.Venturi.Core.Frill
--import Language.Venturi.Calculus.ToCommand
import Language.Venturi.Unique
import Language.Venturi.Core.Pretty
import Language.Venturi.Calculus.Pretty
import Language.Venturi.Debug

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe

import Var
import Outputable

{-
data PseudoProgram = PsP [Pseudo] (Maybe (VRef Command)) [PseudoRecur]

data Pseudo where
    PsAssign  :: String -> [Pseudo] -> Pseudo
    PsOp      :: Command -> [VRef Command] -> Pseudo
    PsBlock   :: VRef Command -> [String] -> [Pseudo] -> Pseudo
    PsSwitch  :: String -> [PseudoAlt] -> [Pseudo] -> Pseudo
    PsCommand :: VRef Command -> Int -> Pseudo

data PseudoRecur where
    PsRecur :: VRef Command -> [VRef Command] -> [VRef Command] -> PseudoRecur

data PseudoAlt = PsAlt Choice [Pseudo]

data PseudoState = PsState
  { psCallStack :: [VRef Var]
  , psSpine     :: [CLc]
  }

type PsStateM = ReaderT PseudoState UniqueM

psUnique :: String -> PsStateM (VRef Command)
psUnique name = VRef <$> lift (unique name)

pushSpine :: CLc -> PseudoState -> PseudoState
pushSpine add (PsState callStack spine) = PsState callStack (add:spine)

putSpine :: [CLc] -> PseudoState -> PseudoState
putSpine newSpine (PsState callStack spine) = PsState callStack newSpine

emptySpine :: PseudoState -> PseudoState
emptySpine (PsState callStack spine) = PsState callStack []

pushCallStack :: [VRef Var] -> PseudoState -> PseudoState
pushCallStack list (PsState callStack spine) = PsState (list ++ callStack) spine

vRefString (VRef uq) = show uq
vRefString (V var) = pprShow (varName var)

lcToPseudo :: CLc -> PseudoProgram
lcToPseudo term = PsP (runIdentity (evalUnique $ runReaderT (go term) (PsState [] []))) Nothing []
    where
    go :: CLc -> PsStateM [Pseudo]
    go term = case term of
        Lam v body -> do  spine <- reader psSpine
                          case spine of
                              [] -> error "unApplied lambda"
                              x:xs  -> do applyCode <- local emptySpine (go x)
                                          bodyCode <- local (putSpine xs) (go body)
                                          return $ PsAssign (vRefString v) applyCode:bodyCode
        Var v -> goVar v
        App f x -> local (pushSpine x) (go f)
        Case s as other ->
            do scrutVar <- lift $ unique "scrutVar"
               sCode <- go s
               alts <- mapM goAlt as
               otherCode <- maybe (return []) go other
               return $ PsAssign (show scrutVar) sCode:[PsSwitch (show scrutVar) alts otherCode]
        Select _ -> error "selects should be removed."
        Call name body -> do  let (args, body') = collectLams body
                              callName <- psUnique (unNm name)
                              bCode <- local (pushCallStack args) (go body')
                              return [PsBlock callName (map vRefString args) bCode]
        Recur i -> error "recur encountered"
    goAlt (Alt choice body) = do bodyCode <- local emptySpine $ go body
                                 return (PsAlt choice bodyCode)


    goVar :: VRef Command -> PsStateM [Pseudo]
    goVar vRef =
        case vRef of
            VRef name -> undefined
            V command ->
                do spine <- reader psSpine
                   spineComs <- mapM (local emptySpine . go) spine
                   case command of
                       Peek ty -> return [PsOp (Peek ty) []]
                       Poke ty -> return [PsOp (Poke ty) []]
                       Offset ty -> undefined
                       NilRef -> undefined
                       AddRef -> undefined
                       Tag i -> undefined
                       CFrill f -> undefined
instance Outputable PseudoProgram where
    ppr (PsP code name recurs) = hang (text "Recurs:") 4 (vcat (map ppr recurs)) $+$
                                 hang (text "Program:") 4 (vcat (map ppr code))

pprArgs args = parens (hcat $ punctuate comma $ map text args)
pprApply args = parens (hcat $ punctuate comma $ map ppr args)
instance Outputable Pseudo where
    ppr psuedo = case psuedo of
        PsAssign  name code   -> hang (text name <+> text "=") 4 (ppr code) <> text ";"
        PsOp      frill args  -> ppr frill  <> pprApply args
        PsBlock   name args block  -> hang (ppr name <> pprArgs args) 4 (vcat $ map ppr block)
        PsSwitch  scrut alts other -> hang (text "switch" <+> ppr scrut) 4 (vcat (map ppr alts) $+$ pprOther other)
        PsCommand name offset -> text "load:" <+> ppr name <+> text "+" <+> ppr offset

pprOther :: [Pseudo] -> SDoc
pprOther other = case other of
                    [] -> empty
                    xs -> hang (text "otherwise:") 4 (vcat $ map ppr other)

instance Outputable PseudoAlt where
    ppr (PsAlt choice code) = hang (ppr choice <> text ":") 4 (vcat (map ppr code))

instance Outputable PseudoRecur where
    ppr (PsRecur name args applied) = text "Recur" <+> ppr name <+> (vcat $ map ppr args) <+> text "<-" <+> (vcat $ map ppr applied)
-}
