-- Dumping ground for deleted code.

            Case scrut as o -> do ref <- uniqueRef "case_ref"
                                  let refVar = mkV ref
                                  scrut' <- noSpine (go isTop scrut)
                                  as' <- mapM (goAlt isTop refVar) as
                                  o'  <- mapM (noSpine . go isTop) o
                                  return $ lam ref $ Case (App scrut' refVar) as' (fmap (flip App refVar) o')

let deBruijn = fmap Dj.toDeBruijn toVarExpr
putStep "toDeBruijn:" DjR.pprDj deBruijn

data TemplateVar v e = NewVar | T (Lc v e)

insideLam :: String -> Lc VRef (TemplateVar VRef (DeBruijn Z) -> CMonad e (DLc VRef e)
insideLam name template =
    do ref <- uniqueRef name
       Lam ref <$> traverseLc undefined undefined unTemplate template
    where
    unTemplate goTraverse term =
       case term of
          Var NewVar -> return $ mkB Z
          Var (T embedded) -> return $ abstract embedded
          _ -> goTraverse term


-----
data LcVar v where
    OfType :: v -> Type -> LcVar v
    Lv     :: v -> LcVar v

fromLcVar :: LcVar v -> v
fromLcVar (OfType v t) = v
fromLcVar (Lv v) = v

stripType :: LcVar v -> LcVar v
stripType (OfType v t) = Lv v
stripType (Lv v)       = Lv v

lcVarType :: LcVar v -> Type
lcVarType (OfType v t) = t
lcVarType (Lv t) = voidPrimTy

instance Functor LcVar where
    fmap f (Lv v)       = Lv (f v)
    fmap f (OfType v t) = OfType (f v) t

instance (Outputable v) => Outputable (LcVar v) where
  pprPrec i var = case var of
      OfType var ty -> ppr var <+> text "::" <+> ppr ty
      Lv var -> ppr var

mapLcVar :: Monad m => (a -> m b) -> LcVar a -> m (LcVar b)
mapLcVar f (Lv a) = do b <- f a
                       return (Lv b)
mapLcVar f (OfType a t) = do b <- f a
                             return (OfType b t)


goLcVar :: [Var] -> LcVar Frill -> LcVar (DeBruijn Frill)
goLcVar stack (OfType v t) = OfType (goFrill stack v) t
goLcVar stack (Lv v) = Lv (goFrill stack v)

pprDjVarLc :: (Outputable v, Outputable e)
           => ([v],[Name]) -> DeBruijn e -> SDoc
pprDjVarLc s v = text "𝗩" <+>
  case v of
      Lv     v    -> pprDjDj s v
      OfType v ty -> pprDjDj s v <+> text "::" <+> ppr ty

reduceRecursiveMaybe :: forall v e
                     .  (Outputable v, Outputable e)
                     => TraverseSpine v (DeBruijn e)
                     -> DLc v e
                     -> DLc v e
                     -> State [DLc v e] (Maybe (DLc v e))
reduceRecursiveMaybe goNext arg term =
    case term of
        Call name (Lam v body) ->
            if null (findNonMatchRecursions body)
            then Just <$> goNext (Call name (subDj Z (const arg) (removeRecurApp body)))
            else return Nothing
        Lam v body ->
            Just <$> goNext (subDj Z (const arg) body)
        _ -> return Nothing

----------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Language.Venturi.Compile.Psuedo
  ( lcToPsuedo
  , PsuedoProgram
  )
where

import Prelude hiding ((<>))
import Language.Venturi.Unique
import Language.Venturi.Calculus
import Language.Venturi.Calculus.ToDeBruijn
import Language.Venturi.Calculus.ReduceDeBruijn
import Language.Venturi.Core.ToVarLc
import Language.Venturi.Debug
import Language.Venturi.Core.Pretty
import Outputable
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad.Identity
import Type

data Psuedo where
    PsAssign :: Unique -> Psuedo -> Psuedo
    PsCon    :: Choice -> [Unique] -> Psuedo
    PsConst  :: Frill  -> [Unique] -> Psuedo
    PsBlock  :: Unique -> [Unique] -> [Psuedo] -> Psuedo
    PsSwitch :: Unique -> [PsuedoAlt] -> [Psuedo] -> Psuedo
    PsLoad   :: Unique -> Int -> Psuedo

data PsuedoRecur where
    PsRecur :: Unique -> [Unique] -> [Unique] -> PsuedoRecur

data PsuedoAlt = PsAlt Choice [Unique] [Psuedo]

data PsuedoProgram = PsP [Psuedo] (Maybe Unique) [PsuedoRecur]

instance Outputable PsuedoProgram where
    ppr (PsP code name recurs) = hang (text "Recurs:") 4 (vcat (map ppr recurs)) $+$
                                 hang (text "Program:") 4 (vcat (map ppr code))

pprArgs args = parens (hcat $ punctuate comma $ map ppr args)
instance Outputable Psuedo where
    ppr psuedo = case psuedo of
        PsAssign name code   -> hang (ppr name <+> text "=") 4 (ppr code) <> text ";"
        PsCon    choice args -> ppr choice <> pprArgs args
        PsConst  frill args  -> ppr frill  <> pprArgs args
        PsBlock  name args block  -> hang (ppr name <> pprArgs args) 4 (vcat $ map ppr block)
        PsSwitch scrut alts other -> hang (text "switch" <+> ppr scrut) 4 (vcat (map ppr alts) $+$ pprOther other)
        PsLoad   name offset -> text "load:" <+> ppr name <+> text "+" <+> ppr offset

pprOther :: [Psuedo] -> SDoc
pprOther other = case other of
                    [] -> empty
                    xs -> hang (text "otherwise:") 4 (vcat $ map ppr other)

instance Outputable PsuedoAlt where
    ppr (PsAlt choice args code) = hang (ppr choice <> pprArgs args <> text ":") 4 (vcat (map ppr code))

instance Outputable PsuedoRecur where
    ppr (PsRecur name args applied) = text "Recur" <+> ppr name <+> (vcat $ map ppr args) <+> text "<-" <+> (vcat $ map ppr applied)


lcToPsuedo :: DLc Name Frill -> PsuedoProgram
lcToPsuedo term = let (code, name, recurs) = runIdentity $ evalUnique (go 300 [] [] [] $ trPW pprDj "start term" term)
                  in PsP code name recurs
    where
    go :: Int -> [(Unique,[Unique])] -> [Unique] -> [Unique] -> DLc Name Frill -> UniqueM ([Psuedo], Maybe Unique, [PsuedoRecur])
    go i calls stack spine term = if i > 0
                                  then goTerm (i-1) calls stack spine term
                                  else return ([], Nothing, [])
    goTerm :: Int ->  [(Unique,[Unique])] ->  [Unique] -> [Unique] -> DLc Name Frill -> UniqueM ([Psuedo], Maybe Unique, [PsuedoRecur])
    goTerm i calls stack spine term =
        trP ("case term" ++ (docShow $ pprDj term) ++ "\n----------\n") <$> case term of
            Lam  v body -> case spine of
                              [] -> error "unApplied lambda"
                              _  -> go i calls ((head spine):stack) (tail spine) body
            Var v -> case fromLcVar v of
                 Bound n -> let name = case natIndex n stack of
                                        Just name -> name
                                        Nothing   -> error "catToPsuedo encountered unbound var."
                         in  return ([], Just name, [])
                 Unbound fr -> case fr of
                                FrillLit i  -> do litName <- unique "lit"
                                                  return ([PsAssign litName (PsConst fr spine)], Just litName, [])
                                FrillType ty   -> return ([], Nothing, [])
                                FrillCoerce cs -> return ([], Nothing, [])
                                FrillVar v     ->  do varName <- unique "var"
                                                      return ([PsAssign varName (PsConst fr spine)], Just varName, [])

            App  f x       -> do  (xCode, xName, xRecur) <- go i calls stack []            x
                                  (fCode, fName, fRecur) <- go i calls stack (maybeCons xName spine) f
                                  return (xCode ++ fCode, fName, xRecur ++ fRecur)
            Case s as other -> do name <- unique "scrut"
                                  (sCode, sName, sRecurs) <- go i calls stack [] s
                                  (altCodes, _, altRecurs) <- unzip3 <$> mapM (goAlt i calls stack []) as
                                  (otherCode, _, otherRecurs) <- maybe (return ([],Nothing,[])) (go i calls stack []) other
                                  tagName <- unique "tag"

                                  return (sCode ++ [PsAssign tagName (PsLoad (fromJust sName) 0)
                                                   ,PsSwitch tagName altCodes otherCode], Nothing, concat altRecurs ++ otherRecurs)
            Select choice  -> do  conName <- unique "con"
                                  return ([PsAssign conName (PsCon choice spine)], Just conName, [])
            Call name body -> do  callName <- unique (unNm name)
                                  (args, body') <- collectLambdas body
                                  (bCode, bName, bRecurs) <- go i ((callName,args):calls) (args++stack) spine body'
                                  return ([PsBlock callName args bCode], Nothing, bRecurs)
            Recur i        -> do  let (call, args) = fromJust $ natIndex i calls
                                  return ([], Nothing, [PsRecur call args spine])
    goAlt i calls stack spine (Alt choice body) = do (args, body') <- collectLambdas body
                                                     (bCode, bName, bRecurs) <- go i calls (args ++ stack) spine body'
                                                     return (PsAlt choice args bCode, bName, bRecurs)

collectLambdas (Lam v body) = do (args, body') <- collectLambdas body
                                 uv <- unique . unNm . fromLcVar $ v
                                 return (uv:args, body')
collectLambdas term         = return ([], term)







---------------


uncurryLc :: LcVar v -> LcVar v -> CLc v e -> CLc v e -> CLc v e
uncurryLc aN bN f pair = Case pair
                            [Alt pairChoice . Lam aN . Lam bN
                            $ mkApps f [mkVarDj (S Z), mkVarDj Z]] Nothing

--storeTag choice = return Lam refName Nothing $ Var (Poke Nil (STag choice))

natSequence :: [Nat]
natSequence = natSequence' Z
natSequence' n = n:natSequence' (S n)


storeArg :: Nat -> Type -> CLc v e
storeArg n s = mkVarDj n `App` mkOff s `App`  mkVarDj Z


appAll :: [Lc v e] -> Lc v e
appAll (x:xs) = mkApps x xs

appendVar :: [CLc v e] -> [CLc v e]
appendVar = (++ [mkVarDj Z])


offsetList :: [Type] -> [CLc v e]
offsetList ss = map (appAll . appendVar . map (mkOff)) $ growList [] ss



intList = repeat intPrimTy

offsets :: String
offsets = let x :: [CLc Name Var]
              x = offsetList $ take 4 intList
          in  showSDocUnsafe . vcat . map ppr $ x








splitLast :: [a] -> ([a],a)
splitLast [y] = ([],y)
splitLast (x:xs) =
    let (xss,y) = splitLast xs
    in  (x:xss,y)
splitLast [] = error "splitLast must have at least one element"

spineToList :: Type -> [Type]
spineToList (a `TArr` b) = [a] ++ spineToList b
spineToList x            = [x]

inferAlt :: TVar -> Alt I TVar -> Infer BuiltIn (Type, [Constraint])
inferAlt tv (Alt cName term) =
    do  (t1, c1) <- inferTerm term
        let (args, ret) = splitLast . spineToList $ t1
        tName <- lookupConstructor cName
        return (TSum (SumType tName [Sa cName (Just c1)]), ret ++ c1)

        in
        case findAlt env scrut' alts of
            Just expr -> go env spine expr
            Nothing   -> let alts' = map (expandAlt env) alts
                         in (S.Case scrut' v ty alts', spine)

    expandApp :: Env (Expr Var) -> [Lc] -> Expr Var -> (Lc, [Lc])
    expandApp env spine expr = (uncurry S.mkApps $ go env spine expr, [])

    expandLam :: Env (Expr Var) -> [Lc] -> Var -> Expr Var -> (Lc, [Lc])
    expandLam env (x:xs) v body = undefined -- mapFst (S.substitute v x) . go env xs $ body
    expandLam env []     v body = mapFst (S.Lam v) . go env [] $ body


    findAlt :: Env (Expr Var) -> Expr Var -> [Alt Var] -> Maybe (Expr Var)
    findAlt env expr alts = undefined -- join $ find isJust (map (altMatch env expr) alts)

    altMatch :: Env (Expr Var) -> Expr Var -> Alt Var -> Maybe (Expr Var)
    altMatch env scrut (altCon, altVars, altBody)
      | (Just scrutLit) <- isLiteralMaybe scrut
      , (LitAlt patternLit) <- altCon
      , patternLit == scrutLit = Just altBody
      | (Just (scrutDataCon, conAppExprs)) <- isDataConAppMaybe scrut
      , (DataAlt patternDataCon) <- altCon
      , patternDataCon == scrutDataCon =
          let varLams   = mkLams altVars
              tyVarLams = mkLams (dataConUnivTyVars scrutDataCon)
          in  Just (mkApps (tyVarLams . varLams $ altBody) conAppExprs)
      | otherwise = Nothing

isDataConAppMaybe :: Expr Var -> Maybe (DataCon, [Expr Var])
isDataConAppMaybe expr = go [] expr
  where
    go spine expr = case expr of
        App f x -> go (x:spine) f
        Var v -> case isDataConId_maybe v of
                    Just dataCon -> Just (dataCon, spine)
                    Nothing -> Nothing
        _     -> Nothing

isLiteralMaybe (Lit lit) = Just lit
isLiteralMaybe _         = Nothing

         --instances <- getInsts
         --liftIO $ putStrLn $ pprShow instances

{-# LANGUAGE ScopedTypeVariables #-}

module Language.Venturi.Calculus.Reduce
  ( reduce
  )
where

import Language.Venturi.Calculus
import Language.Venturi.Core.ToVarLc
import Language.Venturi.Calculus.Tools
import Language.Venturi.Debug
import Var
import Type
import Outputable

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

type Retrans spine v e = Lc v e -> [spine] -> (Lc v e, [spine])

traverseCalculus :: forall spine v e .
                    Retrans spine v e
                 -> Retrans spine v e
                 -> Lc v e -> Lc v e
traverseCalculus before after term = fst $ go term []
  where
    go :: Lc v e -> [spine] -> (Lc v e, [spine])
    go term spine = uncurry after
                  . uncurry goTraverse
                  . uncurry before
                  $ (term, spine)
    goTraverse :: Lc v e -> [spine] -> (Lc v e, [spine])
    goTraverse term spine =
        case term of
            Lam  v body     -> mapFst (Lam v) $ go body spine
            Var  v          -> (Var v, spine)
            App  f x        -> (App  (fst $ go f spine) (fst $ go x spine), spine)
            Prod a b        -> (Prod (fst $ go a spine) (fst $ go b spine), spine)
            Ex   d          -> (Ex d, spine)
            Case scrut as o -> (Case (fst $ go scrut spine)
                                (map (fst . flip goAlt spine) as)
                                (fmap (fst . flip go spine) o), spine)
            Select choice   -> (Select choice, spine)
            Call name body  -> (Call name (fst $ go body spine), spine)
            Recur i tys     -> (Recur i tys, spine)
    goAlt :: LcAlt v e -> [spine] -> (LcAlt v e, [spine])
    goAlt (Alt choice body) spine = (Alt choice $ fst $ go body spine, spine)

reduce :: forall spine v e
       .  (Outputable v, Outputable e, Outputable spine)
       => (v -> spine -> Lc v e -> Lc v e)
       -> (Lc v e -> Maybe spine)
       -> (spine -> Lc v e)
       -> (v -> Bool)
       -> Retrans spine v e
       -> Lc v e -> Lc v e
reduce applyRedex toSpineMaybe fromSpine varIsSpine transform =
    traverseCalculus before after
    where
    before :: Lc v e -> [spine] -> (Lc v e, [spine])
    before term spine = uncurry transform
                      . uncurry goLam
                      . uncurry goApp
                      $ (term, spine)

    goLam :: Lc v e -> [spine] -> (Lc v e, [spine])
    goLam term spine =
        case term of
            Lam v body -> if varIsSpine v && (not . null $ spine)
                          then before (applyRedex v (head spine) body) (tail spine)
                          else (term, spine)
            _ -> (term, [])

    goApp :: Lc v e -> [spine] -> (Lc v e, [spine])
    goApp term spine =
        case term of
          App f x -> case toSpineMaybe x of
                        Just s -> before f (s:spine)
                        Nothing -> (term, spine)
          _ -> (term, spine)

    after :: Lc v e -> [spine] -> (Lc v e, [spine])
    after term spine = (mkApps term (map fromSpine spine), [])

reduceDeBruijn :: forall v e
               .  (Outputable v, Outputable e)
               => DLc v e -> DLc v e
reduceDeBruijn = reduce mapDj Just id (const True) checkRecursiveCall
    where
    mapDj :: v -> DLc v e -> DLc v e -> DLc v e
    mapDj _ sub = subDeBruijn Z sub

    checkRecursiveCall :: (DLc v e -> Reduce (DLc v e) (DLc v e))
                       -> DLc v e
                       -> Reduce (DLc v e) (DLc v e)
    checkRecursiveCall go term =
        case term of
            Call name body -> if isRecursive body
                              then go term
                              else go body
            _ -> go term

orMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
orMaybe f (Just a) (Just b) = Just (f a b)
orMaybe f (Just a) Nothing  = Just a
orMaybe f Nothing  (Just b) = Just b
orMaybe f Nothing  Nothing  = Nothing

trP (docShow (text "goCall[" $+$ (vcat (map ppr spine)) $+$ text "]>>>"))

foldCallLc :: forall v n a . (Nat -> Nat -> Lc v n -> Maybe a) -> (a -> a -> a) -> Lc v n -> Maybe a
foldCallLc from rawOp term = go Z Z term
    where
    op :: Maybe a -> Maybe a -> Maybe a
    op (Just a) (Just b) = Just (a `rawOp` b)
    op (Just a) Nothing  = Just a
    op Nothing  (Just b) = Just b
    op Nothing  Nothing  = Nothing
    go :: Nat -> Nat -> Lc v n -> Maybe a
    go var level term =
        op (from var level term) $
        case term of
            Lam  v body     -> go (S var) level body
            Var  v          -> Nothing
            App  f x        -> go var level f `op` go var level x
            Prod a b        -> go var level a `op` go var level b
            Ex   d          -> Nothing
            Case scrut as o -> go var level scrut `op` foldl op Nothing (map (goAlt var level) as) `op` maybe Nothing (go var level) o
            Select choice   -> Nothing
            Call name body  -> go var (S level) body
            Recur i         -> Nothing
    goAlt :: Nat -> Nat -> LcAlt v n -> Maybe a
    goAlt var level (Alt _ body) = go var level body

checkRecur var level term =
  case term of
      Recur i -> if i == level then Just True else Just False
      _ -> Nothing

isRecursive :: Lc v n -> Bool
isRecursive term = fromMaybe False (foldCallLc checkRecur (||) term)

mapOverType :: Var -> Type -> VarLc -> VarLc
mapOverType match sub = mapVar $ mapFrillType $ substituteType match sub


--------------------
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs      #-}


module Language.Venturi.Compile.LcToCat
  ( Program(..)
  , ccc
  )
where

import Prelude hiding ((<>))
import Language.Venturi.Calculus
import Language.Venturi.Core.ToVarLc
import Language.Venturi.Calculus.ToDeBruijn
import Language.Venturi.Calculus.ReduceDeBruijn
import Language.Venturi.Calculus.Tools
import Language.Venturi.Debug
import Language.Venturi.Core.Pretty
import Literal
import Outputable

-- abst: add a level of abstraction to a term by increasing the variable indexes for a term.
abst :: Lc v Nat -> Lc v Nat
abst term =
  case term of
    Lam v body     -> Lam v (abst body)
    Var i          -> Var (S i)
    App a b        -> App (abst a) (abst b)
    Case scrut alts other -> Case (abst scrut) (map abstAlt alts) (fmap abst other)
    Select c       -> Select c
    Call name body -> Call name (abst body)
    Recur i        -> Recur i

abstAlt :: LcAlt v Nat -> LcAlt v Nat
abstAlt (Alt choice body) = Alt choice (abst body)

data Program = Program [Op]

data Op where
  IdC      :: [Name] -> Op
  ProdC    :: [[Op]] -> Op
  ExC      :: Nat -> Op
  SwitchC  :: [OpAlt] -> Op
  SelectC  :: Choice -> Op
  ApplyC   :: Op
  CurryC   :: Op
  UncurryC :: Op
  ConstC   :: Frill -> Op
  CallC    :: Name -> [Op] -> Op
  RecurC   :: Nat -> Op



data OpAlt = OpAlt Choice [Op]
           | OpDefault [Op]

fmapAlt f (Alt choice body) = Alt choice (f body)

reduceVar (Var (Bound (S v))) = Var (Bound v)


composeC f x = f:x

listOf x = [x]

ccc :: Lc Name (DeBruijn Frill) -> [Op]
ccc term = go 300 (mapName listOf term)
  where
  go i term = if i > 0
              then goTerm (i-1) term
              else []
  goTerm i term = undefined
  {-
    case {-trDj (text "ccc") -} term of
      Lam p (Lam q body) -> composeC CurryC $
                             go i (Lam (p ++ q)
                                      (subDj (S Z) (App (Ex Z) . reduceVar) $
                                       subDj    Z  (App (Ex (S Z))            ) $
                                       body)
                                 )
      Lam p (Var (Bound Z))    -> [IdC p]
      Lam p (Var (Unbound fr)) -> [ConstC fr]
      Lam p (App  a b)    -> composeC ApplyC (go i (Lam p (Prod [a, b])))
      Lam p (Case s as o) -> composeC ApplyC $
                             [ProdC [[SwitchC (map (goAlt i . fmapAlt (Lam p)) as ++ goDefault i (fmap (Lam p) o))],
                                   (go i (Lam p s))]]
      Lam p (Call name body) -> [CallC name $ go i (Lam p body)]
      Lam p (Recur i)        -> [RecurC i]
      Lam p x             -> go i x
      Var  (Bound _)         -> error "Ccc encountered unbound var."
      Var  (Unbound fr)      -> [ConstC fr]
      App  a b            -> composeC ApplyC (go i (Prod [a,b]))
      Case s as o         -> composeC (SwitchC (map (goAlt i) as ++ goDefault i o)) (go i s)
      Select c            -> [SelectC c]
      Call name body      -> [CallC name $ go i body]
      Recur i             -> [RecurC i]
  -}
  goDefault :: Int -> Maybe (Lc [Name] (DeBruijn Frill)) -> [OpAlt]
  goDefault i Nothing  = []
  goDefault i (Just x) = [OpDefault $ go i x]

  goAlt i (Alt choice body) = OpAlt choice $ go i body


pprStack :: [Op] -> SDoc
pprStack opStack = vcat $ map ppr opStack

instance Outputable Program where
  ppr (Program ops) = hang (text "program") 4 (vcat (map ppr ops))

instance Outputable Op where
  ppr op = case op of
      IdC name       -> text "id" <+> ppr name
      ProdC a        -> (vcat ( text "✕ ------------":
                                map (nest 4 .  pprStack) a)
                              )
      ExC direction  -> ppr direction
      SwitchC alts   -> hang (text "switch") 4 (vcat (map ppr alts))
      SelectC choice -> text "select" <+> ppr choice
      ApplyC         -> text "@              "
      CurryC         -> text "curry"
      UncurryC       -> text "uncurry"
      ConstC i       -> text "const" <+> ppr i
      CallC name body-> hang (text "Ⓒ" <+> ppr name) 4 (pprStack body)
      RecurC i       -> text "Ⓡ" <+> ppr (natToInt i)

instance Outputable OpAlt where
  ppr alt = case alt of
    OpAlt choice ops -> (ppr choice <+> text "->") $+$ nest 4 (pprStack ops)
    OpDefault ops -> (text "otherwise ->") $+$ nest 4 (pprStack ops)
