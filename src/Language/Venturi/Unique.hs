module Language.Venturi.Unique
  ( Unique(..)
  , UniqueSupplyT
  , UniqueM
  , UniqueSupply(..)
  , emptyUniqueSupply
  , unique
  , evalUnique
  , runUnique
  )
where

import Control.Monad.State
import Control.Monad.Identity
import Outputable
import qualified Data.Map as M

evalUnique :: (Monad m) => UniqueSupplyT m a -> m a
evalUnique m = evalStateT m emptyUniqueSupply

runUnique :: (Monad m) => UniqueSupplyT m a -> UniqueSupply -> m (a, UniqueSupply)
runUnique m s = runStateT m s

data Unique = Uq String Int deriving (Eq, Ord)

instance Show Unique where
  show (Uq name i) = name ++ if i == 0
                             then ""
                             else show i

instance Outputable Unique where
  ppr u = text (show u)

type UniqueSupply = M.Map String Int

emptyUniqueSupply :: UniqueSupply
emptyUniqueSupply = M.empty

type UniqueSupplyT m = StateT UniqueSupply m
type UniqueM = UniqueSupplyT Identity

unique :: Monad m => String -> UniqueSupplyT m Unique
unique prefix = do
    supply <- get
    let p = case M.lookup prefix supply of
               Just i -> i + 1
               Nothing -> 0
    put $ M.insert prefix p supply
    return $ Uq prefix p
