module Language.Venturi.Type.Monad
  ( TypeError(..)
  , Infer(..)
  , runInfer
  , evalInfer
  , fresh
  )
where

import Language.Venturi.Term
import Language.Venturi.Type
import Language.Venturi.Kind
import Language.Venturi.Type.Env
import Language.Venturi.Type.Constraint
import Language.Venturi.Unique

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

data TypeError
  = UnificationFail Type Type
  | InfiniteType TyVar Type
  | UnboundVariable (Either LName VName)
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  | SumTypeMismatch TyName TyName
  | CallEncountered
  deriving (Show)

-- | Inference monad
type Infer b a = (ReaderT
                 (Env b)             -- Typing environment
                 (UniqueSupplyT      -- Type Variable Supply
                 (Except TypeError)) -- Inference errors
                 a)                  -- Result

-- | Run the inference monad
runInfer :: Env b -> UniqueSupply -> Infer b a -> Either TypeError (a, UniqueSupply)
runInfer env supply m = runExcept $ runUnique (runReaderT m env) supply

evalInfer :: Env b -> UniqueSupply -> Infer b a -> Either TypeError a
evalInfer env supply m = let e = runExcept $ runUnique (runReaderT m env) supply
                         in  case e of
                               Left err -> Left err
                               Right (a, _) -> Right a

fresh :: String -> Infer b TyVar
fresh prefix = do
    u <- lift $ unique prefix
    return $ TyVar Star u
