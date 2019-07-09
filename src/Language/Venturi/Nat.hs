module Language.Venturi.Nat
  (Nat(..)
  , natLength
  , natToInt
  , natElem
  , natIndex
  )
where

import Outputable

data Nat = Z | S Nat deriving (Show, Eq)

instance Ord Nat where
  compare Z Z = EQ
  compare (S _) Z = GT
  compare Z (S _) = LT
  compare (S n) (S m) = compare n m

natLength :: [a] -> Nat
natLength [] = Z
natLength (x:xs) = S (natLength xs)
natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S n) = 1 + natToInt n

natElem :: Eq a => a -> [a] -> Maybe Nat
natElem x (y:ys) = if x == y
                   then Just Z
                   else fmap S (natElem x ys)
natElem x []     = Nothing

natIndex :: Nat -> [a] -> Maybe a
natIndex (S n) (x:xs) = natIndex n xs
natIndex (S n) [] = Nothing
natIndex Z (x:_) = Just x
natIndex Z [] = Nothing
