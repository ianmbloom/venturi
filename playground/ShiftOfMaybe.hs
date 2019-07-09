module ShiftOfMaybe
  ( Nat(..)
  , n
  , xs
  , ofMaybe
  , reverseMaybe
  , offsetK
  , aShift
  , shift
  )
where

data Nat = S Nat | Z

instance Eq Nat where
  S x == S y = x == y
  Z   == Z   = True
  _   == _   = False

instance Show Nat where
  show = show . natToInt

natToInt (S i) = natToInt i + 1
natToInt  Z    = 0

wordToNat :: Word -> Nat
wordToNat 0 = Z
wordToNat x = S (wordToNat $ x - 1)

n = wordToNat

item (x:xs) k =
    case k of
      S j -> item xs j
      Z   -> x
item [] k = error "item out of boundaries."

xs = [Just "A", Nothing, Just "B", Just "C", Nothing, Just "D"]

ofMaybe xs k i =
    case item xs k of
        Just _  -> i
        Nothing -> S i

shift f k i =
    case f k i of
        S p -> shift f (S k) p
        Z   -> k

aShift k i =
  if offsetK (reverseMaybe xs) k == S i
  then k
  else aShift (S k) i

reverseMaybe xs k i =
    case item xs k of
        Just _  -> S i
        Nothing -> i

offsetK f k =
    f k $
    case k of
      S j -> offsetK f j
      Z   -> Z
