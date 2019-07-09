-- {-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE UnboxedSums         #-}

module TestFunctions_vent
  (-- Coord(..)
  --, Tree(..)
  --, mapTree
  --, translate
  --, buildTree
  --, buildTree8
  --, buildTree2
  --, mapTreeInc
  --, myFun
  add
  , myMap
  --, inc
  --, n1
  --, n2
  --, n3
  --, n4
  --, n5
  --, n6
  --, n7
  --, n8
  --, n9
  --, n10
  --, n11
  --, n12
  --, n13
  --, n14
  --, n15
  --, n16
  )
where

--import GHC.Prim
--
--
--myFun :: Int -> Int -> XY Int
--myFun x y = X x Y -- X (x + y) Y

add a b = a + b
{-
data Nat = S Nat | Z
data Coord a = Coord a a

data Tree :: * -> * where
  Leaf   :: a -> Tree a
  Branch :: Tree a -> Tree a -> Tree a
  deriving (Eq, Show)

mapTree :: (a -> b) -> Tree a-> Tree b
mapTree f tree =
    case tree of
        Branch left right -> Branch (mapTree f left) (mapTree f right)
        Leaf leafValue     -> Leaf (f leafValue)

--{-# ANN incTree8 True #-}
incTree :: Tree Int -> Tree Int
incTree = mapTree (+1)

class Translatable a where
    translate :: a -> a -> a

instance Num a => Translatable (Coord a) where
    translate (Coord dx dy) (Coord x y) = Coord (x + dx) (y + dy);

translateTree :: Num a => Coord a -> Tree (Coord a) -> Tree (Coord a)
translateTree delta = mapTree (translate delta)

buildTree :: a -> Nat -> Tree a
buildTree x n =
    case n of
        S n' -> let left  = buildTree x n'
                    right = buildTree x n'
                in  Branch left right
        Z    -> Leaf x

buildTree8 :: Int -> Tree Int
buildTree8 x = buildTree x n8

buildTree2 :: Int -> Tree Int
buildTree2 x = buildTree x n1

-- myTree :: Tree Nat
-- myTree = mapTree S $ mapTree S $ Branch (Leaf Z) (Leaf Z)

mapTreeInc :: Tree Int -> Tree Int
mapTreeInc = mapTree (+1) -- $ buildTree 3 n1

-}
data XY a = X a (XY a) | Y | Zo a

xyMap :: (a -> a) -> XY a -> XY a
xyMap f z = case z of
              X a n -> X (f a) (xyMap f n)
              Y   -> Y

inc = (+1)

myMap :: XY Int -> XY Int
myMap = xyMap (+3)
{-

n0  = Z
n1  = S n0
n2  = S n1
n3  = S n2
n4  = S n3
n5  = S n4
n6  = S n5
n7  = S n6
n8  = S n7
n9  = S n8
n10 = S n9
n11 = S n10
n12 = S n11
n13 = S n12
n14 = S n13
n15 = S n14
n16 = S n15


-}
{-
myApp f x = f x

shift :: (Nat -> Nat -> Nat) -> Nat -> Nat -> Nat
shift f o i =
    case i of
        S p -> shift f (S o) (f o p)
        Z   -> o
-}
{-
-- this is inverted so it increments the iterator on shift if there is an empty space
--shiftOfMaybe :: (N -> Maybe a) -> N -> N -> N
let shiftOfMaybe f o i  =
    case f o of
        Just x  -> i;
        Nothing -> S i;
    done

--justDuplicates :: (Show a, Eq a) => (N -> a) -> N -> Maybe a
let justDuplicates xs i =
    case xs i == xs (S i) of
       True  -> Nothing;
       False -> Just (xs i);
    done

--duplicatesRemoved :: (Show a, Eq a) => (N -> a) -> N -> a
let duplicatesRemoved xs i = xs (shift (shiftOfMaybe (justDuplicates xs)) Z i)
-}
{-
coordId p =
    case p of
        Coord x y -> Coord (x + y) y
-}

{-
let rotate a p =
  case p of
    Coord x y -> Coord (x * cos a - y * sin a) (y * cos a + x * sin a);
  done

let map f xs i = f (xs i)

let rotateAll a = map (rotate a)
-}
-- Bonepile
{-
let oneTwoThree a b c =
    case a of
        Just x -> One x;
        Nothing ->  case b of
                        Just y -> Two y y;
                        Nothing -> Three c c c;
                    done;
        done
-}
