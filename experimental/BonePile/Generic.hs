


data Product a b = Product a b
data Sum a b = Left a | Right b
newtype Comp g f a = Comp1 (g (f a))

data Void a
newtype Unit a = Unit
newtype Par a = Par a


class Generic1 f where
  type Rep1 f :: * -> *
  from1 :: f a -> Rep1 f a
  to1 :: Rep1 f a -> f a

class Pointer f where
    pointer :: f -> (f, Int)

instance (Pointer a, Pointer b) => Pointer (Product a, Product b) where
    pointer (Product a b) = pointer a ++ pointer b

instance (Pointer a, Pointer b) => Sum a b where
    pointer (Left a)  = (Left a,  pointer a)
    pointer (Right b) = (Right a, pointer b)

instance (Pointer g, Pointer f, Pointer a) => Pointer (Comp g f a) where
    pointer (Comp a) = pointer a


product a x a mapped to b x b where b = a | (a x a)
is there a generic way to make b x b in

-- All machine memory is just an array of bytes. Any data structure we put on top of that.
