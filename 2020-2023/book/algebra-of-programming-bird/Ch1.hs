{-# LANGUAGE BangPatterns #-}
module Ch1 where

import Prelude hiding (succ)

class Injective a b where to :: a -> b

--------------------------------------------------------------------------------
-- structual recursion


-- structural recursion

data Nat = Succ Nat | Z deriving (Show, Eq)


class ToInt a where
  toInt :: a -> Int

succ :: Nat -> Nat
succ = Succ

zero :: Nat
zero = Z

nat :: (Eq t, Num t) => t -> Nat
nat 0 = zero
nat x = succ (nat (x - 1))


instance ToInt Nat where
  toInt Z = 0
  toInt (Succ n) = 1 + toInt n

-- define a general structural recursive function on nat
-- foldn inductively replace succ with h

foldn :: a -> (a -> a) -> Nat -> a
foldn c _ Z = c
foldn c h (Succ n) = h (foldn c h n)


plus, mult, expn :: Nat -> Nat -> Nat
plus m = foldn m succ
mult m = foldn zero (plus m)
expn m = foldn (succ zero) (mult m)


testNat = ((((nat 2) `plus` (nat 3)) `mult` (nat 5)) `expn` (nat 2)) `mult` (nat 20)


fact :: Nat -> Nat
fact = snd . foldn (zero, nat 1) (\(m, n) -> (m `plus` nat 1, (m  `plus` nat 1) `mult` n))

fib :: Nat -> Nat
fib = fst . foldn (zero, nat 1) (\(m, n) -> (n, m `plus` n))

--------------------------------------------------------------------------------
-- 1.3
-- an nat that rerpesents integer > 0

data Nat' = SuccFromOne Nat

one' = SuccFromOne zero
nat' n = SuccFromOne (nat (n - 1))

instance ToInt Nat' where toInt (SuccFromOne n) = 1 + toInt n

-- nat+ to nat is partial
natToNat' (Succ n) = SuccFromOne n
natToNat' Z = error "can't convert 0 to Nat'"

nat'ToNat :: Nat' -> Nat
nat'ToNat (SuccFromOne n) = (nat 1) `plus` n

idNat :: Nat' -> Nat'
idNat = natToNat' . nat'ToNat

idNat' :: Nat -> Nat
idNat' = nat'ToNat . natToNat'


--------------------------------------------------------------------------------
-- 1.4
sqr :: Nat -> Nat
sqr x = foldn zero (`plus` x) $ x


--------------------------------------------------------------------------------
-- 1.5

-- return the largest nat m <= n such that p(m)

last :: (Nat -> Bool) -> Nat -> Nat -> Nat
last p n = foldn zero undefined

--------------------------------------------------------------------------------
-- 1.6

-- ackerman function is a non primitive recursive function
ack :: (Nat, Nat) -> Nat
ack (Z, y) = y `plus` (nat 1)
ack ((Succ x), Z) = ack (x, (nat 1))
ack ((Succ x), (Succ y)) = ack (x, (ack ((Succ x), y)))


-- we can define the curried version of ackerman function using foldn.
-- This shows functions taht are not primitive recursive can be expressed in terms of foldn
ackCurried :: Nat -> Nat -> Nat
ackCurried x y = undefined -- undefined foldn succ (ack (x, y)) (nat 0)



--------------------------------------------------------------------------------
-- list


-- two possible views of lists: cons list and snoc list

-- cons list build from the right
data ListR a = Cons a (ListR a) | NilListR deriving (Show)

-- snoc list build from the left
data ListL a = Snoc (ListL a) a | NilListL deriving (Show)


-- cons list and snoc list are isomorphic

instance Injective (ListR a) (ListL a) where
  to (Cons x xs) = Snoc (to xs) x
  to NilListR = NilListL

instance Injective (ListL a) (ListR a) where
  to (Snoc xs x) = Cons x (to xs)
  to NilListL = NilListR

cons :: a -> ListR a -> ListR a
cons a (Cons x xs) = Cons a (Cons x xs)

snoc :: a -> ListL a -> ListL a
snoc a (Snoc xs x) = Snoc (Snoc xs x) a

-- concatenation reveals equational reasoning of functional programming
-- we can evaluate an expresssion by following the definition mechanically, and get a deterministic result.
-- So it's easy to have programs to help us reason our program.
instance Semigroup (ListR a) where
  NilListR <> xs = xs
  xs <> NilListR = xs
  (Cons x xs) <> (Cons y ys) = Cons x (xs <> Cons y ys)

instance Monoid (ListR a) where mempty = NilListR

instance Semigroup (ListL a) where
  NilListL <> xs = xs
  xs <> NilListL = xs
  (Snoc xs x) <> (Snoc ys y) = Snoc (Snoc xs x <> ys) y

instance Monoid (ListL a) where mempty = NilListL


-- append an element ot the end of cons list
snocr :: a -> ListR a -> ListR a
snocr a NilListR = Cons a NilListR
snocr a (Cons x xs) = Cons x (snocr a xs)

consList1 :: ListR Integer
consList1 = snocr 4 (Cons 1 (Cons 2 (Cons 3 NilListR)))


instance Functor ListR where
  fmap f NilListR = NilListR
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Functor ListL  where
  fmap f NilListL = NilListL
  fmap f (Snoc xs x) = Snoc (fmap f xs) (f x)

instance Foldable ListR where
  foldr f z NilListR = z
  foldr f z (Cons x xs) = f x (foldr f z xs)

instance Foldable ListL where
  foldl f z NilListL = z
  foldl f z (Snoc xs x) = f (foldl f z xs) x


--------------------------------------------------------------------------------
-- equational reasoning


