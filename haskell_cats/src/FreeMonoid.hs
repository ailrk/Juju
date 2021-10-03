{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module FreeMonoid where

import           Iso
-------------------------------------------------------------------------------
-- free monoid
-------------------------------------------------------------------------------

{- A free monoid is the monoid with elements are all finite sequences of 0 or
   more elements from that set. (string with concatenation)
   Identity is empty string

   given a set A, the free monid is denoted as A*

   Any monoid iso to free monoid is described as free.
-}


data Seq = Seq Char Seq | Empty

seq1 = Seq 'A' (Seq 'B' (Seq 'C' (Seq 'D' Empty)))

-- note <> is non communative
instance Semigroup Seq where
  Empty <> Empty   = Empty
  Empty <> xs      = xs
  xs <> Empty      = xs
  (Seq a as) <> bs = Seq a (as <> bs)

instance Monoid Seq where
  mempty = Empty

-- Sequence is just a string.
instance Injective String Seq where
  to []     = Empty
  to (x:xs) = Seq x (to xs)

instance Injective Seq String where
  to (Seq x xs) = x:to xs
  to Empty      = []

instance Iso Seq String

-------------------------------------------------------------------------------
-- natural number including zero (N0, +)
-------------------------------------------------------------------------------
{-
   map sequence from 1, 1+1, 1+1+1... to their value in (N0, +)
   in this case.
-}

-- nat with zero
data Nat = Z | S Nat

nat2Integer :: Nat -> Integer
nat2Integer Z     = 0
nat2Integer (S n) = 1 + nat2Integer n

instance Num Nat where
  m + n = fromInteger $ nat2Integer m + nat2Integer n
  m * n = fromInteger $ nat2Integer m * nat2Integer n
  fromInteger 0 = Z
  fromInteger n = S (fromInteger (n - 1))
  abs = error "nat err"
  signum = error "nat err"
  negate = error "nat err"

instance Semigroup Nat where
  (<>) = (+)

instance Monoid Nat where
  mempty = 0

-- sequence of 1, 1+1, ...


-------------------------------------------------------------------------------
-- history monoid
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- trace monoid
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- endo moprphism of free monoid
-------------------------------------------------------------------------------
-- endomorphism is homomorphism from a math object to itself.

class Endo a where
  appEndo :: a -> a


