{-# OPTIONS_GHC -Wno-missing-methods #-}
module Sharing.Sharing where


tree1 :: (Eq a, Num a, Num b) => a -> b
tree1 0 = 1
tree1 a = tree1 (a - 1) + tree1 (a - 1)

tree :: (Eq a, Num a, Num b) => a -> b
tree 0 = 1
tree a = let shared = tree (a - 1)
          in shared + shared

-- >>> tree 50
-- 1125899906842624

data Expr = Lit Int | Add Expr Expr deriving (Show, Eq, Ord)

-- idea: overload typeclass to get operators generate the abstract syntax.
-- use eval to execute the syntax.

instance Num Expr where
  fromInteger = Lit . fromInteger
  (+) = Add
  negate (Lit n) = Lit (negate n)
  negate (Add a b) = Add (negate a) b

eval (Lit n) = n
eval (Add a b) = eval a + eval b

-- >>> 1 + 1 :: Expr
-- Add (Lit 1) (Lit 1)

-- >>> eval (1 + 1 :: Expr)
-- 2
