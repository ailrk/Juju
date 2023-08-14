{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFunctor #-}

module FixPoint where

import           Control.Arrow

-- - we can factor out the recursion part out from a recursive function and
--   rewrite it with fix point.
-- - same idea applies to type. We can factor out a inductive type to make it
--   flat.

data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)


-- Expr has no recursive part.
data Expr a
  = Index a a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Lit
  deriving (Show, Eq, Functor)

-- Type level recursion
data Fix f = In (f (Fix f))

-- out is a helper function to evaluate the Term f
out :: Fix f -> f (Fix f)
out (In t) = t


-- fold over expr
foldF :: Functor f => (f a -> a) -> Fix f -> a
foldF f (In expr) = f (fmap (foldF f) expr)

-- fold over an expr
test1 :: Monoid a => a
test1 = foldF (const mempty) (In (Paren undefined))

