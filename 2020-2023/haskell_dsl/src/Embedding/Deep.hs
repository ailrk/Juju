{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Embedding.Deep where

import Data.Kind

-- First we define the ast
type ExprF :: Type -> Type
data ExprF a where
  ValF :: Integer -> ExprF a
  AddF :: ExprF a -> ExprF a -> ExprF a

-- makes it a functor
instance Functor ExprF where
  fmap _ (ValF n) = (ValF n)
  fmap f (AddF n m) = AddF (fmap f n) (fmap f n)

-- Define fix point ove the functor
type Deep :: (Type -> Type) -> Type
data Deep f where
  In :: Functor f => f (Deep f) ->  Deep f

--
type Expr = Deep ExprF
