{-# LANGUAGE GADTs #-}

module Syntax where


data Expr a where
  Pure :: a -> Expr a
  Lam :: (Expr a -> Expr b) -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b
  Tup :: Expr a -> Expr b -> Expr (a, b)
  Fix :: Expr (a -> a) -> Expr a


eval :: Expr a -> a
eval (Pure a) = a
eval (Tup a b) = (eval a, eval b)
eval (Lam f) = \x -> eval (f (Pure x))
eval (App f a) = (eval f) (eval a)
eval (Fix f) = (eval f) (eval (Fix f))
