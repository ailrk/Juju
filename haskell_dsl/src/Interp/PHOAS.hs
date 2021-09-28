{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Interp.PHOAS where

{-
   PHOAS
   A slightly different form of HOAS.
   Using lambda datatype parameterized over the binder types. Evaluation
   requires unpacking into a separate value type to wrap the lambda expression.
-}

data Expr a
  = Var a
  | App (Expr a) (Expr a)
  | Lam (a -> Expr a)
  | Lit Integer

data Value
  = Vlit Integer
  | VFun (Value -> Value)

