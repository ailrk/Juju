{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Interp.HOAS where

import           Prelude       hiding (succ)


{-
   Higher order abstract syntax.
   A technique for implementing the lambda calculus in a langauge where the
   binders of the lambda expression map direcly onto lambda binders of the host
   language to give substitution machinery in custom language by exploiting the
   host language's implementation.
-}


-------------------------------------------------------------------------------
-- HOAS representation
-------------------------------------------------------------------------------
import           Control.Monad (ap)
data Expr a where
  Con :: a -> Expr a
  Lam :: (Expr a -> Expr b) -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b

-------------------------------------------------------------------------------
-- base line interpreter for HOAS
-------------------------------------------------------------------------------
eval :: Expr a -> a
eval (Con v)     = v
eval (Lam f)     = \x -> eval (f (Con x))
eval (App e1 e2) = (eval e1) (eval e2)

-------------------------------------------------------------------------------
-- define some combinatorial logic with the new lambda calculus DSL.
-------------------------------------------------------------------------------
i :: Expr (a -> a)
i = Lam $ \x -> x

k :: Expr (a -> b -> a)
k = Lam $ \x ->
    Lam $ \_ -> x

s :: Expr ((a -> b -> c) -> (a -> b) -> (a -> c))
s = Lam $ \x -> Lam $ \y -> Lam $ \z -> App (App x z) (App y z)

skk = App (App s k) k

-------------------------------------------------------------------------------
-- Make the language monadic.
-------------------------------------------------------------------------------
instance Functor Expr where
  fmap f x = Con . f $ eval x

instance Applicative Expr where
  pure a = Con a
  (<*>) = ap

instance Monad Expr where
  return = pure
  m >>= k = k . eval $ m

-------------------------------------------------------------------------------
-- define some short hand for creating expressions.
-------------------------------------------------------------------------------
class LC m where
  apply :: m (a -> b) -> m a -> m b
  lambda :: (m a -> m b) -> m (a -> b)
  val :: a -> m a

instance LC Expr where
  apply = App
  lambda = Lam
  val = Con

-------------------------------------------------------------------------------
-- define church encoding with new dsl.
-------------------------------------------------------------------------------

z :: Expr ((a -> b) -> a -> a)
z = lambda . const . lambda $ id

succ :: Expr (((a -> c) -> b -> a) -> (a -> c) -> b -> c)
succ = lambda $ \n -> lambda $ \f -> lambda $ \x -> f <*> (n <*> f <*> x)

plus :: Expr ((a -> b) -> (a -> c) -> (c -> a) -> c -> b)
plus = lambda $ \m -> lambda $ \n -> lambda $ \f -> lambda
     $ \x -> m <*> (f <*> (n <*> (f <*> x)))

mult n = lambda $ \m -> lambda $ \n -> lambda $ \f -> lambda $ \x ->
         m <*> (n <*> (f <*> x))

exp :: Expr (a -> (a -> b) -> b)
exp = lambda $ \m -> lambda $ \n -> n <*> m

pred :: Expr ((((c -> a) -> (a -> b) -> b) -> (x -> z) -> (y -> y) -> d) -> c -> z -> d)
pred =
  lambda $ \n -> lambda $ \f -> lambda $ \x ->
    n <*> (lambda $ \g -> lambda $ \h ->
             h <*> (g <*> f))
      <*> (lambda $ \u -> x)
      <*> (lambda $ \u -> u)

n1 = succ <*> z
n2 = succ <*> n1
n3 = succ <*> n2
n4 = succ <*> n3
n5 = succ <*> n4
n6 = succ <*> n5
n7 = succ <*> n6
n8 = succ <*> n7
n9 = succ <*> n8

toInt n = eval $ n <*> lambda ((+1) <$>) <*> (val 0)

-- >>> toInt n4
-- 4
