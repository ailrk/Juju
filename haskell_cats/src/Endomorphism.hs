{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Endomorphism where

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Iso

{- abstract:
     endomorphism is simply homomorphism back to self.
     written as f: A -> A
     we care about endomorphism because:
       1. It simplify type A -> A. to Endo A
       2. there is a monoid of endomorphism under composition
          it helps us chain list of functions act on same type.
       3. we can generalize the composition of endomorphism under
          kleisli composition to compose effects.
       4. Further generalize kleisli endomorphism we can get StateT monad.
-}

--------------------------------------------------------------------------------
-- endo moprphism
-------------------------------------------------------------------------------
-- endomorphism is homomorphism from a math object to itself. with type
-- f: A -> A.

-- endo morphism itself is composable, we can compose endomorphism with normal
-- function composition, and chain long operations like
-- A -> A -> A -> .. -> A

-- - the composition of any two endomorphism of X is also an endomorphism of X

type E a = a -> a
newtype Endo a = Endo { appEndo :: a -> a }

-- | flip
runEndo :: a -> Endo a -> a
runEndo x (Endo f) = f x

mapEndo :: (E a -> E b) -> Endo a -> Endo b
mapEndo f (Endo a) = Endo (f a)

liftEndo :: Functor f => Endo a -> Endo (f a)
liftEndo (Endo f) = Endo (fmap f)

-- monoid of endomorphism under composition
instance Semigroup (Endo a) where (Endo a) <> (Endo b) = Endo (a . b)
instance Monoid (Endo a) where mempty = Endo id

--------------------------------------------------------------------------------
-- endomorphism e.g 1
-------------------------------------------------------------------------------

-- order is the same as the list.
endo1 = foldMap Endo [(1+), (3-), (5-)]
endo2 = foldMap Endo [("1"++), ("2"++), ("3"++)]

-- >>> appEndo endo1 10     --  1 + (3 - (5 - (10)))
-- 9

-- this is the same as foldr
-- >>> appEndo endo2 "0"
-- "1230"

--------------------------------------------------------------------------------
-- Kesli endomorphism
-------------------------------------------------------------------------------

newtype KEndo m a = KEndo { appKEndo :: a -> m a }

instance Monad m => Semigroup (KEndo m a) where
  (KEndo a) <> (KEndo b) = KEndo (a >=> b)

instance Monad m => Monoid (KEndo m a) where
  mempty = KEndo return

concatKEndos :: Monad m => [a -> m a] -> a -> m a
concatKEndos = appKEndo . foldMap KEndo

kendo1 = concatKEndos [ \n -> putStrLn n >> return n
                      , \n -> putStrLn n >> return (n ++ "v1")
                      , \n -> putStrLn n >> return (n ++ "v2") ]

-- 1 2
kendo2 = concatKEndos [ \() -> putStrLn "1", \() -> putStrLn "2"]


--------------------------------------------------------------------------------
-- Kesli endomorphism is isomorphic to stateT monad
-------------------------------------------------------------------------------
-- https://chrispenner.ca/posts/kleisli-endo

-- some setup. Say we have a state monad.
newtype StateT s m a = StateT { runState :: s -> m (s, a) }
instance Monad m => Functor (StateT s m) where
  fmap f (StateT r) = StateT $ \s -> do (s1, a) <- r s; return (s1, f a)
instance Monad m => Applicative (StateT s m) where pure = return; (<*>) = ap
instance Monad m => Monad (StateT s m) where
  return a = StateT $ \s -> return (s, a)
  st >>= f = StateT $ \s -> do (s1, a) <- runState st s; runState (f a) s1
newtype Identity a = Identity { unIdentity :: a }
instance Functor Identity where fmap f (Identity a) = Identity (f a)
instance Applicative Identity where pure = return; (<*>) = ap
instance Monad Identity where return = Identity; (Identity a) >>= f = f a

type State s a = StateT s Identity a

-- first, State s () ~= Endo s
-- s -> s ~= s -> ((), s)
instance Injective (Endo s) (State s ()) where to (Endo endo) = StateT $ \s -> return (endo s, ())
instance Injective (State s ()) (Endo s) where to st = Endo $ fst . unIdentity . runState st
instance Iso (Endo s) (State s ())

--------------------------------------------------------------------------------
-- generalize KEndo essentially make it a state monad.
-------------------------------------------------------------------------------
--    s -> m ((), s) ~= s -> m s
-- so we can generalize KEndo by substiting s with (a, s)
-- simple generalization of Kleisli endomorphism is just state monad t.
newtype KEndoT s m a = KEndoT { appKEndoT :: s -> m (s, a) }

instance Injective (KEndoT s m a) (StateT s m a) where to (KEndoT kendot) = StateT $ kendot
instance Injective (StateT s m a) (KEndoT s m a) where to (StateT st) = KEndoT $ st
instance Iso (KEndoT s m a) (StateT s m a)

instance Monad m => Functor (KEndoT s m) where
  fmap f k = (from :: StateT s m a -> KEndoT s m a) $ fmap f (to k)
instance Monad m => Applicative (KEndoT s m) where pure = return; (<*>) = ap
instance Monad m => Monad (KEndoT s m) where
  return a = from $ (return :: a -> StateT s m a) a
  m >>= f = (from :: StateT s m a -> KEndoT s m a)  $ to m >>= \x -> to (f x)

-- and of course, KEndoT is still a monoid.
instance (Monoid a, Monad m) => Semigroup (KEndoT s m a) where
  (KEndoT k1) <> (KEndoT k2) = KEndoT $ \s -> do
    (s1, a) <- k2 s; (s2, a1) <- k1 s1; return (s2, a <> a1)
instance (Monoid a, Monad m) => Monoid (KEndoT s m a) where
  mempty = KEndoT $ \s -> return (s, mempty)

--------------------------------------------------------------------------------
-- A reducer with Endo
-------------------------------------------------------------------------------

data VM6502 = Count { regX :: Int, regY :: Int, acc :: Int, stack :: [Int] }
data Action =

 -- reducer :: Action

