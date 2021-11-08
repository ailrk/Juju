{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Loop where


import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Kind          (Type)
import Data.Foldable

-- cases
-- from i to j        -> build [i, j..1]
-- from i to j step k -> build [i,j..k]
-- across xs          -> build [a0, a1, ..]
-- while              -> give [i, j..k], put in except monad, check
--                       before calling action, if unsatisfied then break.

-- | List wrapper for from .. to .. step loop. This type avoid conflit with
-- | general traversables.
newtype IndexList n = IndexList [n]

type Loop :: (Type -> Type) -> Type -> Type -> Type
data Loop m r a where
  Across :: Traversable f => Loop m r a -> f a -> Loop m r (f a)
  While  :: Traversable f
         => Loop m r (f a) -> (a -> Bool) -> Loop m r (f a, a -> Bool)
  Run    :: Loop m r a

across :: Traversable f => Loop m r a -> f a -> Loop m r (f a)
across = Across

while  :: Traversable f
       => Loop m r (f a) -> (a -> Bool) -> Loop m r (f a, a -> Bool)
while = While

loop = Run

evalLoop :: Monad m => Loop m r a -> m a
evalLoop Run = undefined
evalLoop (Across loop xs) = do
  _ <- evalLoop loop
  return xs

evalLoop (While loop pred) = do
  xs <- evalLoop loop
  return (xs, pred)


class With loop k where
  type WithRet loop k
  with :: loop -> k -> WithRet loop k

instance (Traversable f, Monad m) =>
         With (Loop m r (f a)) (a -> m r)
  where
  type WithRet (Loop m r (f a)) (a -> m r) = m (f r)
  with loop k = do
    xs <- evalLoop loop
    traverse k xs

instance (Traversable f, Monad m) =>
         With (Loop m r (f a)) (a -> m ())
  where
  type WithRet (Loop m r (f a)) (a -> m ()) = m ()
-- with_ :: (Traversable f, Monad m) => Loop m r (f a) -> (a -> m r) -> m ()
  with loop k = do
    xs <- evalLoop loop
    traverse_ k xs





-- foo :: IO ()
-- foo = do
--   loop `across` [1, 2, 3] `with_` \i -> return ()
