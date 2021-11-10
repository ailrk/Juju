{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeFamilies             #-}
module Control.Monad.Loop where


import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.ST.Lazy
import           Control.Monad.State
import           Data.Foldable
import           Data.Kind            (Type)
import           Data.Maybe
import           Data.STRef.Lazy
import           Debug.Trace


type Loop :: (Type -> Type) -> Type -> Type -> Type

data Loop m r a where
  Across :: Traversable t => Loop m r () -> t a -> Loop m r (t a)
  While  :: Traversable t
         => Loop m r (t a) -> (a -> Bool) -> Loop m r (t a, a -> Bool)
  Run    :: Loop m r ()

across :: Traversable t => Loop m r () -> t a -> Loop m r (t a)
across = Across

while  :: Traversable t
       => Loop m r (t a) -> (a -> Bool) -> Loop m r (t a, a -> Bool)
while = While

loop = Run

evalLoop :: Monad m => Loop m r a -> m a
evalLoop Run = return ()
evalLoop (Across loop xs) = do
  _ <- evalLoop loop
  return xs

evalLoop (While loop pred) = do
  xs <- evalLoop loop
  return (xs, pred)

with_ :: (Traversable t, Monad m)
      => Loop m r (t a) -> (a -> ExceptT () m ()) -> m ()
with_ loop k = do
  xs <- evalLoop loop
  runExceptT $ traverse_ k xs
  return ()

enumerateTrav :: (Traversable t, Integral n) => t a -> t (n, a)
enumerateTrav ts = runST $ do
  idxref <- newSTRef 0
  flip traverse ts $ \value -> do
    idx <- readSTRef idxref
    idxref `modifySTRef` (+ 1)
    return (idx, value)

withi_ :: (Traversable t, Monad m, Integral n)
       => Loop m r (t a) -> ((n, a) -> ExceptT () m ()) -> m ()
withi_ loop k = do
  xs <- evalLoop loop
  runExceptT $ traverse_ (\n -> trace "hi" $ k n) (enumerateTrav xs)
  return ()

withWhile_ :: (Traversable t, Monad m)
           => Loop m r (t a, a -> Bool) -> (a -> ExceptT () m ()) -> m ()
withWhile_ loop k = do
  (ts, pred) <- evalLoop loop
  runExceptT $ traverse_ (\a -> if pred a then k a else throwError ()) ts
  return ()


-- | wrap monadic action in ExceptT that we can jump out.
loopWrap :: Monad m => ExceptT e m a -> m e
loopWrap = fmap (either id id) . runExceptT . forever

-- | break to the outer loop.
quit :: Monad m => ExceptT () m a
quit = throwError ()
