{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
module MuationTypeFam where

import           Control.Monad.ST
import           Control.Monad.Trans
import           Control.Monad.Trans.RWS
import           Control.Monad.Trans.Cont
import           Data.IORef
import           Data.STRef


------------------------------------------------------ ------------------------
-- Use type family to replace functional dependency

-- associate open type family
class Mutation m where
  type Ref m :: * -> *
  newRef :: a -> m (Ref m a)
  readRef :: (Ref m a) -> m a
  writeRef :: (Ref m a) -> a -> m ()

instance Mutation IO where
  type Ref IO = IORef
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

instance Mutation (ST s) where
  type Ref (ST s) = STRef s
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

instance (Monad m, Mutation m, MonadTrans t) => Mutation (t m) where
  type Ref (t m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef = (lift .) . writeRef  -- a lil trick to apply one more argument

run :: (Mutation m, Monad m) => m ()
run = do
  ref <- newRef 0
  step ref
  where
    step ref = do
      v <- readRef ref
      ref `writeRef` (v + 1)

-- same interface works on all monads.

run1 :: IO ()
run1 = run

run2 :: ST s ()
run2 = run

run3 :: RWST () () () IO ()
run3 = run

run4 :: ContT r IO ()
run4 = run
