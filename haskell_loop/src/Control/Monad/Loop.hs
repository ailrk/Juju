{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Control.Monad.Loop where


import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Kind          (Type)


type Loop :: (Type -> Type) -> Type -> Type
data Loop m a where
  From   :: Loop m a -> n -> Loop m n
  To     :: Loop m n -> n -> Loop m (n, n)
  Step   :: Loop m (n, n) -> n -> Loop m (n, n, n)
  While  :: Loop m a -> (a -> Bool) -> Loop m a

  Across :: Loop m a -> m a -> Loop m a

  Run    :: Loop m a

  -- entry
  With   :: Loop m a -> (a -> m b) -> Loop m a

from :: Integral n => Loop m a -> n -> Loop m n
from = From
to :: Integral n => Loop m n -> n -> Loop m (n, n)
to = To
step :: Integral n => Loop m (n, n) -> n -> Loop m (n, n, n)
step = Step
across = Across
while = While
loop = Run
with = With

-- instance Monad m => Functor (Loop m) where
--   fmap f loop = fmap f (evalLoop loop)

evalLoop :: Monad m => Loop m a -> m a
evalLoop Run = undefined
evalLoop (From loop i) = do
  _ <- evalLoop loop
  return $ i
evalLoop (To loop j) = do
  i <- evalLoop loop
  return (i, j)
evalLoop (Step loop s) = do
  (i, j) <- evalLoop loop
  return (i, j, s)
evalLoop (Across loop foldable) = do
  _ <- evalLoop loop
  foldable
evalLoop (While loop pred) = undefined
evalLoop (With loop k) = undefined -- evalLoop loop >>= k


-- how do you unrow this thing?
lang = With (To (From Run 10) 20) undefined

-- evalLoop header should give (return [10 .. 20]), we can evalLoop
-- header >>= k

header = (To (From Run 10) 20)

-- should give us (return . \x -> [10 .. n])
fromclause = (From Run 10)

start = Run

xs = loop `from` 10 `to` 20 `with` \i -> do undefined
