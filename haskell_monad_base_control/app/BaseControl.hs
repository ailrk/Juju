{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Control where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import MonadBase

-- for monad being a functions they can have a input state and a output state.

-- representation:  r -> m (a, w)
-- input type is r, output type is w
type T1 r w m = ReaderT r (WriterT w m)


-- representation: s -> r -> m (a, w)
-- input type is (s, r), output type is w.
type T2 s r w m = StateT s (ReaderT r (WriterT w m))


-- representation: s -> r -> m (a, s)
-- input type is (s, r), output type is s
type T3 s r m = StateT s (ReaderT r m)



-- etc.

-- steps to unlift arbitrary monad transformers:
-- 1. capture the action's output state and close over it
-- 2. pcakages up the output state with it's reuslt and run it
-- 3. restore the action's output state into the enclosing transformer
-- 4. return the actions' result.


class MonadBase b m => MonadBaseControl b m | m -> b where
  type InputState m
  type OutputState m

  captureInputState :: m (InputState m)
  closeOverInputState :: m a -> InputState m -> b (a, OutputState m)
  restoreOutputState :: OutputState m -> m ()


-- simple case for b to m
instance MonadBaseControl IO IO where
  type InputState IO = ()
  type OutputState IO = ()

  captureInputState = return ()
  closeOverInputState m () = m >>= \a -> return (a, ())
  restoreOutputState () = return ()


instance MonadBaseControl b m => MonadBaseControl b (StateT s m) where
  type InputState (StateT s m) = (s, InputState m)
  type OutputState (StateT s m) = (s, OutputState m)

  captureInputState = (,) <$> get <*>  lift captureInputState
  closeOverInputState m (s, is) = do
    ((v, s'), is') <- closeOverInputState (runStateT m s) is
    return (v, (s', is'))
  restoreOutputState (s, ss) = lift (restoreOutputState ss) *> put s

instance MonadBaseControl b m => MonadBaseControl b (ReaderT r m) where
  type InputState (ReaderT r m) = (r, InputState m)
  type OutputState (ReaderT r m) = OutputState m

  captureInputState = (,) <$> ask <*>  lift captureInputState
  closeOverInputState m (s, is) = closeOverInputState (runReaderT m s) is
  restoreOutputState ss = lift (restoreOutputState ss)


instance (Monoid w, MonadBaseControl b m) => MonadBaseControl b (WriterT w m) where
  type InputState (WriterT w m) = InputState m
  type OutputState (WriterT w m) = (w, OutputState m)

  captureInputState = lift captureInputState
  closeOverInputState m is = do
    ((v, s'), is') <- closeOverInputState (runWriterT m) is
    return (v, (s', is'))
  restoreOutputState (s, is) = lift (restoreOutputState is) *> tell s



foo :: IO a -> IO a
foo action = do
  print "action"
  action


foo' :: MonadBaseControl IO m => m a -> m a
foo' m = do
  s <- captureInputState
  let m' = closeOverInputState m s
  (v, s') <- liftBase $ foo m'
  restoreOutputState s'
  return v


bar' :: StateT Int IO Char
bar' = StateT $ (\s -> return $ ('a', s))


-- foo' is still annoying to use, but it's type is generic now.
-- anything implements MonadBaseControl works with it.
run1 :: IO ()
run1 = void $ flip runStateT 0 (foo' bar')



-------------------------------------------------------------------------------

