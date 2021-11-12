{-# LANGUAGE StandaloneKindSignatures #-}
module Transformers.State where

import           Control.Applicative
import           Control.Monad              (MonadPlus (..), ap)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Functor.Classes
import           Data.Functor.Contravariant
import           Data.Functor.Identity
import           Data.Kind
import           Data.Maybe
import           Signatures
import           Transformers.Class

-- lazy state


type StateT :: Type -> (Type -> Type) -> Type -> Type
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

type State m a = StateT m Identity a

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT $ return . f

runState :: State s a -> s -> (a, s)
runState m = runIdentity . runStateT m

evalState :: State s a -> s -> a
evalState m = fst . runState m

execState :: State s a -> s -> s
execState m = snd . runState m

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f = mapStateT (Identity . f . runIdentity)

withState :: (s -> s) -> State s a -> State s a
withState = withStateT

-- T

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = fst <$> runStateT m s

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = snd <$> runStateT m s

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m =  StateT $ f . runStateT m

withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m =  StateT $ runStateT m . f


instance Functor m => Functor (StateT s m) where
  fmap f (StateT m) =  StateT $ \s -> fmap (\ ~(a, s) -> (f a, s)) (m s)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  (StateT fn) <*> (StateT ma) = StateT $ \s -> do
    (f, s') <- fn s
    (a, s'') <- ma s'
    return (f a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  m >>= k = StateT $ \s -> do
    (a, s') <- runStateT m s
    runStateT (k a) s'

instance MonadFail m => MonadFail (StateT s m) where
  fail = StateT . const . fail

instance (Alternative m, Monad m) => Alternative (StateT s m) where
  empty = StateT $ const empty
  ma <|> mb = StateT $ \s -> runStateT ma s <|> runStateT mb s

instance (MonadPlus m, Monad m)=> MonadPlus (StateT s m) where
  mzero = StateT $ const mzero
  ma `mplus` mb = StateT $ \s -> runStateT ma s `mplus` runStateT mb s

instance MonadFix m => MonadFix (StateT s m) where
  mfix f = StateT $ \s -> do mfix $ \ ~(a, _) -> runStateT (f a) s

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> m >>= \a -> return (a, s)

instance Contravariant m => Contravariant (StateT s m) where
  contramap f m = StateT $ \s ->
    contramap (\ ~(a, s') -> (f a, s')) $ runStateT m s

get :: Monad m => StateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> StateT s m ()
put s = state $ \s -> ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s)

modify' :: Monad m => (s -> s) -> StateT s m ()
modify' f = state $ \s -> ((), f $! s)

gets :: Monad m => (s -> a) -> StateT s m a
gets getter = state $ \s -> (getter s, s)

-- use original state
liftCallCC :: CallCC m (a, s) (b, s) -> CallCC (StateT s m) a b
liftCallCC callCC f = StateT $ \s ->
  callCC $ \c -> runStateT (f (\a -> StateT $ const (c (a, s)))) s

-- use the next state
liftCallCC' :: CallCC m (a, s) (b, s) -> CallCC (StateT s m) a b
liftCallCC' callCC f = StateT $ \s ->
  callCC $ \c -> runStateT (f (\a -> StateT $ \s' -> (c (a, s')))) s

-- In error case  fall back to the previous state.
liftCatch :: Catch e m (a, s) -> Catch e (StateT s m) a
liftCatch catchE m h = StateT $
  \s -> runStateT m s `catchE` (\e -> runStateT (h e) s)

liftListen :: Monad m => Listen w m (a, s) -> Listen w (StateT s m) a
liftListen listen m = StateT $ \s -> do
  ~((a, s), w) <- listen (runStateT m s)
  return _

