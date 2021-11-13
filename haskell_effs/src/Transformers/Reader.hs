{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
module Transformers.Reader where


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
import GHC.Generics


type Reader r a = ReaderT r Identity a

reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT (return . f)

runReader :: ReaderT r Identity a -> r -> a
runReader m = runIdentity . runReaderT m

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f = mapReaderT (fmap f)

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
  deriving (Generic, Generic1)

instance Functor m => Functor (ReaderT r m) where
  fmap f = mapReaderT (fmap f)

instance Monad m => Applicative (ReaderT r m) where
  pure = liftReaderT . pure
  x <*> y = ReaderT $ \r -> runReaderT x r <*> runReaderT y r

instance (Alternative m, Monad m) => Alternative (ReaderT r m) where
  empty = liftReaderT empty
  x <|> y = ReaderT $ \r -> runReaderT x r <|> runReaderT y r

instance Monad m => Monad (ReaderT r m) where
  return = pure
  m >>= k = ReaderT $ \r -> runReaderT m r >>= \a -> runReaderT (k a) r

instance MonadFail m => MonadFail (ReaderT r m) where
  fail msg = lift (fail msg)

instance MonadTrans (ReaderT r) where
  lift = liftReaderT

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance MonadFix m => MonadFix (ReaderT r m) where
  mfix f = ReaderT $ \r -> mfix $ \a -> runReaderT (f a) r

instance MonadPlus m => MonadPlus (ReaderT r m) where
  mzero = ReaderT $ const mzero
  m `mplus` n = ReaderT $ \r -> runReaderT m r `mplus` runReaderT n r

ask :: Monad m => ReaderT r m r
ask = ReaderT return

-- run an readerT action in a modified environment.
local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local = withReaderT

-- access a field of the environment
asks :: Monad m => (r -> a) -> ReaderT r m a
asks f = ReaderT $ return . f

liftReaderT :: m a -> ReaderT r m a
liftReaderT = ReaderT . const

liftCallCC :: CallCC m a b -> CallCC (ReaderT r m) a b
liftCallCC callcc f = ReaderT $ \r ->
  callcc $ \c -> runReaderT (f (ReaderT . const . c)) r

