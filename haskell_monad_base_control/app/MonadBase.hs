{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MonadBase where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

-- same mechanism for MonadIO but works for different base monad.

-- monad morphism from base monad b to monad m.
class (Monad b, Monad m) => MonadBase b m | m -> b where
  liftBase :: b a -> m a

-- base monad
instance MonadBase IO IO where liftBase = id
instance MonadBase [] [] where liftBase = id
instance MonadBase Maybe Maybe where liftBase = id
instance MonadBase ((->) a) ((->) a) where liftBase = id

-- trans monad
instance MonadBase b m => MonadBase b (StateT s m) where liftBase = lift . liftBase
instance MonadBase b m => MonadBase b (ReaderT r m) where liftBase = lift . liftBase
instance (Monoid w, MonadBase b m) => MonadBase b (WriterT w m) where liftBase = lift . liftBase
