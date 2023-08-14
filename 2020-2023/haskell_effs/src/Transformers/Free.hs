{-# LANGUAGE TypeOperators #-}
module Transformers.Free where

-- Free monad transformer

import           Control.Applicative
import           Control.Monad
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Functor.Identity
import           Data.Function         (on)
import           Data.Monoid
import           Data.Traversable
import           Data.Typeable
import           Transformers.Class


-- build dsl with free monad.

data FreeF f a b
  = Pure a
  | Free (f b)
  deriving (Eq, Show, Ord, Read, Typeable)

-- always do nothing in pure case.

instance Functor f => Functor (FreeF f a) where
  fmap _ (Pure a)  = Pure a
  fmap f (Free as) = Free (fmap f as)

instance Foldable f => Foldable (FreeF f a) where
  foldMap f (Free as) = foldMap f as
  foldMap _ _         = mempty

instance Traversable f => Traversable (FreeF f a) where
  traverse _ (Pure a)  = pure (Pure a)
  traverse f (Free as) = Free <$> traverse f as

newtype FreeT f m a = FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }
type Free f = FreeT f Identity
