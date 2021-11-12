{-# LANGUAGE DeriveGeneric #-}
module Transformers.Identity where


import Data.Functor.Identity
import GHC.Generics
import Data.Functor.Classes
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Transformers.Class
import Data.Functor.Contravariant
import Signatures


newtype IdentityT f a = IdentityT { runIdentityT :: f a }
  deriving (Generic, Generic1)


instance (Eq1 f) => Eq1 (IdentityT f) where
  liftEq eq (IdentityT x) (IdentityT y) = liftEq eq x y

instance (Ord1 f) => Ord1 (IdentityT f) where
  liftCompare comp (IdentityT x) (IdentityT y) = liftCompare comp x y

instance (Read1 f) => Read1 (IdentityT f) where
  liftReadsPrec rp rl = readsData $
    readsUnaryWith (liftReadsPrec rp rl) "IdentityT" IdentityT

instance (Show1 f) => Show1 (IdentityT f) where
  liftShowsPrec sp sl d (IdentityT m) =
    showsUnaryWith (liftShowsPrec sp sl) "IdentityT" d m

instance (Eq1 f, Eq a) => Eq (IdentityT f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (IdentityT f a) where compare = compare1
instance (Read1 f, Read a) => Read (IdentityT f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (IdentityT f a) where showsPrec = showsPrec1

instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT m) = IdentityT (fmap f m)

instance Foldable f => Foldable (IdentityT f) where
  foldMap f (IdentityT t) = foldMap f t
  foldr f z (IdentityT t) = foldr f z t
  foldl f z (IdentityT t) = foldl f z t
  foldr1 f (IdentityT t) = foldr1 f t
  foldl1 f (IdentityT t) = foldl1 f t

instance Traversable f => Traversable (IdentityT f) where
  traverse f (IdentityT t) = IdentityT <$> traverse f t

instance Applicative f => Applicative (IdentityT f) where
  pure = IdentityT . pure
  (IdentityT fn) <*> (IdentityT ma) = IdentityT $ fn <*> ma

instance Alternative m => Alternative (IdentityT m) where
  empty = IdentityT empty
  (IdentityT x) <|> (IdentityT y) = IdentityT (x <|> y)

instance Monad m => Monad (IdentityT m) where
  return = pure
  IdentityT x >>= f = IdentityT $ x >>= runIdentityT . f

instance MonadFail m => MonadFail (IdentityT m) where
  fail = IdentityT . fail

instance MonadPlus m => MonadPlus (IdentityT m) where
  mzero = IdentityT mzero
  (IdentityT x) `mplus` (IdentityT y) = IdentityT (x `mplus` y)

instance MonadFix m => MonadFix (IdentityT m) where
  mfix f = IdentityT (mfix $ runIdentityT . f)

instance MonadTrans IdentityT where
  lift = IdentityT

instance MonadIO m => MonadIO (IdentityT m) where
  liftIO  = IdentityT . liftIO

instance Contravariant f => Contravariant (IdentityT f) where
  contramap f = IdentityT . contramap f . runIdentityT

mapIdentityT :: (m a -> n b) -> IdentityT m a -> IdentityT n b
mapIdentityT f = IdentityT . f . runIdentityT

lift2IdentityT :: (m a -> n b -> p c) -> IdentityT m a -> IdentityT n b -> IdentityT p c
lift2IdentityT f a b = IdentityT (f (runIdentityT a) (runIdentityT b))

liftCallCC :: CallCC m a b -> CallCC (IdentityT m) a b
liftCallCC callCC f =  IdentityT . callCC $ \c -> runIdentityT (f (IdentityT . c))

liftCatch :: Catch e m a -> Catch e (IdentityT m) a
liftCatch f m h = IdentityT $ f (runIdentityT m) (runIdentityT . h)
