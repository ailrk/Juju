{-# LANGUAGE StandaloneKindSignatures #-}
module Mtl.Maybe where

import           Control.Monad        (ap, MonadPlus(..))
import           Data.Functor.Classes
import           Data.Kind
import           Transformers.Class
import           Transformers.Except
import Control.Applicative
import Control.Monad.Fix
import Data.Maybe

type MaybeT :: (Type -> Type) -> Type -> Type
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  {-# INLINE return #-}

  m >>= f = MaybeT $ do
    x <- runMaybeT m
    case x of
      Nothing -> return Nothing
      Just a  -> runMaybeT $ f a
  {-# INLINE (>>=) #-}

instance Monad m => Applicative (MaybeT m) where
  pure = MaybeT . return . Just
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad m => Functor (MaybeT m) where
  fmap f m = MaybeT $ (fmap . fmap) f (runMaybeT m)

instance MonadTrans MaybeT where
  lift action = MaybeT $ (fmap Just) action

-- liftting wrapped monad.

instance (Eq1 m) => Eq1 (MaybeT m) where
  liftEq eq (MaybeT x) (MaybeT y) = liftEq (liftEq eq) x y

instance (Ord1 m) => Ord1 (MaybeT m) where
  liftCompare comp (MaybeT x) (MaybeT y) = liftCompare (liftCompare comp) x y

instance (Read1 m) => Read1 (MaybeT m) where
  liftReadsPrec rp rl = readsData $
    readsUnaryWith (liftReadsPrec rp' rl') "MaybeT" MaybeT
    where
      rp' = liftReadsPrec rp rl
      rl' = liftReadList rp rl

instance (Show1 m) => Show1 (MaybeT m) where
  liftShowsPrec sp sl d (MaybeT m) =
    showsUnaryWith (liftShowsPrec sp' sl') "MaybeT" d m
    where
      sp' = liftShowsPrec sp sl
      sl' = liftShowList sp sl

instance (Eq1 m, Eq a) => Eq (MaybeT m a) where (==) = eq1
instance (Ord1 m, Ord a) => Ord (MaybeT m a) where compare = compare1
instance (Read1 m, Read a) => Read (MaybeT m a) where readsPrec = readsPrec1
instance (Show1 m, Show a) => Show (MaybeT m a) where showsPrec = showsPrec1

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT = undefined

hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = undefined

maybeToExceptT :: (Functor m) => e -> MaybeT m a -> ExceptT e m a
maybeToExceptT e (MaybeT m) = undefined

instance (Monad m) => MonadFail (MaybeT m) where
  fail _ = MaybeT (return Nothing)
  {-# INLINE fail #-}

instance (Monad m) => Alternative (MaybeT m) where
  empty = MaybeT (return Nothing)
  {-# INLINE empty #-}

  x <|> y = MaybeT $ do
    xv <- runMaybeT x
    case xv of
      Nothing -> return Nothing
      Just _ -> runMaybeT y
  {-# INLINE (<|>) #-}

instance (Monad m) => MonadPlus (MaybeT m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance (MonadFix m) => MonadFix (MaybeT m) where
  mfix f = MaybeT (mfix (runMaybeT . f . fromMaybe bom))
    where bom = error "mfix (MaybeT): Inner computation returned Nothing"
