{-# LANGUAGE DeriveGeneric #-}
module Transformers.Writer where

import           Control.Applicative
import           Control.Monad              (MonadPlus (..), ap)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Functor.Classes
import           Data.Functor.Contravariant
import           Data.Functor.Identity
import           Data.Kind
import           Data.Maybe
import           GHC.Generics
import           Signatures
import           Transformers.Class

-- note wirter monad works works with monoid.

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) } deriving (Generic)

type Writer w a = WriterT w Identity a

writer :: (a, w) -> Writer w a
writer = WriterT . return

runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

execWriter :: Writer w a -> w
execWriter = snd . runWriter

mapWriter :: ((a -> w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter = undefined

instance (Eq w, Eq1 m) => Eq1 (WriterT w m) where
    liftEq eq (WriterT m1) (WriterT m2) = liftEq (liftEq2 eq (==)) m1 m2
    {-# INLINE liftEq #-}

instance (Ord w, Ord1 m) => Ord1 (WriterT w m) where
    liftCompare p m1 m2 =
      liftCompare (liftCompare2 p compare) (runWriterT m1) (runWriterT m2)
    {-# INLINE liftCompare #-}

instance (Read w, Read1 m) => Read1 (WriterT w m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "WriterT" WriterT
      where
        rp' = liftReadsPrec2 rp rl readsPrec readList
        rl' = liftReadList2 rp rl readsPrec readList

instance (Show w, Show1 m) => Show1 (WriterT w m) where
    liftShowsPrec sp sl d (WriterT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "WriterT" d m
      where
        sp' = liftShowsPrec2 sp sl showsPrec showList
        sl' = liftShowList2 sp sl showsPrec showList

instance (Eq w, Eq1 m, Eq a) => Eq (WriterT w m a) where (==) = eq1
instance (Ord w, Ord1 m, Ord a) => Ord (WriterT w m a) where compare = compare1
instance (Read w, Read1 m, Read a) => Read (WriterT w m a) where
    readsPrec = readsPrec1
instance (Show w, Show1 m, Show a) => Show (WriterT w m a) where
    showsPrec = showsPrec1

execWriterT :: Monad m => WriterT w m a -> w
execWriterT = undefined

mapWriterT :: Monad m
           => (m (w, a) -> n (w', b))
           -> WriterT w m a
           -> WriterT w' n b
mapWriterT = undefined

instance Functor m => Functor (WriterT w m) where
  fmap f = undefined

instance Foldable f => Foldable (WriterT w f) where
  foldMap f = undefined

instance Traversable f => Traversable (WriterT w f) where
  traverse f = undefined

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure a = WriterT . pure $ (a, mempty)
  a <*> b = undefined


