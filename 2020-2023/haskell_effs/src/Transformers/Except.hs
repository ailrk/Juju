{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
module Transformers.Except where


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


import Data.Kind
type ExceptT :: Type -> (Type -> Type) -> Type -> Type
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a)}
  deriving (Generic, Generic1)

type Except e a = ExceptT e Identity a

except :: (Monad m) => Either e a -> ExceptT e m a
except = ExceptT . return

runExcept :: Except e a -> Either e a
runExcept = runIdentity . runExceptT

mapExcept :: (Either e a -> Either e' b) -> Except e a -> Except e' b
mapExcept = undefined

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept = undefined

mapExceptT :: Monad m
           => (m (Either e a) -> n (Either e' b))
           -> ExceptT e m a -> ExceptT e' n b
mapExceptT = undefined

withExceptT :: Monad m
            => (e -> e')
            -> ExceptT e m a
            -> ExceptT e' m a
withExceptT = undefined

-- this stupid blob...

instance (Eq e, Eq1 m) => Eq1 (ExceptT e m) where
    liftEq eq (ExceptT x) (ExceptT y) = liftEq (liftEq eq) x y
    {-# INLINE liftEq #-}

instance (Ord e, Ord1 m) => Ord1 (ExceptT e m) where
    liftCompare p (ExceptT x) (ExceptT y) = liftCompare (liftCompare p) x y
    {-# INLINE liftCompare #-}

instance (Read e, Read1 m) => Read1 (ExceptT e m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "ExceptT" ExceptT
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show e, Show1 m) => Show1 (ExceptT e m) where
    liftShowsPrec sp sl d (ExceptT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "ExceptT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Eq e, Eq1 m, Eq a) => Eq (ExceptT e m a) where (==) = eq1
instance (Ord e, Ord1 m, Ord a) => Ord (ExceptT e m a) where compare = compare1
instance (Read e, Read1 m, Read a) => Read (ExceptT e m a) where
    readsPrec = readsPrec1
instance (Show e, Show1 m, Show a) => Show (ExceptT e m a) where
    showsPrec = showsPrec1


instance Functor m => Functor (ExceptT e m) where
  fmap = undefined

instance Foldable f => Foldable (ExceptT e f) where
  foldMap = undefined

instance Traversable f => Traversable (ExceptT e f) where
  traverse = undefined

instance Monad m => Applicative (ExceptT e m) where
  pure = ExceptT . return . Right
  f <*> m = undefined

instance Monad m => Monad (ExceptT e m) where
  return = pure
  m >>= k = undefined

instance (Monad m, Monoid e) => Alternative (ExceptT e m) where
  empty = ExceptT . return . Left $ mempty
  x <|> y = undefined

instance MonadFix m => MonadFix (ExceptT e m) where
  mfix f = undefined

instance MonadFail m => MonadFail (ExceptT e m) where
  fail msg = undefined

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO

instance MonadTrans (ExceptT e) where
  lift = ExceptT . fmap Right

throwE :: Monad m => e -> ExceptT e m a
throwE = undefined

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
m `catchE` h = undefined

handleE :: Monad m => (e -> ExceptT e' m a) -> ExceptT e m a -> ExceptT e' m a
handleE = flip catchE

tryE :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
tryE = undefined

finallyE :: Monad m => ExceptT e m a -> ExceptT e m () -> ExceptT e m a
finallyE = undefined

liftCallCC :: CallCC m (Either e a) (Either e b) -> CallCC (ExceptT e m) a b
liftCallCC = undefined
