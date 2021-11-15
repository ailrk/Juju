{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}

{-# LANGUAGE RankNTypes               #-}
module Transformers.Coroutine where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Either
import           Data.Functor.Classes
import           Data.Functor.Contravariant
import           Data.Functor.Identity
import           Data.Kind
import           GHC.Generics
import           Signatures
import           Transformers.Class


type Coroutine :: (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype Coroutine s m r =
  Coroutine { resume :: m (Either (s (Coroutine s m r)) r) }
  deriving (Generic)

instance (Functor m, Functor s) => Functor (Coroutine s m) where
  fmap f = Coroutine . (fmap (apply f)) . resume
    where
      apply f (Right x) = Right $ f x
      apply f (Left s)  = Left  $ (fmap . fmap) f s

instance (Monad m, Functor s) => Applicative (Coroutine s m) where
  pure  = Coroutine . pure . Right
  (<*>) = ap

instance (Monad m, Functor s) => Monad (Coroutine s m) where
  return = pure
  m >>= k = Coroutine $ (apply k =<< resume m)
    where
      apply k (Right x) = resume $ k x
      apply k (Left s)  = return . Left . fmap (>>= k) $ s

instance MonadTrans (Coroutine s) where
  lift = Coroutine . fmap Right

instance (Functor s, MonadFail m) => MonadFail (Coroutine s m) where
  fail = Coroutine . fmap Right . fail

instance (Functor s, MonadIO m) =>  MonadIO (Coroutine s m) where
  liftIO = lift . liftIO


-- functor for non suspendable coroutine.
data Naught x
instance Functor Naught where
  fmap _ _ = undefined

-- suspend the current coroutine
suspend :: (Monad m, Functor s) => s (Coroutine s m x) -> Coroutine s m x
suspend = Coroutine . return . Left

-- change the base monad of Coroutine
mapMonad :: forall s m m' x . (Functor s, Monad m, Monad m')
         => (forall y . m y -> m' y)
         -> Coroutine s m x
         -> Coroutine s m' x
mapMonad f = Coroutine . fmap apply  . f . resume
  where
    apply (Right r) = Right r
    apply (Left s)  = Left $ fmap (mapMonad f) s

-- change the suspension functor  of a Coroutine.
mapSuspension :: (Functor s, Monad m)
              => (forall y . s y -> s' y)
              -> Coroutine s m x
              -> Coroutine s' m x
mapSuspension f = Coroutine . fmap apply . resume
  where
    apply (Right x) = Right x
    apply (Left s)  = Left $ f (fmap (mapSuspension f) s)

-- modify the fisrt suspension
mapFirstSuspension :: forall s m x . (Functor s, Monad m)
                   => (forall y . s y -> s y)
                   -> Coroutine s m x
                   -> Coroutine s m x
mapFirstSuspension f = Coroutine . fmap apply . resume
  where
    apply (Right x) = Right x
    apply (Left s)  = Left $ f s

-- convert a non suspendable coroutine to the base monad.
runCoroutine :: Monad m => Coroutine Naught m x -> m x
runCoroutine =
  pogoStick (error "runCouroutine works on non suspending coroutines")

-- run once step of a suspendable coroutine.
bounce :: (Functor s, Monad m)
       => (s (Coroutine s m x) -> Coroutine s m x)
       -> Coroutine s m x
       -> Coroutine s m x
bounce spring gen = lift (resume gen) >>= either spring return

-- run suspendable coroutine to completion
pogoStick :: Monad m
          => (s (Coroutine s m x) -> Coroutine s m x)
          -> Coroutine s m x
          -> m x
pogoStick spring = loop
  where loop gen = resume gen >>= either (loop . spring) return

-- pogo stick with monadic action
pogoStickM :: Monad m => (s (Coroutine s m x) -> m (Coroutine s m x))
           -> Coroutine s m x
           -> m x
pogoStickM spring = loop
  where loop gen = resume gen >>= either (loop <=< spring) return

-- Suspension functors
-- The behavior of coroutine depends on what suspension functor is used.

data Yield x y = Yield x y
instance Functor (Yield x) where
  fmap f (Yield x y) = Yield x (f y)

newtype Await x y = Await (x -> y)
instance Functor (Await x) where
  fmap f (Await g) = Await (f . g)
