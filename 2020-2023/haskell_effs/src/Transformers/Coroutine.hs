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
import           Control.Monad.Parallel     (MonadParallel (..))
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

instance (Functor s, MonadParallel m) => MonadParallel (Coroutine s m) where
  bindM2 = liftBinder bindM2

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
foldRun :: Monad m
        => (a -> s (Coroutine s m x) -> (a, Coroutine s m x))
        -> a
        -> Coroutine s m x
        -> m (a, x)
foldRun f a gen = do
  s <- resume gen
  case s of
    Left  s' -> let (a', cont) = f a s' in foldRun f a' cont
    Right x  -> return (a, x)

-- bind coroutines together
type PairBinder m =  forall x y r . (x -> y -> m r) -> m x -> m y -> m r

liftBinder :: forall s m . (Functor s, Monad m)
           => PairBinder m -> PairBinder (Coroutine s m)
liftBinder binder f mx my = Coroutine $ binder combine (resume mx) (resume my)
  where
    combine (Right x) (Right y) = resume (f x y)
    combine (Right x) (Left y)  = return . Left . (fmap (f x =<<)) $ y
    combine (Left x)  (Right y) = undefined
    combine (Left x)  (Left y)  = undefined

sequentialBinder :: Monad m => PairBinder m
sequentialBinder f mx my = do
  x <- mx
  y <- my
  f x y

parallelBinder :: (Monad m, MonadParallel m) => PairBinder m
parallelBinder = bindM2

-- weave two coroutines iunto a single coroutine.
type Weaver s1 s2 s3 m x y z = Coroutine s1 m x
                            -> Coroutine s2 m y
                            -> Coroutine s3 m z


type WeaverStepper s1 s2 s3 m x y z = Weaver s1 s2 s3 m x y z
                                   -> Either (s1 (Coroutine s1 m x)) x
                                   -> Either (s2 (Coroutine s2 m y)) y
                                   -> Coroutine s3 m z

weave :: forall s1 s2 s3 m x y z
       . (Monad m, Functor s1, Functor s2, Functor s3)
      => PairBinder m
      -> WeaverStepper s1 s2 s3 m x y z
      -> Weaver s1 s2 s3 m x y z
weave binder weaveStep gen1 gen2 = zipCoroutine gen1 gen2
  where zipCoroutine g1 g2 = Coroutine $ binder f (resume g1) (resume g2)
          where f a b = resume $ weaveStep zipCoroutine a b


-- Suspension functors
-- The behavior of coroutine depends on what suspension functor is used.
data Yield x y = Yield x y
instance Functor (Yield x) where
  fmap f (Yield x y) = Yield x (f y)

newtype Await x y = Await (x -> y)
instance Functor (Await x) where
  fmap f (Await g) = Await (f . g)

-- combine yield and await
data Request request response x = Request request (response -> x)
instance Functor (Request x f) where
  fmap f (Request x g) = Request x (f . g)

yield :: Monad m => x -> Coroutine (Yield x) m ()
yield x = suspend (Yield x (return ()))

await :: Monad m => Coroutine (Await x) m x
await = suspend (Await return)

request :: Monad m => x -> Coroutine (Request x y) m y
request x = suspend (Request x return)
