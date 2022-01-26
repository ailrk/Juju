{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module Free1 where

import Control.Monad


-------------- 1.
-- a free is either a singleton Pure or a fix point
data Free f a
  = Pure a
  | Free (f (Free f a))


-- fmap to a free means map f all the way down
instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free fa) = Free (fmap f <$> fa)


instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap


instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Free fx >>= f = Free ((>>= f) <$> fx)


-- natural transformation phi
infixr 0 ~>
type (f :: * -> *) ~> (g :: * -> *) = forall x . f x -> g x


-- with phi we can chang the context functor from f to g.
-- read: given a natural transformation f ~> g we can have a
-- natural transformation from Free f ~ Free g
--
--
-- an example of morphism between natural transformation.
freeM :: (Functor f, Functor g) => f ~> g -> Free f ~> Free g
freeM _ (Pure x) = Pure x
freeM phi (Free fx) = Free $ phi (freeM phi <$> fx)


-------------- 2.

-- if m is already a monad, natural transformation from a monad to antother monad
-- is a monad morphism
-- this is a monad morphism from a Free Monad to it's embeded monad
--
-- general form of monad morphism
-- phi' :: (Monad m, Monad n) => m ~> n
-- phi' = undefined


monad :: Monad m => Free m ~> m
monad (Pure x) = pure x
monad (Free mfx) = do
  fx <- mfx
  monad fx



-------------- 3.
-- combine monad and freeM we get a morphism from a natural transformation from
-- a functor to a monad get a monad morphism by free.
-- 1 f is just a functor
-- 2 Free maps a haskell functor to a haskell monad
-- 3 Free f ~> m maps a functor to a monad and then maps to another monad m

interp :: (Functor f, Monad m) => f ~> m -> Free f ~> m
interp phi = monad . freeM phi



-------------- 4.
-- redefine monad in ternms of Free

class Functor m => Monad' m where
  monad' :: Free m ~> m

pure' :: Monad' m => a -> m a
pure' = monad' . Pure

join' :: Monad' m => m (m a) -> m a
join' = monad' . Free . fmap (Free . fmap Pure)




-------------- 5.
-- free monad in practice
-- define dsl as functors for each functionality of the system


-- lift any functor value into Free monad
liftF :: Functor f => f a -> Free f a
liftF command = Free (fmap Pure command)

----
data KeyValF a
  = GetKey String (Maybe String -> a)
  | PutKey String String a
  deriving Functor

type KeyVal = Free KeyValF

getKey :: String -> KeyVal (Maybe String)
getKey k = liftF (GetKey k id)

----

data ConsoleF a
  = PutStrLn String a
  | GetLine (String -> a)
  deriving Functor

type Console = Free ConsoleF

putStrLn' :: String -> Console ()
putStrLn' s = liftF (PutStrLn s ())

getLine' :: Console String
getLine' = liftF (GetLine id)
