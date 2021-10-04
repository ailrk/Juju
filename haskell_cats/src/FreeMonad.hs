{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module FreeMonad where

import Control.Monad

{- The idea of a free monad is to give a simpler structure more properties. -}


-------------------------------------------------------------------------------
-- free monad
-------------------------------------------------------------------------------

data Free f a
  = Pure a
  | Free (f (Free f a))

unit :: a -> Free f a
unit x = Pure x

bind :: Functor f => Free f a -> (a -> Free f b) -> Free f b
bind (Pure x) f = f x
bind (Free m) f = Free $ (fmap (\n -> bind n f) m)

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free m) = Free ((fmap . fmap) f m)

instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

instance Functor f => Monad (Free f) where
  return = unit
  (>>=) = bind

-------------------------------------------------------------------------------
-- free monad
-------------------------------------------------------------------------------

