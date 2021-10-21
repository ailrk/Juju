{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lens where

import           Data.Functor.Const
import           Data.Functor.Identity

import           Control.Applicative
import           Data.Function         ((&))

-- we want to set and get.
-- we have these two represnetation.
-- In fact they are isomorphic.
-- one fact: a is always contained in s.
type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

set :: forall s a. Lens s a -> (a -> s -> s)
set lens x = runIdentity . lens (Identity . const x)

over :: forall s a. Lens s a -> (a -> a) -> s -> s
over lens f = runIdentity . lens (Identity . f)

view :: forall s a. Lens s a -> s -> a
view lens = getConst . lens Const

-- multi foci lens
type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s
