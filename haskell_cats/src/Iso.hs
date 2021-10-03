{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Iso where

-- little utility

class Injective a b where
  to :: forall b1 a1. (b1 ~ b, a1 ~ a) => a -> b

instance Injective a a where
  to = id

class (Injective a b, Injective b a) => Iso a b where

from :: forall b a. (Iso a b) => b -> a
from = to

instance Iso a a


