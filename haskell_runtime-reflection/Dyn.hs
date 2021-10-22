{-# LANGUAGE GADTs #-}
module Dyn where

import Type.Reflection

data Dynamic where Dyn :: Typeable a => a -> Dynamic

