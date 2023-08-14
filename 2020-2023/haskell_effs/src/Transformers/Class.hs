{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Transformers.Class where

import Data.Kind

type MonadTrans :: ((Type -> Type) -> Type -> Type) -> Constraint
class MonadTrans t where
  lift :: Monad m => m a -> t m a
