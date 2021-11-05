{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Transformers.Class where

import Data.Kind

type MonadTrans :: ((Type -> Type) -> Type -> Type) -> Constraint
class (forall (m :: Type -> Type) . Monad m => Monad (t m)) => MonadTrans t where
  lift :: Monad m => m a -> t m a
