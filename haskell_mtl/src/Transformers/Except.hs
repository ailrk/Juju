{-# LANGUAGE StandaloneKindSignatures #-}
module Transformers.Except where


import Data.Kind
type ExceptT :: Type -> (Type -> Type) -> Type -> Type
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a)}
