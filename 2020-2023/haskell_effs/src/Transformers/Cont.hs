{-# LANGUAGE DeriveGeneric #-}
module Transformers.Cont where

import           Control.Applicative
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Data.Functor.Identity
import           GHC.Generics
import           Transformers.Class

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r } deriving Generic
type Cont r = ContT r Identity


cont :: ((a -> r) -> r) -> Cont r a
cont f = ContT $ \c ->  Identity $ f (runIdentity . c)

runCont :: Cont r a -> (a -> r) -> r
runCont = undefined

evalCont :: Cont r r -> r
evalCont m = undefined

mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont = undefined

withCont :: ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont = undefined

-- delimited continuation
