{-# LANGUAGE DeriveGeneric #-}
module Transformers.Writer where

import           Control.Applicative
import           Control.Monad              (MonadPlus (..), ap)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Functor.Classes
import           Data.Functor.Contravariant
import           Data.Functor.Identity
import           Data.Kind
import           Data.Maybe
import           GHC.Generics
import           Signatures
import           Transformers.Class

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) } deriving (Generic)


