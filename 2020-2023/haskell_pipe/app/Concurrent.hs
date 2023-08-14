{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Concurrent where

import Pipe

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad


newtype Input a = Input (STM (Maybe a))
newtype Output a = Output (a -> STM Bool)

type Mailbox a = (Output a, Input a)
