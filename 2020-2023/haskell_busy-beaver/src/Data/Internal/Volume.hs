{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Data.Internal.Volume
  (
  ) where


import GHC.TypeLits
import Data.Kind

import Data.Vector
import Data.Internal.Matrix


newtype Volume (m :: Nat) (n :: Nat) (depth :: Nat) (a :: Type) = Volume a
