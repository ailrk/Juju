{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Data.Internal.Vector
  (
  ) where

import           Control.DeepSeq
import           Data.Kind
import           Data.Maybe
import           Data.Proxy
import qualified Data.Vector.Sized as VectorSized
import           GHC.TypeLits

newtype Vector (n :: Nat) a = Vector (VectorSized.Vector n a)
  deriving (Eq, Show, Functor, Applicative, Foldable, Traversable, NFData)


instance (KnownNat n, Read a) => Read (Vector n a) where
  readsPrec p s = [ ((Vector . fromJust . VectorSized.toSized) ys, t)
                  | (ys, t) <- xs
                  , length ys == vectorLenght]
    where
      xs = readsPrec p s
      vectorLenght = fromIntegral $ natVal (Proxy :: (Proxy n))
