{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module ReifyNat where

import Data.Kind
import GHC.Natural
import GHC.Base (assert)

data Nat = Z | S Nat


type ReifyNat :: Nat -> Constraint
class ReifyNat a where
  reifyNat :: Natural

instance ReifyNat Z where
  reifyNat = 0

instance ReifyNat a => ReifyNat (S a) where
  reifyNat = 1 + (reifyNat @a)

-- doing type level arithmetic and reify to term

type Add :: Nat -> Nat -> Nat
type family Add a b where
  Add Z y = y
  Add (S x) y = Add x (S y)

type Five = Add (S (S (S Z))) (S (S Z))
test = assert (reifyNat @Five == 5) ()
