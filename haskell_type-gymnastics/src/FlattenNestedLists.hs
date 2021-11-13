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

{-# LANGUAGE FlexibleInstances #-}
module FlattenNestedLists where

import Data.Kind


type family ElementOf a where
  ElementOf [[a]] = ElementOf [a]
  ElementOf [a] = a

class Flatten a where
  flatten :: a -> [ElementOf a]

instance (ElementOf [a] ~ a) => Flatten [a] where
  flatten = id

-- generating new instance use existed one.
instance {-# OVERLAPS #-} Flatten [a] => Flatten [[a]] where
  flatten = flatten . concat

test1 = flatten [[[1 :: Int, 2], [1, 2], [1, 2]], [[1, 2], [2, 3]]]
