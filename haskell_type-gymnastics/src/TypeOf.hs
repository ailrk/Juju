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

module TypeOf where

import Data.Kind
import GHC.Natural

-- one view: class Name as type level function mirrows the
-- term levle function. It takes a type as parameter whch choose
-- the right overload of typeOf.

data Void

class TypeOf a where
  typeOf :: String

instance TypeOf Bool where
  typeOf = "Bool"

instance TypeOf Char where
  typeOf = "Char"

instance (TypeOf a, TypeOf b) => TypeOf (a, b) where
  typeOf = "(" ++ typeOf @a ++ ", " ++ typeOf @b ++ ")"

-- type application allows us to define TypeOf for void.
instance TypeOf Void where
  typeOf = "Void"

instance TypeOf a => TypeOf [a] where
  typeOf = "[" ++ typeOf @a ++"]"
