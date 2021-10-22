{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Type where

import           Data.Type.Equality
import           Type.Reflection    (eqTypeRep,splitApps, TyCon)

-- reflection (rtti) requires you to stuff some kind of runtime information
-- but what kind?

-- In C++ you maybe attach a string at compile time that might uniquely
-- represent the type.
-- or you can mannually wrap dynamic values with it's type representation in a
-- pair so you can check at runtime yourself.

-- In haskell we use typeclass to generate a TypeRep :: * type that indexed by
-- actual type to be the runtime type representation.
-- All type can have runtime type info has instance Typeable, which gives you
-- method typeRep that gets the TypeRep at runtime.

data OrderingT a b where
  LTT :: OrderingT a b
  EQT :: OrderingT t t
  GTT :: OrderingT a b

-- our Runtype type rep indexed by type
-- GHC will silently generate TyCon for each type declaration.
data TypeRep' (a :: k) where
  TrApp :: TypeRep' a -> TypeRep' b -> TypeRep' (a b)
  TrTyCon :: TyCon -> TypeRep' k -> TypeRep' (a :: k)

-- [HOLE] in real life GHC will generate TyCon bind to each type declaration.
-- So it's another compiler hole.
data TyCon' = TyCon' { tcModule:: Module', tcName :: String }
data Module' =  Module' { modPkg :: String, modName :: String }

class Typeable' a where
  typeRep' :: TypeRep' a

-- we know types are the same after perform runtime checking, but how do we
-- convince the compiler that we did the check and two values will be the same
-- type at runtime?

-- We need some compiler primtives to do so

data TypeRepX where TypeRepX :: TypeRep' a -> TypeRepX
