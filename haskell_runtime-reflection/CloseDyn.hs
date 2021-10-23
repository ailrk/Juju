{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeOperators  #-}
module CloseDyn where


-- Dynamic type in a closed world

import           Data.Kind
import           Data.Type.Equality

-- the idea is defining indexed TypeRep. it's not extensible, so
--  it's called closed world.
data TypeRep (a :: k) where-- Note the polymorphic kind, important for splitApps
  TBool :: TypeRep Bool
  TInt :: TypeRep Int
  TProd :: TypeRep x -> TypeRep y
        -> TypeRep (TypeRep x, TypeRep y)
  TSum :: TypeRep x -> TypeRep y
       -> TypeRep (Either x y)

eqT :: TypeRep a -> TypeRep b -> Maybe (a :~: a)
eqT TBool TBool = Just Refl
eqT TInt TInt = Just Refl
eqT (TProd a1 b1) (TProd a2 b2) = do
  eqT a1 a2    -- if we can't get a relf we return Nothing
  eqT b1 b2
  return Refl
eqT (TSum a1 b1) (TSum a2 b2) = do
  eqT a1 a2
  eqT b1 b2
  return Refl
eqT _ _ = Nothing

data SomeTypeRep where SomeTypeRep :: TypeRep a -> SomeTypeRep

data AppResult t where
  App :: TypeRep a -> TypeRep b -> AppResult (a b)

splitApps :: TypeRep a -> Maybe (AppResult a)
splitApps = undefined
