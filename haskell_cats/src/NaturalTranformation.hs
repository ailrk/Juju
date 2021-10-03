{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TypeOperators            #-}
module NaturalTranformation where

import           Data.Kind

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------
--  - endofunctor               T: X -> X
--  - natural transformation   mu: T x T -> T
--  - natural transformation   nu: I -> T
--  where   mu(mu(T x T) x T) = mu(T x mu(T x T))
--          mu(nu(T)) = T = mu(T(nu))

-------------------------------------------------------------------------------
-- Functor:
-------------------------------------------------------------------------------
--   Mapping from category to category. Mapping between categories means we
--   need to map objects to objects, morphisms to morphisms.
--   formally:
--   For category C, D, a function F : C -> D is a mapping  that
--      1. forall X in C, exists F(X) in D
--      2. forall moprhism f : X -> Y in C, exists F(f): F(X) -> F(Y) in D.
--          where F(id_x) = id_F(x) forall X in C
--                F(g.f) = F(g) . F(f) for all morphisms f:X->Y and f:Y->Z
--
--   feels like a homomorphism but functor requires preserving identity.
--
--   In haskell there is only one category Hask, and objects are haskell
--   type, morphisms are haskell functions. so only endo functors in haskell.
--   (Only functor from hask to hask)

type Identity :: Type -> Type
newtype Identity a = Identity { runIdentity :: a }

-- we define how the functor map from object to object.
instance Functor Identity where
  fmap f (Identity a) = (Identity (f a))

-- nu: I -> T
return' :: Monad t => Identity :-> t
return' = NaturalTransfrom (return . runIdentity)

-------------------------------------------------------------------------------
-- composition of functors
-------------------------------------------------------------------------------

-- mu: T x T -> T

-------------------------------------------------------------------------------
-- Natural transformation: transformation between functors
-------------------------------------------------------------------------------

{-
    Natural transformation map from functor to functor without touching the
    object.
    let C, D be categories, F: C -> D, G: C -> D be functors between C, D.
    then the natural transformation nu from F to G is a `family of morphisms`
    that:
     1. forall X in C, nuX: F(X) -> G(X)
     2. for all morphisms f: X -> Y in C, `nuY o F(f) = G(F) o nuX`

                                nuX
            X          F(X) ------------> G(X)
            |           |                   |
            | f         | F(f)              | G(f)
            |           |                   |
            v           v       nuY         v
            Y          F(y) -------------> G(Y)

    There is only one category in haskell, functor only maps from type to
    type. Thus natrual transformation only map function from type to type to
    function from type to type.
-}

-- Natural transformation
newtype (f :: Type -> Type) :-> (g :: Type -> Type) =
  NaturalTransfrom { eta :: forall x. f x -> g x }

listToMaybe :: [] :-> Maybe
listToMaybe = NaturalTransfrom go
  where go []    = Nothing
        go (x:_) = Just x

maybeToList :: Maybe :-> []
maybeToList = NaturalTransfrom go
  where go Nothing = []
        go (Just x) = [x]

-- an automorphism.
reverse' :: [] :-> []
reverse' = NaturalTransfrom reverse



