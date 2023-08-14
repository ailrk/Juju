{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module AdjointFunctor where
import Data.Data

class Injective a b where
  to :: a -> b

class (Injective a b, Injective b a) => Iso a b

-- the initial algebra in the category of F-algebra is Fix f.
data Fix f = In (f (Fix f))

----------------------------------------------------------------
-- left adjoints preserve initial objects (L 0 ≡ 0)

-- Void is the initial algebra for haskell type.
data Void
absurd :: Void -> a
absurd _ = undefined

-- Free f is a functor from a category to it's F algebras.
-- it's left adjoint to a forgetful functor going the other way
-- around.
data Free f a = Pure a | Free (f (Free f a))
  deriving (Typeable)

-- From left adjoint, we know (Free f Void ≡ 0)
-- Thus we can conclude: Free f Void ≡ Fix f
instance Functor f => Injective (Free f Void) (Fix f) where
  to (Pure void) = absurd void
  to (Free body) = In (to <$> body)

instance Functor f => Injective (Fix f) (Free f Void) where
  to (In body) = Free (to <$> body)

instance Functor f => Iso (Fix f) (Free f Void)

----------------------------------------------------------------
-- right adjoints preserve initial objects (R 1 ≡ 1)
-- For duality's sake, we have Cofree, a functor from category to it's
-- F-coalgebra.

data Cofree f a = a :< f (Cofree f a) deriving (Typeable)

instance Functor f => Injective (Cofree f ()) (Fix f) where
  to (() :< body) = In (to <$> body)

instance Functor f => Injective (Fix f) (Cofree f ()) where
  to (In body) = () :< (to <$> body)
