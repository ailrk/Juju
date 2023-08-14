{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module MuNu where
class Injective a b where to :: a -> b
class (Injective a b, Injective b a) => Iso a b

-- these mu nu things are normal fix point with some constrains.


-- fix point class
class FixPoint f n  where
  out :: f -> n
  into :: n -> f

--------------------------------------------------------------------------
-- the initial algebra in the category of F-algebra is Fix f.
data Fix f = In { unFix :: (f (Fix f)) }
instance FixPoint (Fix f) (f (Fix f)) where
  into = In
  out (In f) = f

-- recursive type as its fold. (inductive finite data)
-- it's the iniitial fix point of f.
newtype Mu f = Mu { unMu :: forall a . (f a -> a) -> a }
instance Functor f => FixPoint (Mu f) (f (Mu f)) where
  into fmu = Mu $ \f -> f (flip unMu f <$> fmu)
  out = flip unMu $ fmap into


-- recursive type as its unfold (coinductive infinite data)
-- it's the terminal fix point of f.
data Nu f = forall a. Nu  (a -> f a) a

instance Functor f => FixPoint (Nu f) (f (Nu f)) where
  into = Nu (fmap out)
  out (Nu f a) = Nu f <$> f a

--------------------------------------------------------------------------
-- mu, nu are isomorphic

instance Injective (Mu f) (Nu f) where
  to (Mu f) = undefined
