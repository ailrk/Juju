{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}

module Free.FAlgebra where

----------------------------------------
-- algebra and it's dual coalgebra
import Data.Kind

type    Algebra :: (Type -> Type) -> Type -> Type
newtype Algebra f a = Algebra { runAlgebra :: f a -> a }

type    CoAlgebra :: (Type -> Type) -> Type -> Type
newtype CoAlgebra f a = CoAlgebra { runCoAlgebra :: a -> f a }


-- initial algebra, final algebra
-- Initial algebra is just Fix.
type    Initial :: (Type -> Type) -> Type
newtype Initial f = In (f (Initial f))

type    Final :: (Type -> Type) -> Type
newtype Final f = Fi { unFi :: (f (Final f)) }

----------------------------------------
-- define some recursion schemes.

catamorphism :: Functor f
             => Algebra f a
             -> Initial f
             -> a
catamorphism alg@(Algebra phi) (In x) = phi (fmap (catamorphism alg) x)

anamorphism :: Functor f
            => (CoAlgebra f a)
            -> a
            -> Final f
anamorphism coalg@(CoAlgebra phi) seed = Fi (fmap (anamorphism coalg) (phi seed))


----------------------------------------
-- list

-- free list
data ListF a x = Nil | Cons a x  deriving Functor

map' :: (a -> b) -> Initial (ListF a) -> Initial (ListF b)
map' f xs = catamorphism phi xs
  where
    phi = Algebra $ \n ->
      case n of
        Nil       -> In Nil
        Cons x xs -> In (Cons (f x) xs)

trans' :: Initial (ListF a) -> [a]
trans' = catamorphism phi
  where
    phi = Algebra $ \n ->
      case n of
        Nil       -> []
        Cons x xs -> x:xs

append' :: Initial (ListF a) -> Initial (ListF a) -> Initial (ListF a)
append' = catamorphism phi
  where
    phi = Algebra $ \n ->
      case n of
        Nil       -> id
        Cons x xs -> \ys -> In (Cons x (xs ys))
