{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}
module Free.DataTypeALaCarte where


-- http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
-- http://www.cs.nott.ac.uk/~pszgmh/alacarte.pdf

-- first, a la carte means by the menu, if you order a steak you get the steak
-- alone without any sides.

-- a way to address expression problem.
-- We factor out the recursion part out from inductive data type, and use
-- fix point to express arbitrary nesting of data type. Like In (f (Expr f)).
-- Because f is polymorphic, we can easily extend it.

import           Data.Kind

----------------------------------------
-- build up the data type one by one.

-- Expr is the fix point of f. We use it to nest arbitrary f.
type Expr :: (Type -> Type) -> Type
data Expr f = In (f (Expr f))

-- val
type Val :: Type -> Type
data Val e = Val Int
instance Functor Val where fmap f (Val x) = Val x

-- add
type Add :: Type -> Type
data Add e = Add e e
instance Functor Add where fmap f (Add e1 e2) = Add (f e1) (f e2)

----------------------------------------
-- combine expression by taking coproduct of their signatures.

type (:+:) :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data (f :+: g) e = Inl (f e) | Inr (g e)
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl fe) = Inl (fmap f fe)
  fmap f (Inr ge) = Inr (fmap f ge)

-------
-- 1. (Var :+: Add) :: Type -> Type
-- 2. Expr accomondates a arbitrarily nested (Var :+: Add)
-- 3. At each level f can be any branch of the coproduct.

testAdd1 :: Expr (Val :+: Add)
testAdd1 = In
         $ Inr (Add ( In
                    $ (Inl (Val 118)))
                    ( In
                    $ (Inl (Val 1219))))

----------------------------------------
-- algebra and fold on expr

-- algebra specifies how different structure f affect the result.
type Algebra f a = f a -> a

-- cata morphism.
foldExpr :: Functor f => Algebra f a -> Expr f -> a
foldExpr alg (In expr) = alg (fmap (foldExpr alg) expr)


----------------------------------------
-- evaluation

-- fold eval over the data
class Functor f => Eval f where evalAlgebra :: f Int -> Int
instance Eval Val where evalAlgebra (Val x) = x
instance Eval Add where evalAlgebra (Add x y) = x + y
instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr


----------------------------------------
-- automating injection

val :: Int -> Expr Val
val = In . Val

(<+>) :: Expr Add -> Expr Add -> Expr Add
x <+> y = In (Add x y)
