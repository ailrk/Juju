{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DataTypeALaCarte where


----------------------------------------

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

-- mult
type Mult :: Type -> Type
data Mult e = Mult e e
instance Functor Mult where fmap f (Mult e1 e2) = Mult (f e1) (f e2)

----------------------------------------
-- combine expression by taking coproduct of their signatures.

infixr 1 :+:
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

-- catamorphism.
foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr alg (In expr) = alg (fmap (foldExpr alg) expr)


----------------------------------------
-- evaluation

-- fold eval over the data
class Functor f => Eval f where evalAlgebra :: f Int -> Int
instance Eval Val where evalAlgebra (Val x) = x
instance Eval Add where evalAlgebra (Add x y) = x + y
instance Eval Mult where evalAlgebra (Mult x y) = x * y
instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

----------------------------------------
-- automating injection

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)     -- a partial inverse

instance Functor f => f :<: f where
  inj = id
  prj = Just . id

instance
  {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj = \case
    Inl n -> Just n
    _     -> Nothing

instance {-# OVERLAPPING #-}
  (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  prj hga = case hga of
              Inr n -> prj n
              _     -> Nothing

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj


-- smart constructor.
val :: (Val :<: f) => Int -> Expr f
val = inject . Val


infixr 6 <+>
(<+>) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x <+> y = inject (Add x y)

infixr 7 <@>
(<@>) :: (Mult :<: f) => Expr f -> Expr f -> Expr f
x <@> y = inject (Mult x y)

-- test it out
test2 = let x :: Expr (Add :+: Val) = val 3000 <+> val 1200 <+> val 7
         in eval x

test3 = let x :: Expr (Val :+: Mult) = val 3000 <@> val 2 <@> val 10
         in eval x

test4 = let x :: Expr (Val :+: Mult :+: Add) = val 10 <@> val 2 <+> val 8
         in eval x

-- adding new datatype is extensible. What data types are available is
-- constrained by the functor, so we don't need to modify the existed functoins.

----------------------------------------
-- let's define another interpreter

class Dump f where
  dump :: Dump g => f (Expr g) -> String

instance Dump Val where
  dump (Val i) = show i

instance Dump Add where
  dump (Add e1 e2) = undefined

instance Dump Mult where
  dump (Mult e1 e2) = undefined

instance (Dump f, Dump g) => Dump (f :+: g) where
  dump (Inl x) = dump x
  dump (Inr y) = dump y

-- adding new function is extensible. This works the same as ordinary
-- algebraic data type.

----------------------------------------
-- use the partial inverse `prj`
-- Answer a question: is x in x :+: y?

match :: g :<: f => Expr f -> Maybe (g (Expr f))
match (In t) = prj t
