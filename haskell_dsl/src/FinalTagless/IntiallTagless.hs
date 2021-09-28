{-# LANGUAGE GADTs #-}

module FinalTagless.IntiallTagless where


{-
   Like initial tagged encoding, initial tagless also uses algebraic datatype
   to encode the abstract syntax.

   Now instead return a `Result` sum type, we use gadt.
-}

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Leq :: Expr Int -> Expr Int -> Expr Bool
  And :: Expr Bool -> Expr Bool -> Expr Bool
  Or :: Expr Bool -> Expr Bool -> Expr Bool
  Not :: Expr Bool -> Expr Bool

{-
   With gadt the constructor encode the type a, we nolonger need a tagged
   result.
-}

eval :: Expr a -> a
eval (I i)               = i
eval (B b)               = b
eval (Not (B n))         = not n
eval (Leq (I i1) (I i2)) = i1 <= i2
eval (And (B b1) (B b2)) = b1 && b2
eval (Or (B b1) (B b2))  = b1 || b2

-- or use phantom type with type constraint.

data LC a
  = forall b . (a ~ b) => Val b
  | forall b c . (a ~ (b -> c)) => Lam (LC b -> LC c)
  | forall b c . (a ~ c) => App (b -> c) (LC b)

evalLC :: LC a -> a
evalLC (Val b) = b
evalLC (Lam f) = \x -> evalLC (f (Val x))
evalLC (App f b) = f (evalLC b)

-- what's in common is that they both don't need a tagged result.
