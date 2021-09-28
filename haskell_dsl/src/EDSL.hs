{-# LANGUAGE GADTs #-}
module EDSL where

-- we have shallow dsl and deep dsl.
-- shallow dsl is just a style of writing library that only
-- exposing combinators and allows the user write logics with
-- provided combinators.
--
-- The benefit is you can not only design a concise set of vocabularies
-- to describe the problem, but extend it with haskell's facilities.

-- Shallow dsl is just an style, not really some deep techniques. It works
-- very well when we need a small langauge to describe and execute some domain
-- specific logics. For example, simulate sequential logics...

-- The problem of shallow dsl is it must execute to something, there is no way to
-- obtain the structure of the dsl since it's just normal haskell program.

-- For instance, it's hard to have a shallow dsl that generate llvm ir, it's just too
-- messy. What we really want for doing this types of work is to convert the langauge in
-- to some structures first. Like an ast.

-- So here deep dsl comes into play. We encode the target operations of the dsl as datatypes,
-- and provide cominator style inteface. But this time, exeuting combinators generate the
-- AST instead of run the logic direcly.
-- Once the AST is built, we can serve it for different purposes. A common use case is to hook
-- up the same AST with different backends.

-- The process of evaluating combinators and generating an AST representation can be though as
-- a "half program". We evaluated the program, but instead of get the result, we stop it in the
-- middle of the process. It's called "partial evaluation"


data Expr where
  Val' :: Integer -> Expr
  Add' :: Expr -> Expr -> Expr
  Sub' :: Expr -> Expr -> Expr
  Mul' :: Expr -> Expr -> Expr
  Abs' :: Expr -> Expr
  deriving (Eq, Show)


instance Num Expr where
  fromInteger n = Val' n
  e1 + e2 = Add' e1 e2
  e1 - e2 = Sub' e1 e2
  e1 * e2 = Mul' e1 e2
  abs e1 = Abs' e1
  signum = error "no signum"

-- this gives you the structure of the program rather
-- than the result of it.
-- We are free to interpret the structure however we want.

-- >>> e1
-- Mul (Add (Lit 1) (Lit 2)) (Lit 3)
e1 = 1 + 2 * 3 :: Expr

-- >>> f e1
-- Add (Add (Lit 1) (Mul (Lit 2) (Lit 3))) (Lit 1)
f :: Expr -> Expr
f x = x + 1



-- Deep embedding for circuit simulator

data Signal a where
  Register :: a -> Signal a -> Signal a
  Mux2 :: Signal Bool -> (Signal a, Signal a) -> Signal a
  Lit :: a -> Signal a
  Add :: Signal a -> Signal a -> Signal a
  Var :: String -> Signal a

instance Num (Signal a) where
   a + b = Add a b

mux2 :: Signal Bool -> (Signal a, Signal a) -> Signal a
mux2 c (a, b) = Mux2 c (a, b)

register :: a -> Signal a -> Signal a
register d s = Register d s


-- a problem with deep embedding is that we might ended up with infinite loops
-- when evalutating the dsl into ast.

-- Program written in the dsl sometimes needs some recursion. In shallow dsl that
-- just means run some functions recursively.
-- However, in deep embeded dsl it means the program will generate a subtree of the
-- AST recursively, which is usually not what we want.

-- solutions:
-- 1. indicate loop as an explicit effect with monad.
-- 2. observable sharing.

-- the interaction between the host language and the edsl.
-- 1. the host language can serve as a macro langauge to generate edsl.
-- 2. monadic reification
