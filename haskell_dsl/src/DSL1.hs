{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

-- random:
-- what's good?
--  1. code lens eval and prop test
--  2. wingman and case split
--  3. virtual text for types.
--  4. type holes and relevant binding
--  5. stylish-haskell
--  6. type definition
--  7. telescope: (file, grep, mark)
--  8. quickfix for import

module DSL1 where

import           Control.Monad (ap)
import           Prelude       hiding (succ, exp)

-------------------------------------------------------------------------------
-- untyped typed lambda calculus with dsl
-- basically it's partial evalution with defuntionalization.

-- We shove programs into dataype so they are defunctionalized and properly
-- typed for specific problem. This allows us to create a program without
-- evluating it, and it brings some benefits:
--  1. we can write program to build up programs.
--  2. we can shove in useful information when building up the program.
--  3. we can control when to evaluate the program.
--  4. we can write different combinators that supports different features,
--     all compile to the same data type, so single source of truth.
--
-- The dsl has it's original semantics, to restore it, we define an evaluation
-- function that actually evaluate the constructed data type.
--
-- To some extend it's like the reverse of macro, instead of handling all
-- program with macro, we define a small language with limited functionalities,
-- and handle the construction of the program as normal datat type.
-- Not like macro, we need to manually convert the program back to haskell to
-- run it.
--
-- Here we try to implement a simplyed typed lambda calculus with HOAS.
-- (higher order abstract syntax) which reuse the meta languages' binder for
-- the object language. Here haskell is the meta language, and LCExpr is the
-- object language. Of course, it's important only when the object langauge
-- has the conecpt of a binder (lambda binds a name as parameter).
-- HOAS is simply to write, but the idea of define a small language and
-- "compile" to it works the same for all embeded dsl.


data LCExpr a where
  Const :: a -> LCExpr a
  Abst :: (LCExpr a -> LCExpr b) -> LCExpr (a -> b)
  App :: LCExpr (a -> b) -> LCExpr a -> LCExpr b

-- Base line interpreter:
-- A eval functions that restores the original semantic of the program.
eval :: LCExpr a -> a
eval (Const a) = a
eval (Abst f)  = eval . f . Const
eval (App f e) = eval f (eval e)

showConst :: Show a => LCExpr a -> String
showConst (Const a) = show a
showConst _         = error "can't show"

-- all we need is to pull the value out, apply f and box it back
-- the monadic interface helps us to compose function that generate data type.
instance Functor LCExpr where
  fmap f x = Const . f $ eval x

instance Applicative LCExpr where
  pure a = Const a
  (<*>) = ap

instance Monad LCExpr where
  return = pure
  m >>= k = k . eval $ m


-- prop> eval (Const 1) == 1
-- +++ OK, passed 1 test.

-- >>> let f = (Abst (\x -> Const $ eval x + 1))
-- prop> eval (App f (Const 1)) == 2
-- +++ OK, passed 1 test.

-------------------------------------------------------------------------------
-- lanaguage

class LC m where
  apply :: m (a -> b) -> m a -> m b
  lambda :: (m a -> m b) -> m (a -> b)
  val :: a -> m a

instance LC LCExpr where
  apply = App
  lambda = Abst
  val = Const

-- >>> let f = (lambda (\x -> return $ eval x + 1))
-- prop> (eval $ apply f (val 3)) == 4
-- +++ OK, passed 1 test.

-------------------------------------------------------------------------------
-- church encoding

-- note the LCExpr type essentially defunctionalized lambda application so
-- we have a data type that represents lambdas to work with.
-- All other operations are based on partial evaluates to the data type.

z :: LCExpr ((a -> b) -> a -> a)
z = lambda . const . lambda $ id

succ :: LCExpr (((a -> c) -> b -> a) -> (a -> c) -> b -> c)
succ = lambda $ \n -> lambda $ \f -> lambda $ \x -> f <*> (n <*> f <*> x)

-- handy
n1 = succ <*> z
n2 = succ <*> n1
n3 = succ <*> n2
n4 = succ <*> n3
n5 = succ <*> n4
n6 = succ <*> n5
n7 = succ <*> n6
n8 = succ <*> n7
n9 = succ <*> n8

plus :: LCExpr ((a -> b) -> (a -> c) -> (c -> a) -> c -> b)
plus = lambda $ \m -> lambda $ \n -> lambda $ \f -> lambda $ \x ->
  m <*> (f <*> (n <*> (f <*> x)))

mult n = lambda $ \m -> lambda $ \n -> lambda $ \f -> lambda $ \x ->
  m <*> (n <*> (f <*> x))

exp :: LCExpr (a -> (a -> b) -> b)
exp = lambda $ \m -> lambda $ \n -> n <*> m

pred :: LCExpr ((((c -> a) -> (a -> b) -> b) -> (x -> z) -> (y -> y) -> d) -> c -> z -> d)
pred =
  lambda $ \n ->
  lambda $ \f ->
  lambda $ \x ->
    n <*> (lambda $ \g ->
           lambda $ \h ->
             h <*> (g <*> f))
      <*> (lambda $ \u -> x)
      <*> (lambda $ \u -> u)

toInt n = eval $ n <*> lambda ((+1) <$>) <*> (val 0)
-- >>> toInt n4
-- 4

-- >>> let x = n5 <*> lambda ((+1) <$>) <*> (val 0)
-- >>> let n16 = (exp <*> n2 <*> n4)
-- >>> let y = n16 <*> lambda ((+1) <$>) <*> (val 0)
-- prop> eval x == 5
-- +++ OK, passed 1 test.
-- prop> eval y == 16
-- +++ OK, passed 1 test.


-------------------------------------------------------------------------------
-- scott encoding


-------------------------------------------------------------------------------
-- HOAS to target, so called correct by construction compiler.
-- How to compile HOAS representation?
-- Reuse binder of the host language.
-- Two transformations:
--  1. closure conversion.
--  2. hoisting.
--  3. do cps transform.
-- More on this:
-- https://www.iro.umontreal.ca/~monnier/tp-compile.pdf
compileToJs :: Show a => LCExpr a -> String
compileToJs c@(Const _) = showConst c
compileToJs (Abst f) = undefined
compileToJs (App f e) = undefined

jsLambda :: (Show p, Show body) => LCExpr p -> LCExpr body -> String
jsLambda p body = "(" <> compileToJs p  <> ") => (" <> compileToJs body <> ")"
-- >>> jsLambda (val "a") (val "a")
-- "(\"a\") => (\"a\")"


-- BTW the abstract syntax doesn't use HOAS is called
-- FOAS (first order abstact syntax), and it's basically the ast you first think
-- of.


-------------------------------------------------------------------------------
-- In FOAS how do we encode binder informations? We uses de bruijn indices to
-- indicate the scope (which layer the paramter is at).
-- What is De brujin indice?
-- well:
-- \x.\y.x => \ \2
-- \x.\y.\z.x z (y z) => \ \ \ 3 1 (2 1)
-- \z.(\y.y(\x.x))(\x.z x) => \ (\ 1 (\ 1)) (\ 2 1)
--
-- number represenest the index of lambda head it's referring to. Note the
-- free variale z in the thrid example.
--
-- FOAS + de brujin indices is hard to implement. You need to deal with
-- 1. renaming
-- 2. unique new name generator
-- 3. capture avoiding substitutino (alpha conversion)
