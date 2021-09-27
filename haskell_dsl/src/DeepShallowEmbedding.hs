{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module DeepShallowEmbedding where

-- https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/embedding-short.pdf
-- Types of DSL:
-- 1. embeded dsl, langage is embedded within the host langauge, reuse host
--    language features.
--
-- 2. external dsl, you design a entire new language with it's hole amenities.
--    parser, compiler, editor, libraries. etc.

-- Algebraic data type, higher order functions are very useful to design
-- embeded dsl. (For deep embedding and shallow embedding respectively).


-- For embedded dsl, we have further two classifications:
--  1. deep embedding
--        The abstract syntax of the target langauge is represented as data
--        type in the host language, acompany with a eval function that
--        interpret terms of the target languages.
data ExprDeep1 :: * where
  Val :: Integer -> ExprDeep1
  Add :: ExprDeep1 -> ExprDeep1 -> ExprDeep1

evalExprDeep1 :: ExprDeep1 -> Integer
evalExprDeep1 (Val n)   = n
evalExprDeep1 (Add n m) = evalExprDeep1 m + evalExprDeep1 n

--  2. shallow embedding
--        Deeply reuse the host language's functionality, skip over the syntax
--        tree and compute values directly. It's similar to make a set of
--        combinators that works together as a dsl.

type ExprShallow1 = Integer
val_ExprShallow1 :: Integer -> ExprShallow1
val_ExprShallow1 n = n

add_ExrpShallow1 :: ExprShallow1 -> ExprShallow1 -> ExprShallow1
add_ExrpShallow1 a b = a + b
-- or tagless final sometimes called.

-- More jargons:
--    deep embedding => initial approach because the AST corresponds to the
--                      initial algebra
--    shallow embedding => final approach

-------------------------------------------------------------------------------
-- Fold on deep embedding (initial embedding)
data ExprF :: * -> * where
  ValF :: Integer -> ExprF a
  AddF :: ExprF a -> ExprF a -> ExprF a

instance Functor ExprF where
  fmap _ (ValF n) = (ValF n)
  fmap f (AddF n m) = AddF (fmap f n) (fmap f n)

data Deep :: (* -> *) -> * where
  In :: Functor f => f (Deep f) ->  Deep f

type ExprDeep = Deep ExprF

-- compositional interpretation (just folding)

type Algebra f a = f a -> a
