{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module FinalTagless.FinalTagless where

{- tagless final:
     1. write code with overloaded functions
     2. run code with any suitable implementation
-}

-- the gist is about adding constraints on polymorphic type variables.
-- the fact that we can add more constraint to make a type variable support
-- more functions make it extensible.

-- define the simple language
class LitExpr a where
  lit :: Int -> a
  add :: a -> a -> a

instance LitExpr Int where
  lit = id
  add = (+)

instance LitExpr String where
  lit = show
  add a b = a ++  " + " ++ b

-- define a little extension
class Minus a where
  minus :: a -> a -> a

instance Minus Int where
  minus = (-)

instance Minus String where
  minus a b = a ++ " - " ++ b

-- define a little extension
class Mult a where
  mult :: a -> a -> a

instance Mult Int where
  mult = (*)

instance Mult String where
  mult a b = mconcat [ a1 : b1 : []  | a1 <- a, b1 <- b ]

-- use the edsl
expr1 :: forall a . (LitExpr a, Minus a) => a
expr1 = add (lit 12) (minus (lit 32) (lit 3))

expr1Int = expr1 @Int
expr1String = expr1 @String

expr2 :: forall a . (LitExpr a, Minus a, Mult a) => a
expr2 = mult (add (lit 12) (minus (lit 32) (lit 3))) (lit 5)

expr2Int = expr2 @Int
expr2String = expr2 @String
