{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module FinalTagless where
import           Data.Text.Lazy.Builder (Builder)


-- https://legacy.cs.indiana.edu/ftp/techreports/TR65.pdf

{-@ Tagless initial and tagless final.

@-}

-- model language with algebraic data type is called initial encoding.
-- given the ast below, how do we infer return type from constructor?
-- normally you will think of using GADT.
--
-- We can also hide the type info behind a common result type.

-- Problem: Data Type saves too much information, thus works on datatype
--          force us to handle each cases.
data Expr' a
  = I Int
  | B Bool
  | Leq (Expr' Int) (Expr' Int)
  | And (Expr' Bool) (Expr' Bool)
  | Or (Expr' Bool) (Expr' Bool)
  | Not (Expr' Bool)
  deriving (Eq, Show)

-- each Result comes with a constructor, we call this a tag.
data Result = BoolR Bool
            | IntR Int

-- so this should be initial tagged encoding.
eval' :: Expr' a -> Result
eval' (B b) = BoolR b
eval' (I i) = IntR i
eval' (Leq e1 e2) =
  let IntR i1 = eval' e1
      IntR i2 = eval' e2
   in BoolR (i1 <= i2)
eval' (And e1 e2) =
  let BoolR b1 = eval' e1
      BoolR b2 = eval' e2
   in BoolR (b1 && b2)
eval' (Or e1 e2) =
  let BoolR b1 = eval' e1
      BoolR b2 = eval' e2
   in BoolR (b1 || b2)


-- Thus writing with GADt is initial tagless encoding.
-- the type of a is implied by the type constructor.
data Expr_ a where
  I_ :: Int -> Expr_ a
  B_ :: Bool -> Expr_ a
  Leq_ :: Expr_ Int -> Expr_ Int -> Expr_ Bool
  And_ :: Expr_ Bool -> Expr_ Bool -> Expr_ Bool
  Or_ :: Expr_ Bool -> Expr_ Bool -> Expr_ Bool
  Not_ :: Expr_ Bool -> Expr_ Bool


eval_ i@(I_ _)               = i
eval_ b@(B_ _)               = b
eval_ (Not_ (B_ n))          = B_ (not n)
eval_ (Leq_ (I_ i1) (I_ i2)) = B_ (i1 <= i2)
eval_ (And_ (B_ b1) (B_ b2)) = B_ (b1 && b2)
eval_ (Or_ (B_ b1) (B_ b2))  = B_ (b1 || b2)


-- Both examples above uses initial encoding.
-- we also have final encoding, where doesn't uses constructors.

-- data type becomes just `Expr a`
newtype Expr a = Expr { unExpr :: a }

bool :: Bool -> Expr Bool
bool b = Expr b

int :: Int -> Expr Int
int i = Expr i

leq :: Expr Int -> Expr Int -> Expr Bool
leq (Expr e1) (Expr e2) = Expr (e1 <= e2)

and :: Expr Bool -> Expr Bool -> Expr Bool
and (Expr b1) (Expr b2) = Expr (b1 && b2)

or :: Expr Bool -> Expr Bool -> Expr Bool
or (Expr b1) (Expr b2) = Expr (b1 || b2)

not' :: Expr Bool -> Expr Bool
not' = Expr . not . unExpr

-- in final encoding, we switch the role of data type and functions. Now
-- functions are the constructor and all values are tagged by default.

lit' :: Int -> Int
lit' =  id

add' :: Int -> Int -> Int
add' = (+)

a = (add' (lit' 2) (lit' 4))


-- for string
lit'' :: String -> String
lit'' = id

add'' :: String -> String -> String
add'' = (++)

b = (add'' (lit'' "asd") (lit'' "asd"))


-- abstract out to return different types.

class LitExpr a where
  lit :: Int -> a
  add :: a -> a -> a

instance LitExpr Int where
  lit = id
  add = (+)

instance LitExpr String where
  lit = show
  add a b = a ++  " + " ++ b

class Minus a where
  minus :: a -> a -> a

instance Minus Int where
  minus = (-)

instance Minus String where
  minus a b = a ++ " - " ++ b

expr1 :: LitExpr a => Minus a => a
expr1 = add (lit 12) (minus (lit 32) (lit 3))

expr1AsInt :: Int
expr1AsInt = expr1 + 12

expr1AsString :: String
expr1AsString = expr1

{-@ quick recap:
    - tagles initial (GADT) allows data constructor to carry the type information,

    - tagless final data constructor to carry the type information, and it makes types extensible.
      - extensible as you can simply add a new function that return the result type.
      - constructors can carry info becasue it's essentially a function, which has a return type
        you can specify.

    - Tagged initial encoding requires you to use a universal result type to return different
      return type.

    - If the data type doesn't need to be change, tagless initial can be more convinent as you can
      pattern match on constructors.

    - If the data type needs to be extensible and there is only one operation act on it, tagless
      final is probably the simplest way.

    - If the data type needs to be extensible and you have different version of funcions, abstract
      constructors with typeclass, and make instances for different result types.

      The idea is similar to wrap in newtype to work with different monoids.
      Or you can make an instances for a completely different type, the usage is quit flexible.
@-}
