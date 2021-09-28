{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Interp.Arith where

import           Data.String
import           Text.ParserCombinators.ReadP
import Data.Char

-- number literals are polymorphic, we can overload for our own type to create
-- literals with specific meaning

data Expr where
  Val :: Integer -> Expr
  Add :: Expr -> Expr -> Expr
  Neg :: Expr -> Expr
  Sub :: Expr -> Expr -> Expr
  Mul :: Expr -> Expr -> Expr
  Abs :: Expr -> Expr
  deriving (Eq, Show)

instance Num Expr where
  fromInteger n = Val n
  e1 + e2 = Add e1 e2
  e1 - e2 = Sub e1 e2
  e1 * e2 = Mul e1 e2
  abs e1 = Abs e1
  negate e1 = Neg e1
  signum = error "no signum"

instance IsString Expr where
  fromString = undefined

{-
   Production rule
   E -> T | E + T | E - T
   T -> F | T * F
   F -> P | + F | - F
   P -> <int> | '(' E ')' | '|' E '|'
-}

token n = string n <* skipSpaces

intp :: ReadP Integer
intp = (munch1 isDigit >>= (return . read)) <* skipSpaces

exprp = (Add <$> termp <* token "+" <*> termp)
    <++ (Sub <$> termp <* token "-" <*> termp)
    <++ termp

termp = (Mul <$> factorp <* token "*" <*> factorp) <++ factorp

factorp = (Neg <$> (token "-" *> primaryp))
      <++ (token "+" *> primaryp)
      <++ primaryp

primaryp = (token "|" *> exprp <* token "|")
       <++ (token "(" *> exprp <* token ")")
       <++ (intp >>= return . Val)

-- >>> readP_to_S exprp "12 + 1 * 2"
-- [(Add (Val 12) (Mul (Val 1) (Val 2)),"")]

-- >>> let e1 = 1 + 2 * 3 :: Expr
-- >>> e1
-- Add (Val 1) (Mul (Val 2) (Val 3))
