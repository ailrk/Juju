{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module FinalTagless2 where

-- use typeclass to chose the return type.
-- https://legacy.cs.indiana.edu/ftp/techreports/TR65.pdf

import           Data.Text.Lazy.Builder


data ExprInterp a = Expr { unExpr :: a }


class Expr (m :: * -> *) where
  bool :: Bool -> m Bool
  int :: Int -> m Int
  leq :: m Int -> m Int -> m Bool
  and' :: m Bool -> m Bool -> m Bool
  or' :: m Bool -> m Bool -> m Bool
  not' :: m Bool -> m Bool


instance Expr ExprInterp where
  bool b = Expr b
  int i = Expr i
  leq (Expr x) (Expr y) = bool (x <= y)
  and' (Expr x) (Expr y) = bool (x && y)
  or' (Expr x) (Expr y) = bool (x || y)
  not' = Expr . not . unExpr


expr1 = unExpr $ and' (or' (bool True) (leq (int 2) (int 3))) (bool True)


-- another implementation

data PersistValue
  = PersistInt64 Int
  | PersistBool Bool
  deriving Show

data PersistExpr a = PersistExpr { unPersistExpr :: (Builder, [PersistValue]) }

instance Expr PersistExpr where
  bool b = PersistExpr ("?", [PersistBool b])
  int i = PersistExpr ("?", [PersistInt64 i])
  leq (PersistExpr (x1, v1)) (PersistExpr (x2, v2)) = PersistExpr (x1 <> " <= " <> x2, v1 <> v2)
  or' (PersistExpr (x1, v1)) (PersistExpr (x2, v2)) = PersistExpr (x1 <> " OR " <> x2, v1 <> v2)
  and' (PersistExpr (x1, v1)) (PersistExpr (x2, v2)) = PersistExpr (x1 <> " AND " <> x2, v1 <> v2)
  not' (PersistExpr (x, v)) = PersistExpr ("NOT " <> x, v)

expr2 = unPersistExpr $ and' (or' (bool True) (leq (int 2) (int 3))) (bool True)


-- again make a comparison with  initial tagged encoding.
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


{-@ Conclusion
    1. The name final tagless corresponds ot "initial" "tagged" encoding, we hide different
       return type behind a universal return type

    2. If you use GADT, it becomes initial taggless encoding, the return type info is determined
       based on the data constructor.

    3. The problem with initial encoding is they are not extensible. You can't simply add a
       data constructor without modifying all functions.

  @-}
