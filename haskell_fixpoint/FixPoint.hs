{-# LANGUAGE DeriveFunctor #-}

module FixPoint where

import           Control.Arrow

data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)


-- one way to define Unary might be `Unary String Expr`. We abstract away the
-- Expr part and turns it into a universal type.
data Expr a
  = Index a a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Lit
  deriving (Show, Eq, Functor)

data Fix f = In (f (Fix f))

-- out is a helper function to evaluate the Term f
out :: Fix f -> f (Fix f)
out (In t) = t


n = let term = In (Binary (In (Paren (In (Literal (IntLit 3)))))
                           "+"
                           (In (Binary (In (Binary (In (Literal (IntLit 3)))
                                           "*"
                                           (In (Literal (IntLit 10)))))
                                        "-"
                                        (In (Literal (IntLit 10))))))
      in out term
