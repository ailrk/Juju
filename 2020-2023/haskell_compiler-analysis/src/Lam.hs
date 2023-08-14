module Lam where

-- simple lambda calculus based langauge for control flow analysis.

{- BNF
     e ::= n | x | λx.e | e₁ e₂ | let x = e₁ in e₂ | if e₁ then e₂ else e₃
-}

type Name = String

data Expr
  = LAM Name Expr
  | APP Expr Expr
  | VAR Name
  | INT Int
  | LET (Name, Expr) Expr
  | IF_THEN_ELSE Expr Expr Expr

-- helper
