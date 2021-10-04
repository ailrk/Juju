module IMP where

-- simple IMPerative langauge for control flow analysis.

{- BNF
    AExp : a ::= n | x | a0 + a1 | a0 ∗ a1 | a0 − a1
    BExp : b ::= true | false | a0 = a1 | a0 ≤ a1 | b0 ∨ b1 | b0 ∧ b1 | ¬b
    Com : c ::= skip | x := a | c0 ; c1 | if b then c1 else c2 | while b do c
-}

data IMP = A AExpr | B BExpr | C Comb

data AExpr
  = NAME String
  | INT Int
  | ADD AExpr AExpr
  | MUL AExpr AExpr
  | SUB AExpr AExpr

data BExpr
  = TRUE
  | FALSE
  | EQ AExpr AExpr
  | GTE AExpr AExpr
  | OR BExpr BExpr
  | AND BExpr BExpr
  | NEG BExpr

data Comb
  = SKIP
  | ASSIGN String AExpr
  | SEQ Comb Comb
  | IF_THEN_ELSE BExpr Comb Comb
  | WHILE BExpr BExpr
