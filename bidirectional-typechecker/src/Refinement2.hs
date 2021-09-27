{-# LANGUAGE DataKinds #-}
module TypeChecking.Refinement2 where

-- a simple type checker.

-- type level expressions
data Type = Nat
          | Prod Type Type
          | Arr Type Type
          deriving (Eq, Show)

-- term level expression
-- we embed types within term experssions ast so it's easier to type check.
-- there are techniques to let us get rid of this, e.g we can inference
-- the type of lambda's argument.
-- But for now we just tag it.
data Expr = Var String
          | Zero
          | Suc Expr
          | Pair Expr Expr
          | Fst Type Expr
          | Snd Type Expr
          | Lam String Expr
          | App Type Expr Expr
          deriving (Eq, Show)

-- context
type Context = [(String, Type)]

-- a judgement is made by given type context, current expression, and
-- the type it claims to have.
-- HasType g e t: Claim with Context g, expression e has type t.
data Judgement = HasType Context Expr Type deriving (Show, Eq)

-- given a expression and a type
decomposeHasType :: Context -> Expr -> Type -> Maybe [Judgement]
-- type of variable need to be proofed by looking into the context.
decomposeHasType g (Var x) a = do
  t <- lookup x g
  if a == t then return [] else Nothing

-- further refines base on cases.
decomposeHasType g Zero Nat = return []   -- Zero proofs itself.
decomposeHasType g (Suc m) Nat = return [HasType g m Nat]
decomposeHasType g (Pair m n) (Prod a b) = return [HasType g m a, HasType g n b]
decomposeHasType g (Fst b p) a = return [HasType g p (Prod a b)]
decomposeHasType g (Snd a p) b =  return [HasType g p (Prod a b)]
decomposeHasType g (Lam x m) (Arr a b) = return [HasType ((x, a) : g) m b]
decomposeHasType g (App a m n) b = return [HasType g m (Arr a b), HasType g n a]
decomposeHasType _ _ _ = Nothing

decompose :: Judgement -> Maybe [Judgement]
decompose (HasType g m a) = decomposeHasType g m a

-- now define the proof tree.
data ProofTree = ProofTree Judgement [ProofTree] deriving (Show, Eq)

findProof :: Judgement -> Maybe ProofTree
findProof j = do
  js <- decompose j
  ts <- traverse findProof js
  return $ ProofTree j ts

-- see some examples:

-- >>> j1 = HasType [] Zero Nat
-- >>> findProof j1
-- Just (ProofTree (HasType [] Zero Nat) [])

-- >>> j1 = HasType [] Zero (Arr Nat Nat)
-- >>> findProof j1
-- Nothing

-- suc suc zero : Nat
-- >>> j2 = HasType [] (Suc (Suc Zero)) Nat
-- >>> findProof j2
-- Just (
--   ProofTree (HasType [] (Suc (Suc Zero)) Nat)
--    [ProofTree (HasType [] (Suc Zero) Nat)
--      [ProofTree (HasType [] Zero Nat)
--        []]])

-- \a:NatxNat.(fst a):Nat
-- >>> ctx3 = [("a", Prod Nat Nat)]
-- >>> expr3 = Lam "a" (Fst Nat (Var "a"))
-- >>> typ3 = Arr (Prod Nat Nat) Nat
-- >>> j3 = HasType ctx3 expr3 typ3
-- >>> findProof j3
-- Just (
--  ProofTree (HasType [("a",Prod Nat Nat)]
--                     (Lam "a" (Fst Nat (Var "a")))
--                     (Arr (Prod Nat Nat) Nat))
--    [ProofTree (HasType [("a",Prod Nat Nat),("a",Prod Nat Nat)]
--                         (Fst Nat (Var "a"))
--                         Nat)
--      [ProofTree (HasType [("a",Prod Nat Nat),("a",Prod Nat Nat)]
--                          (Var "a")
--                          (Prod Nat Nat))
--        []]])’’’
