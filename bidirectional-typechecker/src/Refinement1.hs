{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TypeChecking.Refinement1 where



-- refinement base on a proof tree.
-- We start from the conclusion and refine backwards. Each expression
-- is a node in the tree. A bar represent a rule that justify the node.
-- A node withtout a bar above it is an unjustified statement. It's either
-- a goal or a hypothesis.
--
--     --------         ---------
--      B ^ A             B ^ A
--     --------         ---------
--        A                B
--    ---------------------------
--          A ^ B
--    ---------------------------
--      (B ^ A) -> (A ^ B)
--
data Nat = Zero | Suc Nat deriving (Eq, Show)

-- this is a simplified version, we put the ast into judgemnet directly
-- if the ast is large, we make it a separate type, and judgement takes
-- an ast and some context information to help it refines.
data Judgement = Equal Nat Nat | Plus Nat Nat Nat deriving (Show, Eq)

-- form a statement
statement1 :: Judgement
statement1 = Plus (Suc (Suc Zero)) (Suc Zero) (Suc (Suc (Suc Zero)))

-- An judgement may have multiple proofs

-- to decompose a statement to subproblem.
-- 0 = 0
-- suc x = suc y => x = y
decomposeEqual :: Nat -> Nat -> [[Judgement]]
decomposeEqual Zero Zero       = return []
decomposeEqual (Suc x) (Suc y) = return [Equal x y]
decomposeEqual _ _             = []

-- zero + y = z -> y = z
-- suc x + y = suc z -> x + y = z
decomposePlus :: Nat -> Nat -> Nat -> [[Judgement]]
decomposePlus Zero y z          = return [Equal y z]
decomposePlus (Suc x) y (Suc z) = return [Plus x y z]
decomposePlus _ _ _             = []

-- >>> decomposePlus (Suc Zero) (Suc Zero) (Suc (Suc Zero))
-- Just [Plus Zero (Suc Zero) (Suc Zero)]

decompose :: Judgement -> [[Judgement]]
decompose (Equal x y)  = decomposeEqual x y
decompose (Plus x y z) = decomposePlus x y z

-- now define the proof tree.
data ProofTree = ProofTree Judgement [ProofTree] deriving (Show, Eq)

--  A proof tree example:
--  zero = zero
-- ------------------------------
--  suc zero = suc zero
-- ----------------------------------
-- zero + suc zero = suc zero
-- -------------------------------------------
--  suc zero + suc zero = suc (suc zero)
-- --------------------------------------------------
--  suc (suc zero) + suc zero = suc (suc (suc zero))
--
-- We eventually hit the base case.

findProof :: Judgement -> [ProofTree]
findProof j = do
  js <- decompose j
  ts <- traverse findProof js
  return (ProofTree j ts)

-- it's essentially a evaluator applies reduction rules.

-- >>> findProof (Plus (Suc Zero) (Suc Zero) (Suc (Suc Zero)))
-- [ProofTree (Plus (Suc Zero) (Suc Zero) (Suc (Suc Zero)))
--  [ProofTree (Plus Zero (Suc Zero) (Suc Zero))
--    [ProofTree (Equal (Suc Zero) (Suc Zero))
--     [ProofTree (Equal Zero Zero)
--      []]]]]
