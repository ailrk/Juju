module TypeChecking.BidirectionalTC.TC1 where


-- Problem:
-- Sometimes a jugement only have partial information. We want to refine
-- in those cases.

data Nat = Zero | Suc Nat deriving (Show, Eq)

-- Idea:
-- Originally we had our plus judgement in form (+ a b c), where all three
-- expressions involved are part of the judgement. But not we want to allow
-- one to be absent.
--
-- We can separate the arguments into input and output two categories
-- For form (+ a b _) where we want to know the Nat for _,
--
-- Then the actual jugement is (+ a b), with only two parameters. We can
-- do the same refinement as before, but when the term is reduced to
-- an irreduciable subgoal, we want to reconstruct a result somehow.
--
-- e.g for (+ (s z) z), it will goes:
--    (+ (s z) z) -> (+ (z z)) -> ?
--    Now we want to collect the subgoals along the tree to get a (s(s z))
--    e.g pass continuations at each level, once we hit the end subgoal we
--    collect subgoals back up.
--
-- Hence bidirectional.

-- look at the judgemnet (+ a b _). in this module _ is just evaluating
-- a + b, but it can be other things too. e.g it can be a type, we refine
-- the goal and collect the results, and evetually get a type of the
-- expression.

-- In another word, we can have a interpreter evaluate from expression to
-- types. Expressions are part of the judgement, and type checking is just
-- keep refining the judgemnets and see if we can hit the bottom case.
--
-- A benefit of the bidirectional approach is that we are able to put a
-- hole in arbitrary position of a expression and make a jugement to
-- infer known with known parts.


data Judgement = Plus12 Nat Nat  -- + a b _
               | Plus13 Nat Nat  -- + a _ c
               deriving Show

-- + a b _
-- this one is deterministic. You can always get another Nat by adding two
-- nats.
decompose12 :: Nat -> Nat -> Maybe ([Judgement], [Nat] -> Nat)
decompose12 Zero y    = Just ([], \_ -> y)
-- remove a suc on x add it to z
decompose12 (Suc x) y = Just ([Plus12 x y], \[z] -> Suc z)

-- + a _ c
-- this one is partial, e.g plus 3 _ 2 is not defined for Nat.
decompose13 :: Nat -> Nat -> Maybe ([Judgement], [Nat] -> Nat)
decompose13 Zero z          = Just ([], \_ -> z)
decompose13 (Suc x) (Suc z) = Just ([Plus12 x z], \[x] -> x)
decompose13 _ _             = Nothing

decompose (Plus12 n m) = decompose12 n m
decompose (Plus13 n m) = decompose13 n m

data ProofTree = ProofTree Judgement [ProofTree] deriving Show

-- running the refinement algorithm we get a proof tree and a resulting
-- value.
-- e.g For + a b _ it's the nat for _.
findProof :: Judgement -> Maybe (ProofTree, Nat)
findProof j = do
  (js, f) <- decompose j
  tns <- traverse findProof js
  let (ts, ns) = unzip tns
  return (ProofTree j ts, f ns)

-- >>> findProof (Plus12 Zero (Suc (Suc Zero)))
-- Just (ProofTree (Plus12 Zero (Suc (Suc Zero))) [], Suc (Suc Zero))

-- >>> findProof (Plus12 (Suc Zero) (Suc (Suc Zero)))
-- Just (ProofTree (Plus12 (Suc Zero) (Suc (Suc Zero)))
--        [ProofTree (Plus12 Zero (Suc (Suc Zero)))
--          []],
--      Suc (Suc (Suc Zero)))

--- >>> findProof (Plus13 (Suc Zero) (Suc (Suc Zero)))
-- Just
-- (ProofTree (Plus13 (Suc Zero) (Suc (Suc Zero)))
--   [ProofTree (Plus12 Zero (Suc Zero))
--     []],
--  Suc Zero)
