module TypeChecking.BidirectionalTC.TC2 where

-- bidirectional refinement for type checking.

data Type = Nat | Prod Type Type | Arr Type Type | Checked deriving (Show, Eq)

-- our
data Expr = Var String
          | Ann Expr Type   -- type annotation
          | Zero
          | Suc Expr
          | Pair Expr Expr
          | Fst Expr
          | Snd Expr
          | Lam String Expr
          | App Expr Expr
          deriving Show

type Context = [(String, Type)]

data Judgement = Check Context Expr Type  -- type must be provided
               | Synth Context Expr       -- type can be inferred
               deriving (Show)

-- for ([Judgement], [Type] -> Maybe Type), [Judgement] are subproblems,
-- [Type] -> Maybe Type is the function to construct result from results
-- of the subproblem.

-- In this module subproblems must be independent, we can't use the result of
-- one subproblem in another subproblem.

-- rules needs to be checked explicitly.
decompoesCheck :: Context
               -> Expr
               -> Type
               -> Maybe ([Judgement], [Type] -> Maybe Type) -- result can fail.
decompoesCheck g Zero Nat = Just ([], const $ Just Checked)
decompoesCheck g (Suc m) Nat = Just ([Check g m Nat], const $ Just Checked)
decompoesCheck g (Pair m n) (Prod a b) =
  Just ([Check g m a, Check g n b], const $ Just Checked)
decompoesCheck g (Lam x m) (Arr a b) =
  Just ([Check ((x, a) : g) m b], const $ Just Checked)
-- synthesized types can faiil to type check. it's the base case essentially.
decompoesCheck g m a =
  Just ([Synth g m], \[a'] -> if a == a' then Just Checked else Nothing)

-- rules that's be synthed directly
decomposeSynth :: Context
               -> Expr
               -> Maybe ([Judgement], [Type] -> Maybe Type)
decomposeSynth g (Var x) = lookup x g >>= \a -> return ([], const $ Just a)
-- type annotation. you don't need explicit proof
decomposeSynth g (Ann m a) = return ([Check g m a], const $ Just a)
decomposeSynth g (Fst p) = return ([Synth g p], \[t] -> let (Prod a b) = t in Just a)
decomposeSynth g (Snd p) = return ([Synth g p], \[t] -> let (Prod a b) = t in Just b)
decomposeSynth g (App f x) =
  let cb = \[s, t] -> case s of
                        Arr a b | a == t -> Just b
                        _                -> Nothing
   in return ( [Synth g f, Synth g x] , cb)
decomposeSynth _ _ = Nothing

decompose :: Judgement -> Maybe ([Judgement], [Type] -> Maybe Type)
decompose (Check g m a) = decompoesCheck g m a
decompose (Synth g m)   = decomposeSynth g m

data ProofTree = ProofTree Judgement [ProofTree] deriving (Show)

findProof :: Judgement -> Maybe (ProofTree, Type)
findProof j = do
  (js, f) <- decompose j
  tsas <- traverse findProof js
  let (ts, as) = unzip tsas
  a <- f as
  return (ProofTree j ts, a)

-- >>> findProof (Synth [] (Ann (Suc (Suc (Zero))) (Prod Nat Nat)))
-- Nothing

-- >>> findProof (Synth [] (Ann (Suc (Suc (Zero))) Nat))
-- Just
-- (ProofTree (Synth [] (Ann (Suc (Suc Zero)) Nat))
--   [ProofTree (Check [] (Suc (Suc Zero)) Nat)
--     [ProofTree (Check [] (Suc Zero) Nat)
--        [ProofTree (Check [] Zero Nat)
--          []]]],
--  Nat)

-- >>> findProof (Check [] (Lam "p" (Fst (Var "p"))) (Arr (Prod Nat Nat) Nat))
-- Just
-- (ProofTree (Check [] (Lam "p" (Fst (Var "p"))) (Arr (Prod Nat Nat) Nat))
--   [ProofTree (Check [("p",Prod Nat Nat)] (Fst (Var "p")) Nat)
--      [ProofTree (Synth [("p",Prod Nat Nat)] (Fst (Var "p")))
--         [ProofTree (Synth [("p",Prod Nat Nat)] (Var "p"))
--            []]]],
-- Checked)

-- >>> findProof (Synth [("p",Prod Nat Nat)] (Fst (Var "p")))
-- Just
--  (ProofTree (Synth [("p",Prod Nat Nat)] (Fst (Var "p")))
--    [ProofTree (Synth [("p",Prod Nat Nat)] (Var "p"))
--      []],
--  Nat)
