{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module TypeChecking.BidirectionalTC.TC3 where

-- Stil the same birectional algorithm, but this time we want to be able to
-- reuse the result of one subproblem on another.

-------------------------------------------------------------------------------
-- Language
import           Data.Kind (Constraint)
data Type = Nat | Prod Type Type | Arr Type Type deriving (Show, Eq)

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

-------------------------------------------------------------------------------
-- Problems
--
-- defunctionalize problems.
type Problems :: (* -> *) -> * -> *
data Problems j r where
  Fail :: Problems j r
  Done :: r -> Problems j r
  SubProblem :: j s -> (s -> Problems j r) -> Problems j r

--
subProblem :: j r -> Problems j r
subProblem j = SubProblem j $ \x -> Done x

type Decomposable :: (* -> *) -> Constraint
class Decomposable judgement where
  decompose :: judgement r -> Problems judgement r

findProof :: Decomposable j => j r -> Maybe r
findProof j = solve (decompose j)

solve :: Decomposable j => Problems j r -> Maybe r
solve Fail             = Nothing
solve (Done x)         = Just x
solve (SubProblem j f) = findProof j >>= \x -> solve (f x)

-------------------------------------------------------------------------------
instance Functor (Problems j) where
  fmap f Fail             = Fail
  fmap f (Done x)         = Done (f x)
  fmap f (SubProblem p g) = SubProblem p (fmap f . g)

instance Applicative (Problems j) where
  pure = Done
  pf <*> px = pf >>= \f -> px >>= \x -> return (f x)

instance Monad (Problems j) where
  return = Done
  Fail >>= g           = Fail
  Done x >>= g         = g x
  SubProblem p f >>= g = SubProblem p $ \x -> f x >>= g

-------------------------------------------------------------------------------
-- Judgement

data Judgement r where
  Check :: Context -> Expr -> Type -> Judgement ()
  Synth :: Context -> Expr -> Judgement Type

decomposeCheck :: Context -> Expr -> Type -> Problems Judgement ()
decomposeCheck g (Pair m n) (Prod a b) =
  do { subProblem (Check g m a);
       subProblem (Check g m b)
     }
decomposeCheck g (Lam x m) (Arr a b) = subProblem (Check ((x, a) : g) m b)
decomposeCheck g m a =
  do { a2 <- subProblem (Synth g m);
       if a == a2
          then return ()
          else Fail
     }

decomposeSynth :: Context -> Expr -> Problems Judgement Type
decomposeSynth g (Var x) =
  case lookup x g of
    Nothing -> Fail
    Just a  -> return a
decomposeSynth g (Ann m a) = subProblem (Check g m a) >> return a
decomposeSynth g (Fst p) =
  do { t <- subProblem (Synth g p);
       case t of
         Prod a b -> return a
         _        -> Fail
     }
decomposeSynth g (Snd p) =
  do { t <- subProblem (Synth g p);
       case t of
         Prod a b -> return b
         _        -> Fail
     }
decomposeSynth g (App f x) =
  do { t <- subProblem (Synth g f);
       case t of
         Arr a b -> subProblem (Check g x a) >> return b
         _       -> Fail
     }
decomposeSynth g m = Fail


instance Decomposable Judgement where
  decompose (Check g m a) = decomposeCheck g m a
  decompose (Synth g m) = decomposeSynth g m


-- >>> findProof (Check [] (Lam "p" (Fst (Var "p"))) (Arr (Prod Nat Nat) Nat))
-- Just ()

-- >>> findProof (Check [] (Lam "p" (Fst (Var "p"))) (Arr Nat Nat))
-- Nothing

-- >>> findProof (Synth [("p",Prod Nat Nat)] (Fst (Var "p")))
-- Just Nat
