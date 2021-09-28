{-# LANGUAGE DeriveFunctor #-}

module FixPoint where

import           Control.Arrow

{-@ Fix Point Of Functor
    Sometimes we have recursive types. E.g binary tree

    We can abstract away the recursive part with universal type.
    To make it recursive again we find the fix point of the data type.

    doing so is desiable when you want to separate the primitive representation
    of a data type and it's recursive form.
@-}

data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)


-- e.g one way to define Unary might be `Unary String Expr`. We abstract away the
-- Expr part and turns it into a universal type.
data Expr a
  = Index a a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Lit
  deriving (Show, Eq, Functor)


-- use a fix point to tight the recursive knot.
-- What does it mean?
--    What is f? it's (Expr a)
--    so we have In (Expr (Fix f))
--    further expand,  we have In (Expr ((In (Expr (Fix f)))))
--    this keep going.

-- Note the original Expr a is not recurisve at all.

-- We are just using the Fix type to create a recursive type externally.
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


{-@ Conclusion

    1. fixed point of function f means ∃x. f(f(f(f..(x)))) = x. In another word, a
       fixed point is a value that mapped to itself by some function.
        we say x is the fixed point of f.

    2. but how does fixed point relates to Y combinator?
        fix f = f (fix f)
              = f (f (fix f))
              = f (f (f (fix f)))
              ...

        fix f is the x, f still is the f.  So the fix poinf of f is (fix f).
        Ah I see. (fix f) is the fix point.

        what's the definition of fix?
          fix f = (\x -> f (x x)) (\x -> f (x x))

    3. why is it a big deal?
          Curry paradox.
             you have fix point in lambda calclus.
          -> no guarantee terminate.
          -> lambda calculus is unsound.

    4. the equivalence of fix point operator is on paper, what's the operational
       semantics when we execute it?
          it's just beta reduction. Each time you make a new substitution, a new
          expression with the same form emerge.

    5. but this only build up the infinite expression of fffff, how do you do useful
       things with it?
          well with functions that have only one paramter it will never halt.
          in that case it doesn't matter how function gets applied because you will never
          get an answer.

          You need multiple parameters for functions that do halt with their fix point.
          doing so you can signal the base case on the extra parameter.

          When evaluation hit the base case, it will evaluate the chain of application
          built by fix, which is how things get evaluated.

    6. fix point in type.
          fix point at the type level just creates a recursive type, recurse on the
          type parameter.
          Fix t is the fix point of t.

    7. fix pint of type accommondates arbitrary nested types.
          It's the key idea of using fixed point at the type level. It doesn't matter
          how deep the data type is nested, as long as it's a instance of the fixed point
          of the type it will tye check.
@-}
