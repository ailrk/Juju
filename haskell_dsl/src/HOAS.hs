{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module HOAS where


{-@ Higher order abstract syntax.
    A technique for implementing the lambda calculus in
    a langauge where the binders of the lambda expression
    map direcly onto lambda binders of the host language
    to give substitution machinery in custom language
    by exploiting the host language's implementation.
@-}


data ExprHOAS a where
  Con :: a -> ExprHOAS a
  Lam :: (ExprHOAS a -> ExprHOAS b) -> ExprHOAS (a -> b)
  App :: ExprHOAS (a -> b) -> ExprHOAS a -> ExprHOAS b

i :: ExprHOAS (a -> a)
i = Lam (\x -> x)

k :: ExprHOAS (a -> b -> a)
k = Lam (\x -> Lam (\_ -> x))

s :: ExprHOAS ((a -> b -> c) -> (a -> b) -> (a -> c))
s = Lam (\x -> Lam (\y -> Lam (\z -> App (App x z) (App y z))))

skk = App (App s k) k

eval :: ExprHOAS a -> a
eval (Con v)     = v
eval (Lam f)     = \x -> eval (f (Con x))
eval (App e1 e2) = (eval e1) (eval e2)


{-@ PHOAS
    A slightly different form of HOAS.
    Using lambda datatype parameterized over the binder types.
    Evaluation requires unpacking into a separate value type
    to wrap the lambda expression.
@-}

data ExprPHOAS a
  = VarP a
  | AppP (ExprPHOAS a) (ExprPHOAS a)
  | LamP (a -> ExprPHOAS a)
  | LitP Integer


data ValuePHOAS
  = Vlit Integer
  | VFun (ValuePHOAS -> ValuePHOAS)
