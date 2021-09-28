{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Interp.FOAS where


{-
   First order abstract syntax
   This is how you would normally write an interpreter. you need to handle
   lambda name capturing manually, but because all values are defunctionalized,
   it's easier to compile to different targets.
-}


-------------------------------------------------------------------------------
-- FOAS representation
-------------------------------------------------------------------------------
data Expr = Var Char
          | Lam Char Expr
          | Expr Expr


-------------------------------------------------------------------------------
-- de brujin indicies
-------------------------------------------------------------------------------

{- de brujin indices convert variable names to the distance from it's usage
    to it's first binder
     \x.\y.x => \ \2
     \x.\y.\z.x z (y z) => \ \ \ 3 1 (2 1)
     \z.(\y.y(\x.x))(\x.z x) => \ (\ 1 (\ 1)) (\ 2 1)
-}

