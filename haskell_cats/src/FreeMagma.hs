module FreeMagma where

-------------------------------------------------------------------------------
-- magma
-------------------------------------------------------------------------------

{- A magma is like a semigroup but without being associative.
    It's way too generic!

  A free magma over a set is a binary tree with data sorted as leaves

  A magma is a set of elements with binary operation close in that set.
  Note: magma requires closure, no partial binop.
-}

class Magma a where
  (<.>) :: a -> a


--------------------------------------------------------------------------------
-- free magma
-------------------------------------------------------------------------------

