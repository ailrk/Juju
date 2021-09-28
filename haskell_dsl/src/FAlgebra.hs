{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module FAlgebra where

------------------------------------------------------------------------------
-- What is an algebra?
------------------------------------------------------------------------------
{-
   Common notion of algebra from abstract algebra usually entails a set
   elements, some binary operations on top of it, and a set of axioms.

   e.g A group is a set G with a binary operation * such that
     1. ∃e ∈ G, ∀a ∈ G s.t a * e = e * a = a (identity)
     2. ∀ a b c ∈ G, (a * b) * c = a * (b * c)
     3. ∀a ∃a⁻¹ s.t a * a⁻¹ = e.

   Many other definitions follow the similar format. F algebra abstract the
   definition format, so we have one sturcture to describe all algebras.
-}

------------------------------------------------------------------------------
-- All you need for f algebra:
------------------------------------------------------------------------------
--  1. A functor
--  2. A function
--  3. A type

type Algebra f a = f a -> a

------------------------------------------------------------------------------
-- Commutative digram for F algebra
------------------------------------------------------------------------------
{-
                α
       F(A) ---------> A
        ^              |
   F(f) |              | f
        |              V
       F(B) ---------> B
                β
-}

------------------------------------------------------------------------------
-- Universal algebra
------------------------------------------------------------------------------
