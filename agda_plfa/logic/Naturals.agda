module algda_plfa.logic.Naturals where


-- # Naturals are inductive datatype #
-- definition of natural number in agda
data ℕ : Set where
    zero : ℕ
    suc : ℕ → ℕ

-- here zero and suc are constructors of the datatype.
-- ! Inductive definition:
--  Base case: zero ∈ ℕ
--  Inductive step: m ∈ ℕ → suc m ∈ ℕ
-- so zero is ∈ ℕ, (suc (suc zero)) is also ∈ ℕ

-- # unpacking the inference rules #
-- inference rule consists:
--      zero or more judgments (hypotheses)
--      -----------------------------------
--      one judgment (conclusion)

-- # use pragma #
--
-- this load N directly use 0, 1 .. as short hand for zero, (succ zero) ..

{-# BUILTIN NATURAL ℕ  #-}


import Relation.Binary.PropositionalEquality as Eq
-- open the module (add names specified in using)
open Eq using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

-- # Operations on natruals are recursive functions #
-- we use pattern matching when constructors appear on the left hand side of an equation.
_+_ : ℕ → ℕ → ℕ
zero + n = n
(suc m) + n = suc (m + n)
-- the definition of addition is recursive, because
--  add is defined based on definition of add.
--  because the inductive definition of natrual number circularity is not a problem.
--  Larger numbers are defined in terms of smaller numbers (such definition is called well founded).

-- ! Try type hole!

--  dummy case name _ can be reused
_ : 2 + 3 ≡ 5  -- after : is a type.
_ =
    begin
        2 + 3
    ≡⟨⟩     -- is short hand for
        (suc (suc zero)) + (suc (suc (suc zero)))
    ≡⟨⟩     -- inductive case (associativity)
        suc ((suc zero) + (suc (suc (suc zero))))
    ≡⟨⟩     -- inductive case (associativity agagin)
        suc (suc (zero + (suc (suc (suc zero)))))
    ≡⟨⟩     -- base case
        suc (suc (suc (suc (suc zero))))
    ≡⟨⟩     -- is longhand for
        5
    ∎

-- in compact form
_ : 2 + 3 ≡ 5
_ =
    begin
        2 + 3
    ≡⟨⟩
        suc (1 + 3)
    ≡⟨⟩
        suc (suc (0 + 3))
    ≡⟨⟩
        suc (suc 3)
    ≡⟨⟩
        5
    ∎


-- check reflexive
-- agda compute 2 + 3 and compare the value with 5 immediately
-- (a binary relation is reflexive if every value relates to itself)
_ : 2 + 3 ≡ 5
_ = refl

-- ! How does agda run these code?
--  Agda check is each term simplifies to the same value.
--  So you can omit some lines and the whole term is still valid. Extra terms are good for readability.

-- ! 2 + 3 ≡ 5 is a type
-- ! chain of equations are terms of the given type
-- ! you can also think it as:
--      type is the proposition
--      term is the evidence
-- ! This duality is central to how we formalize concepts in Agda.

-- Exercise 3 add 4
_ : 3 + 4 ≡ 7
_ =
  begin
    3 + 4
  ≡⟨⟩
    suc (2 + 4)
  ≡⟨⟩
    suc (suc (1 + 4))
  ≡⟨⟩
    suc (suc (suc (0 + 4)))
  ≡⟨⟩
    suc (suc (suc 4))
  ≡⟨⟩
    7
  ∎


-- # Multiplication #
_*_ : ℕ → ℕ → ℕ
zero * n = zero
(suc m) * n = n + (m * n)

_^_ : ℕ → ℕ → ℕ
m ^ 0 = 1
m ^ 1 = m
m ^ (suc n) = n * (m ^ n)

-- # Monus #
_∸_ : ℕ → ℕ → ℕ
m ∸ zero = m
zero ∸ suc n = zero
suc m ∸ suc n = m ∸ n

_ =
  begin
    3 ∸ 2
  ≡⟨⟩
    2 ∸ 1
  ≡⟨⟩
    1 ∸ 0
  ≡⟨⟩
    1
  ∎

-- # Precedence #
-- provide specific precedence for operators.
--
infixl 6 _+_  _∸_
infixl 7 _*_

-- ! Exercise: Bin
-- A more efficient representation of natural numbers uses a binary rather than
-- a unary system.

data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

-- 1001 can be represented as  ⟨⟩ I O O I
inc : Bin -> Bin
inc (⟨⟩) = ⟨⟩ I
inc (n O) = n I
inc (n I) = (inc n) O

-- you can proof this kind of equivalence with refl
-- but it's better to do more practice
inc-case0 : inc (⟨⟩ O) ≡ ⟨⟩ I
inc-case0 = refl

inc-case1 : inc (⟨⟩ I) ≡ ⟨⟩ I O
inc-case1 =
    begin
        inc (⟨⟩ I)
    ≡⟨⟩
        (inc ⟨⟩) O
    ≡⟨⟩
        ⟨⟩ I O
    ∎

inc-case2 : inc (⟨⟩ I O) ≡ ⟨⟩ I I
inc-case2 =
    begin
        inc (⟨⟩ I O)
    ≡⟨⟩
        ⟨⟩ I I
    ∎

inc-case3 : inc (⟨⟩ I I) ≡ ⟨⟩ I O O
inc-case3 =
    begin
        inc (⟨⟩ I I)
    ≡⟨⟩
        (inc (⟨⟩ I)) O
    ≡⟨⟩
        ⟨⟩ I O O
    ∎

to : ℕ → Bin
to 0 = ⟨⟩ O
to (suc n) = inc (to n)

from : Bin → ℕ
from (⟨⟩) = 0
from (n O) = 2 * from n
from (n I) = suc (2 * from n)
