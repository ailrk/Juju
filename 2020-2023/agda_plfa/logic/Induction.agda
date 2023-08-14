module logic.Induction where

-- If you are a functional programmer you already know induction by heart.
-- In curry howard isomorphism it's exactly the same as a normal recurison.
-- In agda we primit only structural recursion, that is recursion works on
-- well founded data type. This allows us to be sure that recursion always
-- terminate, because well founded collection has no infinite decrasing chain.


import Relation.Binary.PropositionalEquality as Eq
-- open the module (add names specified in using)
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_)
open import Function using (_∘_; flip)

-- peano natural numbers
data Nat : Set where
  zero' : Nat
  suc' : Nat → Nat

+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc zero n p =
  begin (zero + n) + p
  ≡⟨⟩   n + p
  ≡⟨⟩   zero + (n + p)
  ∎
+-assoc (suc m) n p =
  begin (suc m + n) + p
  ≡⟨⟩   suc (m + n) + p
  ≡⟨⟩   suc ((m + n) + p)
  -- a relation is congruence for a given function if it's preserved after
  -- the function is applied.
  ≡⟨ cong suc (+-assoc m n p) ⟩
        suc (m + (n + p))
  ≡⟨⟩   suc m + (n + p)
  ∎

-- how to unroll all lemma by hand.

+-assoc2 : ∀ (n p : ℕ) → (2 + n) + p ≡ 2 + (n + p)
+-assoc2 n p =
  begin (2 + n) + p
  ≡⟨⟩   suc (1 + n) + p
  ≡⟨⟩   suc ((1 + n) + p)
  ≡⟨ cong suc (+-assoc1 n p) ⟩
        suc (1 + (n + p))
  ≡⟨⟩   2 + (n + p)
  ∎
  where
    +-assoc1 : ∀ (n p : ℕ) → (1 + n) + p ≡ 1 + (n + p)
    +-assoc1 n p =
      begin (1 + n) + p
      ≡⟨⟩   suc (0 + n) + p
      ≡⟨⟩   suc ((0 + n) + p)
      ≡⟨ cong suc (+-assoc0 n p) ⟩
            suc (0 + (n + p))
      ≡⟨⟩   1 + (n + p)
      ∎
      where
        +-assoc0 : ∀ (n p : ℕ) → (0 + n) + p ≡ 0 + (n + p)
        +-assoc0 n p = begin (0 + n) + p ≡⟨⟩ n + p  ≡⟨⟩ 0 + (n + p) ∎

-- proof right identity
+-identityʳ : ∀ (m : ℕ) → m + zero ≡ m
+-identityʳ zero = begin zero + zero ≡⟨⟩ zero ∎
+-identityʳ (suc m) =
  begin (suc m) + zero
  ≡⟨⟩   suc (m + zero)
  ≡⟨ cong suc (+-identityʳ m) ⟩
        suc m
  ∎

-- we knwo suc m + n ≡ suc (m + n), prove the other direction
+-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc zero n = begin zero + suc n ≡⟨⟩ suc n ≡⟨⟩ suc (zero + n) ∎
+-suc (suc m) n =
  begin suc m + suc n
  ≡⟨⟩   suc (m + suc n)
  ≡⟨ cong suc (+-suc m n) ⟩
        suc (suc (m + n))
  ≡⟨⟩   suc (suc m + n)
  ∎


-- commutativity
+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero = begin m + zero ≡⟨ +-identityʳ m ⟩ m ≡⟨⟩ zero + m ∎
+-comm m (suc n) =
  begin m + (suc n)
  ≡⟨ +-suc m n ⟩
        suc (m + n)
  ≡⟨ cong suc (+-comm m n) ⟩
        suc (n + m)
  ≡⟨⟩   suc n + m
  ∎

-- rearranging corollary
-- rearrange parenthesis however we want.
+-rearrange : ∀ (m n p q : ℕ) → (m + n) + (p + q) ≡ m + (n + p) + q
+-rearrange m n p q =
  begin (m + n) + (p + q)
  ≡⟨ +-assoc m n (p + q) ⟩
      m + (n + (p + q))
  -- how do we use assoc the opposite direction?
  -- sym change the side of equation
  -- if e : x ≡ y, sym e : y ≡ x
  ≡⟨ cong (m +_) (sym (+-assoc n p q)) ⟩
        m + ((n + p) + q)
  ≡⟨ sym (+-assoc m (n + p) q) ⟩
        (m + (n + p)) + q
  ∎

-- -- ! Associativity with rewrite
-- +-assoc' : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
-- +-assoc' zero n p = refl
-- +-assoc' (suc m) n p rewrite +-assoc' m n p = refl

-- -- ! Commutativity with rewrite
-- +-identity' : ∀ (n : ℕ) → n + zero ≡ n
-- +-identity' zero = refl
-- +-identity' (suc n) rewrite +-identity' n = refl

-- +-suc' : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
-- +-suc' zero n = refl
-- +-suc' (suc m) n rewrite +-suc' m n = refl

-- +-comm' : ∀ (m n : ℕ) → m + n ≡ n + m
-- +-comm' m zero rewrite +-identity' m = refl
-- +-comm' m (suc n) rewrite +-suc' m n | +-comm' m n = refl      -- rewrite with two equations

-- -- ! Exerceises
-- -- this can be proved directly
-- +-swap : ∀ (m n p : ℕ) → m + (n + p) ≡ n + (m + p)
-- +-swap m n p =
--     begin
--         m + (n + p)
--     ≡⟨ sym (+-assoc m n p) ⟩
--         (m + n) + p
--     ≡⟨ cong (_+ p) (+-comm m n) ⟩  -- (_+ p) as the function takes m n as parameters
--         (n + m) + p
--     ≡⟨ +-assoc n m p ⟩
--         n + (m + p)
--     ∎

-- *-distrib-+ : ∀ (m n p : ℕ) → (m + n) * p ≡ m * p + n * p
-- *-distrib-+ zero n p =
--     begin
--         (zero + n) * p
--     ≡⟨⟩
--         n * p
--     ≡⟨⟩
--         zero * p + n * p
--     ∎

-- *-distrib-+ (suc m) n p =
--     begin
--         (suc m + n) * p
--     ≡⟨⟩
--         (suc (m + n)) * p
--     ≡⟨⟩
--         p + ((m + n) * p)
--     ≡⟨ cong (p +_) (*-distrib-+ m n p) ⟩
--         p + (m * p + n * p)
--     ≡⟨ sym (+-assoc p (m * p) (n * p)) ⟩
--         (p + m * p) + n * p
--     ≡⟨⟩
--         (suc m) * p + n * p
--     ∎

-- *-assoc : ∀ (m n p : ℕ) → (m * n) * p ≡ m * (n * p)
-- *-assoc zero n p = refl
-- *-assoc (suc m) n p rewrite
--       *-distrib-+ n (m * n) p | *-assoc m n p = refl

-- -- proof small lemmas
-- n*0≡0 : ∀ (n : ℕ) → n * zero ≡ zero
-- n*0≡0 zero = refl
-- n*0≡0 (suc n) = n*0≡0 n

-- n+n*m≡n*[1+m] : ∀ (n m : ℕ) → n + n * m ≡ n * suc m
-- n+n*m≡n*[1+m] zero m =
--     begin
--         zero + zero * m
--     ≡⟨⟩
--         zero
--     ≡⟨⟩
--         zero * suc m
--     ∎

-- n+n*m≡n*[1+m] (suc n) m =
--     begin
--         suc n + suc n * m
--     ≡⟨⟩
--         suc (n + suc n * m)
--     ≡⟨⟩
--         suc (n + (m + n * m))
--     ≡⟨ cong suc (+-swap n m (n * m)) ⟩
--         suc (m + (n + n * m))
--     ≡⟨ cong (suc ∘ _+_ m) (n+n*m≡n*[1+m] n m) ⟩
--         suc (m + (n * suc m))
--     ≡⟨⟩
--         suc m + n * suc m
--     ≡⟨⟩
--         suc n * suc m
--     ∎

-- *-comm : ∀ (m n : ℕ) → m * n ≡ n * m
-- *-comm zero n = sym (n*0≡0 n)
-- *-comm (suc m) n =
--     begin
--         (suc m) * n
--     ≡⟨⟩
--         n + m * n
--     ≡⟨ cong (_+_ n) (*-comm m n) ⟩
--         n + n * m
--     ≡⟨ n+n*m≡n*[1+m] n m ⟩
--         n * (suc m)
--     ∎

-- 0∸n≡0 : ∀ (n : ℕ) → zero ∸ n ≡ zero
-- 0∸n≡0 zero = refl
-- 0∸n≡0 (suc n) = refl

-- ∸-+-assoc : ∀ (m n p : ℕ) → m ∸ n ∸ p ≡ m ∸ (n + p)
-- ∸-+-assoc zero n p =
--     begin
--         zero ∸ n ∸ p
--     ≡⟨ cong (flip _∸_ p) (0∸n≡0 n)⟩
--         zero ∸ p
--     ≡⟨ 0∸n≡0 p ⟩
--         zero
--     ≡⟨ sym (0∸n≡0 (n + p)) ⟩
--         zero ∸ (n + p)
--     ∎

-- ∸-+-assoc (suc m) zero p = refl
-- ∸-+-assoc (suc m) (suc n) p = ∸-+-assoc m n p
