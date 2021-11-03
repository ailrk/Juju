module agda_plfa.logic.Relations where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Nat.Properties using (+-comm; +-identityʳ; *-comm; *-identityˡ)

-- Relations --
-- Relation is generalized functions, x R y only states

-- It's interesting to list relations by properties.
--
--    * means have, X means have the opposite prop, empty means doesn't apply
--                             Refl       Sym     Trans      Conectedness   -
-- Directed graph          |                                                →
-- Undirected graph        |               *
-- Dependency              |    *          *
-- Tournament              |    X          X
-- Preorder                |    *                  *                        ≤
-- Total Preorder          |    *                  *              *         ≤
-- Partial order           |    *          X       *                        ≤
-- Strict partial order    |    X          X       *                        <
-- Total order             |    *          X       *              *         ≤
-- Strict total order      |    X          X       *              *         <
-- Partial equivalence rel |               *       *
-- Equivalence relation    |    *          *       *                      ~, ≡

-- Note all orders are transitive. Strict orders are irreflexive, because
-- a < b is not b < a. This force elements in the set to form a strict linear
-- chain.

-- TODO study orders

-- Though no global type inference, agda has implicit parameter wrapped with {}.

-- here we define a well founded paritial order.
data _≤_ : ℕ → ℕ → Set where
    z≤n : ∀ {n : ℕ} → zero ≤ n
    s≤s : ∀ {m n : ℕ} → m ≤ n → suc m ≤ suc n

infix 4 _≤_


-- note how m and n are inferred.
_ : 2 ≤ 4
_ = s≤s (s≤s z≤n)   -- 0 ≤ ? → 0 ≤ 2 → 1 ≤ 3 → 2 ≤ 4

-- this is what get inferred
_ : 2 ≤ 4
_ = s≤s {1} {3} (s≤s {0} {2} (z≤n {2}))


-- you can assign implicit parameter by names.
_ : 2 ≤ 4
_ = s≤s  {m = 1} {n = 3} (s≤s {m = 0} {n = 2} (z≤n {n = 2}))

_ : 2 ≤ 4


-- decrease m by 1 until it hits 0.
_ : 5 ≤ 10
_ = s≤s (s≤s (s≤s (s≤s (s≤s z≤n))))


_ : 3 ≤ 10
_ = s≤s {m = 2} {n = 9}
    (s≤s {m = 1} {n = 8}
    (s≤s {m = 0} {n = 7}
    (z≤n {n = 7})))


-- # Inversion #
inv-s≤s : ∀ {m n : ℕ} → suc m ≤ suc n → m ≤ n
inv-s≤s (s≤s m≤n) = m≤n

inv-z≤n : ∀ {m : ℕ} → m ≤ zero → m ≡ zero
inv-z≤n z≤n = refl

-- Recall partial order:
--     reflexive:       ∀ n. the relation nRn holds
--     transitive:      ∀ m, n. p, (mRn ∧ nRp → mRp)
--     anti-symmetric:  ∀ m, n. mRn ∧ nRm → m ≡ n

-- Reflexivity --

≤-refl : ∀ {n : ℕ} → n ≤ n
≤-refl {zero} = z≤n
≤-refl {suc n} = s≤s ≤-refl   -- again use inductive hypothesis.


-- ! Transitivity
≤-trans : ∀ {m n p : ℕ} → m ≤ n → n ≤ p → m ≤ p
≤-trans z≤n _ = z≤n
≤-trans (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans m≤n n≤p)

-- ! Antisymmetry
≤-antisym : ∀ {m n : ℕ} → m ≤ n → n ≤ m → m ≡ n
≤-antisym z≤n z≤n = refl
≤-antisym (s≤s m≤n) (s≤s n≤m) = cong suc (≤-antisym m≤n n≤m)


-- ! Total
-- total order is also called linear order. Everything relates to some other
-- things and you have a chain that contains all elements a < b < c ...
--   Strict Total order have
--     irreflexsivity
--     transitive
--     contected        if a not = b, thne a < b or b < a
--


data Total (m n : ℕ) : Set where
  forward : m ≤ n → Total m n
  flipped : n ≤ m → Total m n

-- note the above defintion create a datatype with parameter. It's the
-- same as the following indexed type:

data Total' : ℕ → ℕ → Set where
  forward' : ∀ {m n : ℕ} → m ≤ n → Total' m n
  flipped' : ∀ {m n : ℕ} → n ≤ m → Total' m n

≤-total : ∀ (m n : ℕ) → Total m n
≤-total zero n = forward z≤n
≤-total (suc m) zero = flipped z≤n
≤-total (suc m) (suc n) with ≤-total m n
... | forward m≤n = forward (s≤s m≤n)
... | flipped n≤m = flipped (s≤s n≤m)

-- This works the same.
≤-total' : ∀ (m n : ℕ) → Total m n
≤-total' zero n = forward z≤n
≤-total' (suc m) zero = flipped z≤n
≤-total' (suc m) (suc n) = go (≤-total' m n)
  where
    go : Total m n → Total (suc m) (suc n)
    go (forward m≤n) = forward (s≤s m≤n)
    go (flipped m≤n) = flipped (s≤s m≤n)

-- Monotonicity --
-- is an operator monotonic with regard to the ordering?
-- e.g is + monotonic over ≤?

-- given ordering for terms on the right side.
+-monoʳ-≤ : ∀ (n p q : ℕ)
  → p ≤ q
  -----
  → n + p ≤ n + q
+-monoʳ-≤ zero p q p≤q = p≤q
+-monoʳ-≤ (suc n) p q p≤q = s≤s (+-monoʳ-≤ n p q p≤q)

-- given ordering for terms on the left side, use associativity
-- and rewrite to simplify.
+-monoˡ-≤ : ∀ (m n p : ℕ)
  → m ≤ n
  -----
  → m + p ≤ n + p
+-monoˡ-≤ m n p m≤n rewrite +-comm m p | +-comm n p = +-monoʳ-≤ p m n m≤n

+-mono-≤ : ∀ (m n p q : ℕ)
  → m ≤ n
  → p ≤ q
  -----
  → m + p ≤ n + q
+-mono-≤ m n p q m≤n p≤q = ≤-trans (+-monoˡ-≤ m n p m≤n) (+-monoʳ-≤ n p q p≤q)

-- prove multiplication is also monotonic over ≤.
*-monoʳ-≤ : ∀ (n p q : ℕ) → p ≤ q → n * p ≤ n * q
*-monoʳ-≤ zero _ _ _ = z≤n
*-monoʳ-≤ (suc n) p q p≤q = +-mono-≤ p q (n * p) (n * q) p≤q (*-monoʳ-≤ n p q p≤q)

*-monoˡ-≤ : ∀ (m n p : ℕ) → m ≤ n → m * p ≤ n * p
*-monoˡ-≤ m n p m≤n rewrite *-comm m p | *-comm n p = *-monoʳ-≤ p m n m≤n

*-mono-≤ : ∀ (m n p q : ℕ) → m ≤ n → p ≤ q → m * p ≤ n * q
*-mono-≤ m n p q m≤n p≤q = ≤-trans (*-monoˡ-≤ m n p m≤n) (*-monoʳ-≤ n p q p≤q)


-- strict inequality --

infix 4 _<_

data _<_ : ℕ → ℕ → Set where
  z<n : ∀ {n : ℕ} → zero < suc n
  s<s : ∀ {m n : ℕ} → m < n → suc m < suc n

data _>_ : ℕ → ℕ → Set where
  s>z : ∀ {n : ℕ} → suc n > zero
  s>s : ∀ {m n : ℕ} → m > n → suc m > suc n

-- for indexed type you don't need to write the name anyway.
<-trans : ∀ {m n p : ℕ} → m < n → n < p → m < p
<-trans z<n (s<s n<p) = z<n
<-trans (s<s m<n) (s<s n<p) = s<s (<-trans m<n n<p)

-- trichotomy
-- if there is a property you want to prove, define what it is
-- as a data type and construct an instance of it.
data Trichotomous (m n : ℕ) : Set where
  tri< : m < n → Trichotomous m n
  tri≡ : m ≡ n → Trichotomous m n
  tri> : m > n → Trichotomous m n

<-trichotomy : ∀ (m n : ℕ) → Trichotomous m n
<-trichotomy zero zero = tri≡ refl
<-trichotomy zero (suc n) = tri< z<n
<-trichotomy (suc m) zero = tri> s>z
<-trichotomy (suc m) (suc n) with <-trichotomy m n
... | tri< m<n = tri< (s<s m<n)
... | tri≡ m≡n = tri≡ (cong suc m≡n)
... | tri> m>n = tri> (s>s m>n)

+-monoʳ-< : ∀ (n p q : ℕ) → p < q → n + p < n + q
+-monoʳ-< zero p q p<q = p<q
+-monoʳ-< (suc n) p q p<q = s<s (+-monoʳ-< n p q p<q)

+-monoˡ-< : ∀ (m n p : ℕ) → m < n → m + p < n + p
+-monoˡ-< m n p m<n rewrite +-comm m p | +-comm n p = +-monoʳ-< p m n m<n

+-mono-< : ∀ (m n p q : ℕ) → m < n → p < q → m + p < n + q
+-mono-< m n p q m<n p<q = <-trans (+-monoˡ-< m n p m<n) (+-monoʳ-< n p q p<q)

≤-implies-< : ∀ {m n : ℕ} → suc m ≤ n → m < n
≤-implies-< (s≤s z≤n) = z<n
≤-implies-< (s≤s (s≤s n)) = s<s (≤-implies-< (s≤s n))

<-implies-≤ : ∀ {m n : ℕ} → m < n → suc m ≤ n
<-implies-≤ z<n = s≤s z≤n
<-implies-≤ (s<s n) = s≤s (<-implies-≤ n)

-- Even and odd --
-- we are using overload constructor here. Constructor names can conflict with
-- different types.
data even : ℕ → Set
data odd : ℕ → Set

data even where
  zero : even zero
  suc : ∀ {n : ℕ} → odd n → even (suc n)

data odd where
  suc : ∀ {n : ℕ} → even n → odd (suc n)

-- corecursive
o+e≡o : ∀ {m n : ℕ} → odd m → even n → odd (m + n)
e+e≡e : ∀ {m n : ℕ} → even m → even n → even (m + n)

e+e≡e zero n = n
e+e≡e (suc m) n = suc (o+e≡o m n)

o+e≡o (suc m) n = suc (e+e≡e m n)

o+o≡e : ∀ {m n : ℕ} → odd m → odd n → even (m + n)
o+o≡e .{suc m} {n} (suc {m} em) on rewrite +-comm m n = suc (o+e≡o on em)

data Bin : Set where
  ⟨⟩ : Bin
  _I : Bin → Bin
  _O : Bin → Bin

inc : Bin → Bin
inc (⟨⟩) = ⟨⟩ I
inc (n O) = n I
inc (n I) = (inc n) O
