module agda_plfa.logic.Relations where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Nat.Properties using (+-comm; +-identityʳ; *-comm; *-identityˡ)

-- Relations --
--  note: {} means n is implicit, which gives us implicit types.
data _≤_ : ℕ → ℕ → Set where
    z≤n : ∀ {n : ℕ} → zero ≤ n
    s≤s : ∀ {m n : ℕ} → m ≤ n → suc m ≤ suc n

infix 4 _≤_

-- definition:
--      base case: ∀ n ∈ ℕ, zero ≤ n holds
--                 (z≤n gives he evidence that it holds)
--      inductive case: ∀ m, n ∈ ℕ, m ≤ n → suc m  ≤ suc n
--                      (s≤s takes evidence that m ≤ n holds
--                       and then gives evidence that suc m  ≤ suc n holds)

-- Implicit arguments --
-- implict arguments don't need to be written explicitly. They are
-- inferred by the typechecker.
_ : 2 ≤ 4
_ = s≤s (s≤s z≤n)   -- 0 ≤ ? → 0 ≤ 2 → 1 ≤ 3 → 2 ≤ 4

-- write the same proof explicitly
_ : 2 ≤ 4
_ = s≤s {1} {3} (s≤s {0} {2} (z≤n {2}))

-- more explicitly
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

-- # Properties of ordering relations #
--     reflexive:       ∀ n. the relation nRn holds
--     transitive:      ∀ m, n. p, (mRn ∧ nRp → mRp)
--     anti-symmetric:  ∀ m, n. mRn ∧ nRm → m ≡ n
--     total:           ∀ m, n. mRn ∨ nRm
-- ! some name for combination of common properties
--     preorder: reflexive and transitive
--     partial order: any preorder that is also anti-symmetric.
--     total order: any parital order that is also total.
--
-- To be preorder you don't need to be an equivalence relation.
--      preorder + | symmetric -> equivalence relation
--                 | antisymmetric -> partial order

-- exercise orderings
-- An example of a preorder that is not a partial order
--    reflexive and transitive relation but not anti-symmetric.
--    tons.
--    e.g
--    1. + (mod n)
--       not anti-symmetric because a + b (mod n) and b + a (mod n)
--       doesn't mean a = b. at all
--    2. The reachability relationship in any directed graph.
--       a can reach b but it's not necessarily for b to go back.
--    3. {(a, a), (a, b), (b, a), (b, b)} as preorder on {a, b}
-- An example of a partial order that is not a total order
--    e.g ⊆
--    It's partial order becauase  if a ⊆ b and b ⊆ a a = b
--    But not all sets are subset of each other. they can be disjoint.

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
-- first let's define what does it mean for a relation to be total
-- if ≤ is total, either m ≤ n or n ≤ m or both.
-- if we can get a instance of the data type defined below, we proved
-- it exists.
data Total (m n : ℕ) : Set where
  forward : m ≤ n → Total m n
  flipped : n ≤ m → Total m n

-- note the above defintion create a datatype with parameter. It's the
-- same as the following indexed type:

data Total' : ℕ → ℕ → Set where
  forward' : ∀ {m n : ℕ} → m ≤ n → Total' m n
  flipped' : ∀ {m n : ℕ} → n ≤ m → Total' m n

-- it's saying, give me any m and n, I can construct either m≤n
-- with is the pattern matching in agda.
-- zero cases are trivial
-- for both arguments are suc, if m n are total, (suc m) (suc n)
-- are total too.
-- It's still a recursive call on ≤-total with m n, which if m n
-- themselves are suc the same branch will be invoke again, until
-- hit one of two base cases.
≤-total : ∀ (m n : ℕ) → Total m n
≤-total zero n = forward z≤n
≤-total (suc m) zero = flipped z≤n
≤-total (suc m) (suc n) with ≤-total m n
... | forward m≤n = forward (s≤s m≤n)
... | flipped n≤m = flipped (s≤s n≤m)


-- equivalent to this.
-- just define a helper for pattern matching.
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
-- base case is trivial. works in a ring everything times 0 is 0.
-- inductive step. We need to somehow express multiplication in ordering even it's
-- constructor is defined with suc only.
-- we reuse monotonic + to generate the ≤ relation.
-- p + (n * p) ≤ q + (n * q) => n * p ≤ n * q ?
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
-- as a data type and construct an instance of it!
data Trichotomous (m n : ℕ) : Set where
  tri< : m < n → Trichotomous m n
  tri≡ : m ≡ n → Trichotomous m n
  tri> : m > n → Trichotomous m n

-- another interpretation of the proving mechanism is:
--  we try to construct a instance of Trichotomous, to do that
--  we recursively call <-trichotomy m n.
--  if it terminates, which it does, we get a instance of the smaller
--  proof. Then we use that to construct the actual proof (the instance)
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
