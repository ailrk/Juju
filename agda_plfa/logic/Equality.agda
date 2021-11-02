module agda_plfa.logic.Equality where

-- simple intensional propositional equality
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

infix 4 _≡_

-- equality is Equivalence relation. --
-- we don't need to prove reflexivity, because ... it's alreay
-- there.

sym : ∀ {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

trans : ∀ {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

-- congruence and substitution --
cong : ∀ {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

cong2 : ∀ {A B C : Set} {f : A → B → C} {u x : A} {v y : B}
  → u ≡ x
  → v ≡ y
  → f u v ≡ f x y
cong2 refl refl = refl

-- equality congruence in function application
cong-app : ∀ {A B : Set} {f g : A → B}
  → f ≡ g
  → ∀ (x : A)
  → f x ≡ g x
cong-app refl x = refl

-- equality satisfies substittion
-- P is a predicate.
-- if x y are equal, then P holds for both
-- x and y.
subst : ∀ {A : Set} {x y : A} (P : A → Set)
  → x ≡ y
  → P x → P y
subst P refl px = px


-- Chains of equations --
-- we are building the proof framework..
module ≡-Reasoning {A : Set} where

  infix 1 begin_
  infixr 2 _≡⟨⟩_ _≡⟨_⟩_
  infix 3 _∎

  -- dummy id specializef for ≡
  begin_ : ∀ {x y : A}
    → x ≡ y
    → x ≡ y
  begin x≡y = x≡y

  -- x => y
  _≡⟨⟩_ : ∀ (x : A) {y : A}
    → x ≡ y
    → x ≡ y
  x ≡⟨⟩ x≡y = x≡y

  --   x=y
  -- x ---> y=z : x=z
  _≡⟨_⟩_ : ∀ (x : A) {y z : A}
    → x ≡ y
    → y ≡ z
    → x ≡ z
  x ≡⟨ x≡y ⟩ y≡z = trans x≡y y≡z

  _∎ : ∀ (x : A) → x ≡ x
  x ∎ = refl

open ≡-Reasoning  -- open the module

trans' : ∀ {A : Set} → {x y z : A}
  → x ≡ y → y ≡ z → x ≡ z
trans' {A} {x} {y} {z} x≡y y≡z =
  begin x ≡⟨ x≡y ⟩ y ≡⟨ y≡z ⟩ z ∎

-- the fixity:
-- begin (x ≡⟨ x≡y ⟩ (y ≡⟨ y≡z ⟩ (z ∎)))

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero + n = n
(suc m) + n = suc (m + n)

-- claim without proof. This is very bad...
-- use it unless you're sure these are correct lemmas.
postulate
  +-identity : ∀ (m : ℕ) → m + zero ≡ m
  +-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)

+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero =
  begin
    m + zero
  ≡⟨ +-identity m ⟩
    m
  ≡⟨⟩
    zero + m
  ∎

+-comm m (suc n) =
  begin
    m + suc n
  ≡⟨ +-suc m n ⟩
    suc (m + n)
  ≡⟨ cong suc (+-comm m n) ⟩
    suc (n + m)
  ≡⟨⟩
    suc n + m
  ∎

-- rewriting --
data even : ℕ → Set
data odd : ℕ → Set

data even where
  even-zero : even zero
  even-suc : ∀ {n : ℕ} → odd n → even (suc n)

data odd where
  odd-suc : ∀ (n : ℕ) → even n → odd (suc n)

-- we already proved addition is commutative. Also we know
-- we can have even (m + n). Thus we should be able to reuse
-- the +-comm lemma to prove commutativity for even number additions.
-- in agda you do this with rewrite.

{-# BUILTIN EQUALITY _≡_ #-}

even-comm : ∀ (m n : ℕ)
  → even (m + n)
  → even (n + m)
even-comm m n ev rewrite +-comm n m = ev

-- mutiple rewrites --

+-comm' : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm' zero n rewrite +-identity n = refl
+-comm' (suc m) n rewrite +-suc n m | +-comm' m n = refl


-- rewrite expanded
-- rewrite is just a shorthand for with.
-- this actually is equivalent with the proof above.
even-comm' : ∀ (m n : ℕ)
  → even (m + n)
  → even (n + m)
even-comm' m n ev with m + n | +-comm m n
... | .(n + m) | refl  = ev

-- with is more complicated than pattern macthing in most MLs.
-- you need to pattern match on indexed type too.
-- first column here assert m n has commutativity.
-- def: dot pattern: (inaccessable pattern) can be used when the only
--      type-correct value of the argument is determined by the pattern
--      given for the other argument
--

-- unification:
--  mechanism behind polymorphic type inference and pattern matching.
--  def : process of finding a substition that makes two given terms equal.
--  pattern matching: apply unification on expression
--  type inference:   apply unification on type expression
--
-- essence:
--  find a substitution S that unifies two given terms (make them equal).
--  thus, given s and t, we want to find S such that s S = t S
--  such S is called a unifier for s and t
--  e.g:
--    two terms:
--      f x (g y)   f (g z) w
--    with substitution
--      S = [x <- g z, w <- g y]
--    would be a unifier.
--    because
--        f x (g y) [x <- g z, w <-  g y]
--      = f (g z) w
--      = f (g z) (g y).
--      https://cs.stackexchange.com/questions/4650/unification-vs-sat-solver
--
-- 1. a unifier doesn't necessarily exists.
-- 2. if unifier exists, it doesn't necessarily unique.
-- 3. there exists a most general unifier (mgu) which is unique.
-- 4. unifiers that are not mgu are called refinement of the mgu.

-- use substitution instead
even-comm'' : ∀ (m n : ℕ)
  → even (m + n)
  → even (n + m)
even-comm'' m n = subst even (+-comm m n)

-- we were using martin lof's form of equality, but there is a eailer, more naive
-- notino of equality

-- leibniz equality --
--  def: two objects are equal iff they satisfy the same properties
--    relevent: spock's law "a difference that make no difference is no difference"

-- Two terms satisfy leibniz equality iff they satisfy martin lof equalify.

-- define leibniz equlity x ≐ y if exists property P that P x and P y
-- P: predicates that takes A and return a Set
-- so the result of x ≐ y is actually a lambda that takes a P : A → Set,
-- a P x, and gives you a P y.
_≐_ : ∀ {A : Set} (x y : A) → Set₁  -- using levels
_≐_ {A} x y = ∀ (P : A → Set) → P x → P y

-- Levels like Set₁ forms a hierachy oftypes.
-- This way we don't need to assign Set to Set itself.
-- To avoid Russell's paradox and girard's paradox.

refl-≐ : ∀ {A : Set} {x : A} → x ≐ x
refl-≐ P Px = Px

trans-≐ : ∀ {A : Set} {x y z : A}
  → x ≐ y
  → y ≐ z
  → x ≐ z
trans-≐ x≐y y≐z P Px = y≐z P (x≐y P Px)
