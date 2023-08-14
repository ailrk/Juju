module logic.Equality where

-- equality has a lot of subtlties with it. How can we tell two things are
-- the same?


-- simple intensional propositional equality
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

infix 4 _≡_

sym : ∀ {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

trans : ∀ {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

cong : ∀ {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

cong2 : ∀ {A B C : Set} {f : A → B → C} {u x : A} {v y : B}
  → u ≡ x
  → v ≡ y
  → f u v ≡ f x y
cong2 refl refl = refl

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

{-# BUILTIN EQUALITY _≡_ #-}

even-comm : ∀ (m n : ℕ)
  → even (m + n)
  → even (n + m)
even-comm m n ev rewrite +-comm n m = ev

-- mutiple rewrites --

+-comm' : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm' zero n rewrite +-identity n = refl
+-comm' (suc m) n rewrite +-suc n m | +-comm' m n = refl


even-comm' : ∀ (m n : ℕ)
  → even (m + n)
  → even (n + m)
even-comm' m n ev with m + n | +-comm m n
... | .(n + m) | refl  = ev

-- use substitution instead
even-comm'' : ∀ (m n : ℕ)
  → even (m + n)
  → even (n + m)
even-comm'' m n = subst even (+-comm m n)

_≐_ : ∀ {A : Set} (x y : A) → Set₁  -- using levels
_≐_ {A} x y = ∀ (P : A → Set) → P x → P y

refl-≐ : ∀ {A : Set} {x : A} → x ≐ x
refl-≐ P Px = Px

trans-≐ : ∀ {A : Set} {x y z : A}
  → x ≐ y
  → y ≐ z
  → x ≐ z
trans-≐ x≐y y≐z P Px = y≐z P (x≐y P Px)
