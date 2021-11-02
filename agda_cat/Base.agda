{-# OPTIONS --rewriting #-}

import Agda.Primitive
module Base where

open Agda.Primitive

-- heterogeneous  john major equality.
data _≡_ {l : _} {A : Set l} (a : A) : {B : Set l} → B → Set l where
  refl : a ≡ a

-- restrict to homogenous case.
_≡!_ : {l : _} {A : Set l} → A → A → Set l
a ≡! b = a ≡ b


-- martin lof equality for rewriting relation.
data _⇝_ {l : _} {A : Set l} (a : A) : A → Set l where
  refl : a ⇝ a
{-# BUILTIN EQUALITY _⇝_ #-}

-- convert a homongenous JMEq to a rewriting equality
→rewrite : {l : _} {A : Set l} {x y : A} → x ≡! y → x ⇝ y
→rewrite refl = refl

transport : {l : _} {A B : Set l} → A ≡! B → A → B
transport refl x = x

transportConstant : {l : _} {A B : Set l} (p : A ≡! B) (x : A)
                  → x ≡ transport p x
transportConstant refl x = refl

record ∑ {l l'} (A : Set l) (B : A → Set l') : Set (l ⊔ l') where
  constructor _,_
  field
    fst : A
    snd : B fst

record _×_ {l l'} (A : Set l) (B : Set l') : Set (l ⊔ l') where
  constructor _,_
  field
    fst : A
    snd : B

infixr 5 _,_
infixr 1 _≡_ _⇝_ _≡!_

×Path : {l l' : _} {A : Set l} {A' : Set l} {B : Set l'} {B' : Set l'} {x : A × B} {y : A' × B'}
      → _×_.fst x ≡  _×_.fst y
      → _×_.snd x ≡  _×_.snd y
      → x ≡ y
×Path {x = .(_×_.fst y), .(_×_.snd y)} {y = y} refl refl = refl

{-# BUILTIN REWRITE _⇝_ #-}

lhs : {l : _} {A B : Set l} {x : A} {y : B} → x ≡ y → A
lhs {x = x} p = x

rhs : {l : _} {A B : Set l} {x : A} {y : B} → x ≡ y → B
rhs {y = y} p = y

sym : {l : _} {A B : Set l} {x : A} {y : B} → x ≡ y → y ≡ x
sym refl = refl

sym! : {l : _} {A : Set l} {x y : A} → x ⇝ y → y ⇝ x
sym! refl = refl

_∘p_ : {l : _} {A : Set l} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
refl ∘p refl = refl

infixr 5 _∘p_

_∘!_ : {l : _} {A : Set l} {x y z : A} → x ⇝ y → y ⇝ x → x ⇝ y
refl ∘! refl = refl

infix 1 begin_
infixr 2 _≡⟨⟩_ _≡⟨_⟩_ _≡⟨⟩!_ _≡⟨_⟩!_
infix 3 _∎

begin_ : {l : _} {A : Set l} {x y : A} → x ≡ y → x ≡ y
begin x≡y = x≡y

_≡⟨⟩_ : {l : _} {A B : Set l} (x : A) {y : B} → x ≡ y → x ≡ y
x ≡⟨⟩ x≡y = x≡y

_≡⟨⟩!_ : {l : _} {A : Set l} (x : A) {y : A} → x ≡ y → x ≡ y
x ≡⟨⟩! x≡y = x≡y

_≡⟨_⟩_ : {l : _} {A B C : Set l} (x : A) {y : B} {z : C} → x ≡ y → y ≡ z → x ≡ z
x ≡⟨ refl ⟩ refl = refl

_≡⟨_⟩!_ : {l : _} {A : Set l} (x : A) {y : A} {z : A} → x ≡ y → y ≡ z → x ≡ z
x ≡⟨ refl ⟩! refl = refl

_∎ : {l : _} {A : Set l} (x : A) → x ≡ x
x ∎ = refl

ap : {l : _} {l' : _} {A : Set l} {B : Set l'} (f : A → B) {x y : A}
   → x ≡ y
   → f x ≡ f y
ap f refl = refl

ap₂ : {l l₁ l₂ : _}
      {A : Set l} {B : Set l₁} {C : Set l₂} (f : A → B → C)
      {w x : A}
      {y z : B}
    → w ≡ x → y ≡ z → f w y ≡ f x z
ap₂ f refl refl = refl

hap : {l l' : _} {A : Set l} {B : A → Set l'} (f : (x : A) → B x) {x y : A}
    → x ≡ y
    → f x ≡ f y
hap f refl = refl

happly : {l l' : _} {A : Set l} {B : A → Set l'} {f g : (x : A) → B x}
         (p : f ≡ g) (x : A)
       → f x ≡ g x
happly refl x = refl

subst : {l l' : _} {A : Set l} (P : A → Set l') {x y : A} → x ≡ y → P x → P y
subst P refl x = x

substConst : {l l' : _} {A : Set l} {P : A → Set l'} {x y : A} {p : x ≡ y}
             {e : P x}
           →  subst P p e ≡ e
substConst {p = refl} = refl

UIP : {l : _} {A : Set l} {B : Set l} {x : A} {y : B} (p q : x ≡ y) → p ≡ q
UIP refl refl = refl

UIP! : {l : _} {A : Set l} {x y : A} (p q : x ⇝ y) → p ≡ q
UIP! refl refl = refl

-- extensional equivalence.
postulate
  fun-ext : {l l' : _} {A : Set l} {B : A → Set l'}
          → {f g : (x : A) → B x}
          → ((x : A) → f x ≡ g x)
          → f ≡ g
  fun-ext-refl₁ : {l l' : _} {A : Set l} {B : A → Set l'} {f : (x : A) → B x}
                  {p : (x : A) → f x ≡ f x}
                → fun-ext {l} {l'} {A} {B} {f} {f} p ⇝ refl
  {-# REWRITE fun-ext-refl₁ #-}

fun-ext-invis : {l l' : _} {A : Set l} {B : A → Set l'}
              → {f g : {x : A} → B x}
              → ((x : A) → f {x} ≡ g {x})
              → _≡_ {l ⊔ l'} {A = {x : A} → B x} f {B = {x : A} → B x} g
fun-ext-invis {l} {l'} {A} {B} {f} {g} p = ap fixup (fun-ext p) where
  fixup : ((x : A) → B x) → {x : A} → B x
  fixup f {x} = f x

∑-Path : {l l' : _} {A : Set l} {B : A → Set l'} {x y : ∑ A B}
       → (p : ∑.fst x ≡ ∑.fst y)
       → ∑.snd x ≡ ∑.snd y
       → x ≡ y
∑-Path refl refl = refl

∑-Path! : {l l' : _} {A : Set l} {B : A → Set l'} {x y : ∑ A B}
        → (p : ∑.fst x ≡! ∑.fst y)
        → subst B p (∑.snd x) ≡! ∑.snd y
        → x ≡! y
∑-Path! refl refl = refl

record isConst {l : _} {A : Set l} : Set l where
  constructor contract
  field
    center : A
    paths : (y : A) → center ≡! y

data Bot {l : _} : Set l where

absurd : {l' l : _} {A : Set l'} → Bot {l} → A
absurd ()

record Top {l : _} : Set l where
  constructor tt

data Bool {l : _} : Set l where
  tt ff : Bool

data _∐_ {l l' : _} (A : Set l) (B : Set l') : Set (l ⊔ l') where
  left : A → A ∐ B
  right : B → A ∐ B

record Lift {l l' : _} (A : Set l) : Set (l ⊔ l') where
  constructor lift
  field
    unlift : A
