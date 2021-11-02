{-# OPTIONS --rewriting #-}

open import Base
open import Agda.Primitive

module Category where

{- category: proof releveant preorder (A, ≤) -}
record Category (o h : Level) : Set (lsuc (o ⊔ h)) where

  -- definition of a category
  field
    Ob : Set o
    Hom : Ob → Ob → Set h

  -- exists id and compositions.
  field
    id : {x : _} → Hom x x                               -- reflexivity
    _∘_ : {x y z : _} → Hom y z → Hom x y → Hom x z      -- transitivity

  infixr 40 _∘_

  -- identity laws
  field
    idr : {x y : _} (f : Hom x y) → f ∘ id ≡! f
    idl : {x y : _} (f : Hom x y) → id ∘ f ≡! f

  -- associativity laws
  field
    assoc₁ : {w x y z : _} (f : Hom y z) (g : Hom x y) (h :Hom w x)
           → (f ∘ g) ∘ h ≡! f ∘ (g ∘ h)
    assoc₂ : {w x y z : _} (f : Hom y z) (g : Hom x y) (h : Hom w x)
           → f ∘ (g ∘ h) ≡! (f ∘ g) ∘ h

-- once we can give an instance of this Cat type, we know it forms a category.


{- Opposite category: -}

infixl 60 _^op

_^op : {o₁ h₁ : _} → Category o₁ h₁ → Category o₁ h₁
Category.Ob (C ^op) = Category.Ob C
Category.Hom (C ^op) x y = Category.Hom C y x
Category.id (C ^op) = Category.id C
Category._∘_ (C ^op) f g = Category._∘_ C g f

Category.idr (C ^op) = Category.idl C
Category.idl (C ^op) = Category.idr C

-- dual: f ∘ (g ∘ h)  -> (h ∘ g) ∘ f
--       (f ∘ g) ∘ h ->  h ∘ (g ∘ f)
Category.assoc₁ (C ^op) = λ f g h → Category.assoc₁ h g f
Category.assoc₂ (C ^op) = λ f g h → Category.assoc₂ h g f

-- op of op of cat is cat
_ : {o₁ h₁ :} {C : Category o₁ h₁} → (C ^op) ^op ≡ c
_ = refl


{- Cat of sets -}
Sets : (o : _) → Category (lsuc o) o
Category.Ob (Sets o) = Set o
Category.Hom (Sets o) A B = A → B



