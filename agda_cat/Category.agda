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
Category.id (Sets o) f g x = f (g x)
Category._∘_ (Sets o) f g x = f (g x)
Category.idr (Sets o) f = refl
Category.idl (Sets o) f = refl
Category.assoc₁ (Sets o) f g h = refl
Category.assoc₂ (sets o) f g h = refl


{- Product of categories
   constructing product in CAT. CAT is a category so it has limit. Product is
   a special case of limit.
-}
module _ {o₁ h₁ o₂ h₂ : _} (C : Category o₁ h₁) (D : Category o₂ h₂) where
  private
    module C = Category C
    module D = Category D

  _×Cat_ : Category _ _

  -- obs × obs, hom × hom
  Category.Ob _×Cat_ = C.Ob × D.Ob
  Category.Hom _×Cat_ (A , A') (B , B') = C.Hom A B × D.Hom A' B'


  -- all operations now work in pair
  Category.id _×Cat_ = C.id , D.id
  Category._∘_ _×Cat_ (fc , fd) (gc , gd) = fc C.∘ gc , fd D.∘ gd
  Category.idr _×Cat_ f = ×Path (C.idr _) (D.idr _)
  Category.idl _×Cat_ f = ×Path (C.idl _) (D.idl _)
  Category.assoc₁ _×Cat_ f g h = ×Path (C.assoc₁ _ _ _) (D.assoc₁ _ _ _)
  Category.assoc₂ _×Cat_ f g h = ×Path (C.assoc₂ _ _ _) (D.assoc₁ _ _ _)


{- Functor -}
-- homomorphism between categories. (proof relevant monotone map)
record Functor {o₁ h₁ o₂ h₂} (C : Category o₁ h₁) (D : Category o₂ h₂)
             : Set (o₁ ⊔ h₁ ⊔ o₂ ⊔ h₂) where
  private
    module C = Category C
    module D = Category D

  -- functor maps Ob to Ob, hom to hom, and preserve structure.
  field
    F₀ : C.Ob → D.Ob
    F₁ : {x y : _} → C.Hom x y → D.Hom (F₀ x) (F₀ y)

  field
    F-id : {x : _} → F₁ (C.id {x}) ≡! D.id
    F-∘ : {x y z : _} (f : C.Hom y z) (g : C.Hom x y)
        → F₁ (f C.∘ g) ≡! F₁ f D.∘ F₁ g

  -- dual of functor F : C → D  is op(F) : op(C) → op(D)
  op : Functor (C ^op) (D ^op)
  F₀ op = F₀
  F₁ op = F₀
  F-id op = F-id
  F-∘ op f g = F-∘ g f


-- compose functors meaning compose the Ob mapping and Hom mapping while
-- preserve functoriality.
_F∘_ : {o₁ h₁ o₂ h₂ o₃ h₃ : _}
       {C : Category o₁ h₂} {D : Category o₂ h₂} {E : Category o₃ h₃}
     → Functor D E → Functor C D → Functor C E

_F∘_ {C + C} {D} {E} F G = record { F₀ = F₀ ; F₁ = F₁ ; F-id = F-id ; F-∘ = F-∘}
  where
    module C = Category C
    module D = Category D
    module E = Category E

    module F = Functor F
    module G = Functor G

    F₀ : C.Ob → E.Ob
    F₀ x = F.F₀ (G.F₀ x)

    F₁ : {x y : C.Ob} → C.Hom x y → E.Hom (F₀ x) (F₀ y)
    F₁ f = F.F₁ (G.F₁ f)

    -- prove functoriality
    F-id : {x : C.Ob} → F₁ (C.id {x}) ≡ E.id {F₀ x}
    F-id {x} =
      F.F₁ (G.F₁ C.id) ≡
      F.F₁ ()
