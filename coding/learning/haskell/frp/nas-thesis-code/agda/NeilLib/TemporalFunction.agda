{-# OPTIONS --type-in-type  #-}

open import NeilPrelude

module TemporalFunction

  (Time    : Set)
  (_<'_    : Rel Time)
  (transit : Transitive _<'_)
  (trich   : Trichotomous _<'_)

  -- (Δ : Set)
  -- (_+_ : Time → Δ → Time)
  -- (lem-increasing : {t t' : Time} → {ε : Δ} → ((t' + ε) <' t) ⊎ ((t' + ε) ≡ t) → t' <' t)

where

---------------------------------------------------------

import TemporalLogic renaming (_B_ to _`Back-To`_)
open TemporalLogic Time _<'_ transit trich

---------------------------------------------------------

TVal : Set → Set
TVal A = Time → A

TFun : Set → Set → Set
TFun A B = TVal A → TVal B

---------------------------------------------------------

-- time varying equality

infix 3 _≐_

_≐_ : {A : Set} → TVal A → TVal A → TPred
(s₁ ≐ s₂) t = s₁ t ≡ s₂ t

-- EqualTo : {A : Set} → TVal A → A → TPred
-- EqualTo s a t = s t ≡ a

-- EqualAt : {A : Set} → TVal A → Time → TPred
-- EqualAt s t = EqualTo s (s t)

ConstantVal : {A : Set} → A → TVal A → Interval → Set
ConstantVal a s = Over (λ t → s t ≡ a)

ConstantOver : {A : Set} → TVal A → Interval → Set
ConstantOver s i = (t₁ t₂ : Time) → t₁ ∈ i → t₂ ∈ i → s t₁ ≡ s t₂

lem-AlwaysEq : {A : Set} → (s₁ s₂ : TVal A) → Always (H (s₁ ≐ s₂) ⇒ (s₁ ≐ s₂) ⇒ G (s₁ ≐ s₂) ⇒ (λ _ → Always (s₁ ≐ s₂)))
lem-AlwaysEq s₁ s₂ t Heq eq Geq t' with compare t t'
lem-AlwaysEq s₁ s₂ t Heq eq Geq .t | refl   = eq
lem-AlwaysEq s₁ s₂ t Heq eq Geq t' | less p = Geq t' p
lem-AlwaysEq s₁ s₂ t Heq eq Geq t' | more p = Heq t' p

---------------------------------------------------------

lem-eq-strict : {A : Set} → {t₀ t₁ : Time} → (s : TVal A) → (t₀ < t₁ → s t₀ ≡ s t₁) → (t₀ ≤ t₁ → s t₀ ≡ s t₁)
lem-eq-strict s p (inr refl) = refl
lem-eq-strict s p (inl lt) = p lt

---------------------------------------------------------

-- Properties of Temporal Functions

-- Contractive : {A B : Set} → TFun A B → Set
-- Contractive {A} f = ? -- (s₁ s₂ : TVal A) → Σ Δ (λ ε → (t : Time) → ((t' : Time) → ((t' + ε) ≤ t) → s₁ t' ≡ s₂ t') → f s₁ t ≡ f s₂ t)

WeaklyContractive : {A B : Set} → TFun A B → Set
WeaklyContractive {A} f = (s₁ s₂ : TVal A) → Always (H (s₁ ≐ s₂) ⇒ f s₁ ≐ f s₂)

NonExpansive : {A B : Set} → TFun A B → Set
NonExpansive {A} f = (s₁ s₂ : TVal A) → Always (Hʳ (s₁ ≐ s₂) ⇒ f s₁ ≐ f s₂)

---------------------------------------------------------

lem-ne-H-cong : {A B : Set} → {f : TFun A B} → NonExpansive f → (s₁ s₂ : TVal A) → Always (H (s₁ ≐ s₂) ⇒ H (f s₁ ≐ f s₂))
lem-ne-H-cong ne s₁ s₂ t Heq t' lt = ne s₁ s₂ t' (reduce-range-<-incl lt Heq)

lem-ne-Hʳ-cong : {A B : Set} → {f : TFun A B} → NonExpansive f → (s₁ s₂ : TVal A) → Always (Hʳ (s₁ ≐ s₂) ⇒ Hʳ (f s₁ ≐ f s₂))
lem-ne-Hʳ-cong ne s₁ s₂ t Heq = ne s₁ s₂ t Heq , lem-ne-H-cong ne s₁ s₂ t (snd Heq)

---------------------------------------------------------

lem-Hʳ-×-cong : {A B : Set} → (a₁ a₂ : TVal A) → (b₁ b₂ : TVal B) → Always (Hʳ (a₁ ≐ a₂) ⇒ Hʳ (b₁ ≐ b₂) ⇒ Hʳ ((a₁ & b₁) ≐ (a₂ & b₂)))
lem-Hʳ-×-cong a₁ a₂ b₁ b₂ t = ×-map2 ×-cong ×-cong3

lem-Hʳ-×-congL : {A B : Set} →  (a : TVal A) → (b₁ b₂ : TVal B) → Always (Hʳ (b₁ ≐ b₂) ⇒ Hʳ ((a & b₁) ≐ (a & b₂)))
lem-Hʳ-×-congL a b₁ b₂ t = ×-congL ∥ ×-cong3L

lem-Hʳ-×-congR : {A B : Set} →  (a₁ a₂ : TVal A) → (b : TVal B) → Always (Hʳ (a₁ ≐ a₂) ⇒ Hʳ ((a₁ & b) ≐ (a₂ & b)))
lem-Hʳ-×-congR a₁ a₂ b t = ×-congR ∥ ×-cong3R

---------------------------------------------------------

lem-ne-×-cong : {A B C : Set} → {f : TFun (A × B) C} → NonExpansive f → (a₁ a₂ : TVal A) → (b₁ b₂ : TVal B) 
                → Always (Hʳ (a₁ ≐ a₂) ⇒ Hʳ (b₁ ≐ b₂) ⇒ f (a₁ & b₁) ≐ f (a₂ & b₂))
lem-ne-×-cong ne a₁ a₂ b₁ b₂ t (eqa , Heqa) (eqb , Heqb) = ne (a₁ & b₁) (a₂ & b₂) t (×-cong eqa eqb , ×-cong3 Heqa Heqb)

lem-ne-×-congL : {A B C : Set} → {f : TFun (A × B) C} → NonExpansive f → (a : TVal A) → (b₁ b₂ : TVal B) 
                 → Always (Hʳ (b₁ ≐ b₂) ⇒ f (a & b₁) ≐ f (a & b₂))
lem-ne-×-congL ne a b₁ b₂ t Heqb = lem-ne-×-cong ne a a b₁ b₂ t (refl , λ _ _ → refl) Heqb

lem-ne-×-congR : {A B C : Set} → {f : TFun (A × B) C} → NonExpansive f → (a₁ a₂ : TVal A) → (b : TVal B)
                 → Always (Hʳ (a₁ ≐ a₂) ⇒ f (a₁ & b) ≐ f (a₂ & b))
lem-ne-×-congR ne a₁ a₂ b t Heqa = lem-ne-×-cong ne a₁ a₂ b b t Heqa (refl , λ _ _ → refl)

---------------------------------------------------------

lem-ne-×-cong-Hʳ : {A B C : Set} → {f : TFun (A × B) C} → NonExpansive f → (a₁ a₂ : TVal A) → (b₁ b₂ : TVal B) 
                   → Always (Hʳ (a₁ ≐ a₂) ⇒ Hʳ (b₁ ≐ b₂) ⇒ Hʳ (f (a₁ & b₁) ≐ f (a₂ & b₂)))
lem-ne-×-cong-Hʳ ne a₁ a₂ b₁ b₂ t Heqa Heqb = lem-ne-Hʳ-cong ne (a₁ & b₁) (a₂ & b₂) t (lem-Hʳ-×-cong a₁ a₂ b₁ b₂ t Heqa Heqb)

lem-ne-×-congL-Hʳ : {A B C : Set} → {f : TFun (A × B) C} → NonExpansive f → (a : TVal A) → (b₁ b₂ : TVal B) 
                   → Always (Hʳ (b₁ ≐ b₂) ⇒ Hʳ (f (a & b₁) ≐ f (a & b₂)))
lem-ne-×-congL-Hʳ ne a b₁ b₂ t Heqb = lem-ne-Hʳ-cong ne (a & b₁) (a & b₂) t (lem-Hʳ-×-congL a b₁ b₂ t Heqb)

lem-ne-×-congR-Hʳ : {A B C : Set} → {f : TFun (A × B) C} → NonExpansive f → (a₁ a₂ : TVal A) → (b : TVal B)
                   → Always (Hʳ (a₁ ≐ a₂) ⇒ Hʳ (f (a₁ & b) ≐ f (a₂ & b)))
lem-ne-×-congR-Hʳ ne a₁ a₂ b t Heqb = lem-ne-Hʳ-cong ne (a₁ & b) (a₂ & b) t (lem-Hʳ-×-congR a₁ a₂ b t Heqb)

---------------------------------------------------------

lem-ne-×-congL-congR : {A B C D : Set} → {f : TFun (A × C) D} → {g : TFun B C} → NonExpansive f → NonExpansive g → (a : TVal A) → (b₁ b₂ : TVal B) 
                       → Always (Hʳ (b₁ ≐ b₂) ⇒ f (a & g b₁) ≐ f (a & g b₂))
lem-ne-×-congL-congR nef neg a b₁ b₂ t Heq = lem-ne-×-congL nef a _ _ t (lem-ne-Hʳ-cong neg b₁ b₂ t Heq)

---------------------------------------------------------

lem-wcon→ne : {A B : Set} → {f : TVal A → TVal B} → WeaklyContractive f → NonExpansive f
lem-wcon→ne = result3' (argument snd)

lem-wcon-H-cong : {A B : Set} → {f : TFun A B} → WeaklyContractive f → (s₁ s₂ : TVal A) → Always (H (s₁ ≐ s₂) ⇒ H (f s₁ ≐ f s₂))
lem-wcon-H-cong con s₁ s₂ t Heq t' lt = con s₁ s₂ t' (reduce-range-< lt Heq)

lem-wcon-Hʳ-cong : {A B : Set} → {f : TFun A B} → WeaklyContractive f → (s₁ s₂ : TVal A) → Always (H (s₁ ≐ s₂) ⇒ Hʳ (f s₁ ≐ f s₂))
lem-wcon-Hʳ-cong con s₁ s₂ t Heq = con s₁ s₂ t Heq , lem-wcon-H-cong con s₁ s₂ t Heq

---------------------------------------------------------

lem-wcon-×-cong : {A B C : Set} → {f : TFun (A × B) C} → WeaklyContractive f → (a₁ a₂ : TVal A) → (b₁ b₂ : TVal B) 
                 → Always (H (a₁ ≐ a₂) ⇒ H (b₁ ≐ b₂) ⇒ f (a₁ & b₁) ≐ f (a₂ & b₂))
lem-wcon-×-cong con a₁ a₂ b₁ b₂ t Heqa Heqb = con (a₁ & b₁) (a₂ & b₂) t (×-cong3 Heqa Heqb)

lem-wcon-×-congL : {A B C : Set} → {f : TFun (A × B) C} → WeaklyContractive f → (a : TVal A) → (b₁ b₂ : TVal B) 
                  → Always (H (b₁ ≐ b₂) ⇒ f (a & b₁) ≐ f (a & b₂))
lem-wcon-×-congL con a b₁ b₂ t = lem-wcon-×-cong con a a b₁ b₂ t (λ _ _ → refl)

lem-wcon-×-congR : {A B C : Set} → {f : TFun (A × B) C} → WeaklyContractive f → (a₁ a₂ : TVal A) → (b : TVal B)
                 → Always (H (a₁ ≐ a₂) ⇒ f (a₁ & b) ≐ f (a₂ & b))
lem-wcon-×-congR con a₁ a₂ b t Heqa = lem-wcon-×-cong con a₁ a₂ b b t Heqa (λ _ _ → refl)

---------------------------------------------------------

lem-ne-wcon-×-congL-congR : {A B C D : Set} → {f : TFun (A × C) D} → {g : TFun B C} → NonExpansive f → WeaklyContractive g → (a : TVal A) → (b₁ b₂ : TVal B) 
                        → Always (H (b₁ ≐ b₂) ⇒ f (a & g b₁) ≐ f (a & g b₂))
lem-ne-wcon-×-congL-congR ne con a b₁ b₂ t Heq = lem-ne-×-congL ne a _ _ t (lem-wcon-Hʳ-cong con b₁ b₂ t Heq)

---------------------------------------------------------

-- lem-con→wcon : {A B : Set} → {f : TVal A → TVal B} → Contractive f → WeaklyContractive f
-- lem-con→wcon con s₁ s₂ t wHeq with con s₁ s₂
-- ... | ε , Heq = ? -- Heq t (λ t' lt → wHeq t' (lem-increasing lt))

-- lem-con-H-cong : {A B : Set} → {f : TFun A B} → Contractive f → (s₁ s₂ : TVal A) → Always (H (s₁ ≐ s₂) ⇒ H (f s₁ ≐ f s₂))
-- lem-con-H-cong con s₁ s₂ t Heq t' lt = con s₁ s₂ t' (reduce-range-< lt Heq)

-- lem-con-Hʳ-cong : {A B : Set} → {f : TFun A B} → Contractive f → (s₁ s₂ : TVal A) → Always (H (s₁ ≐ s₂) ⇒ Hʳ (f s₁ ≐ f s₂))
-- lem-con-Hʳ-cong con s₁ s₂ t Heq = con s₁ s₂ t Heq , lem-con-H-cong con s₁ s₂ t Heq

-- ---------------------------------------------------------

-- lem-con-×-cong : {A B C : Set} → {f : TFun (A × B) C} → Contractive f → (a₁ a₂ : TVal A) → (b₁ b₂ : TVal B) 
--                  → Always (H (a₁ ≐ a₂) ⇒ H (b₁ ≐ b₂) ⇒ f (a₁ & b₁) ≐ f (a₂ & b₂))
-- lem-con-×-cong con a₁ a₂ b₁ b₂ t Heqa Heqb = con (a₁ & b₁) (a₂ & b₂) t (×-cong3 Heqa Heqb)

-- lem-con-×-congL : {A B C : Set} → {f : TFun (A × B) C} → Contractive f → (a : TVal A) → (b₁ b₂ : TVal B) 
--                   → Always (H (b₁ ≐ b₂) ⇒ f (a & b₁) ≐ f (a & b₂))
-- lem-con-×-congL con a b₁ b₂ t = lem-con-×-cong con a a b₁ b₂ t (λ _ _ → refl)

-- lem-con-×-congR : {A B C : Set} → {f : TFun (A × B) C} → Contractive f → (a₁ a₂ : TVal A) → (b : TVal B)
--                  → Always (H (a₁ ≐ a₂) ⇒ f (a₁ & b) ≐ f (a₂ & b))
-- lem-con-×-congR con a₁ a₂ b t Heqa = lem-con-×-cong con a₁ a₂ b b t Heqa (λ _ _ → refl)

-- ---------------------------------------------------------

-- lem-ne-con-×-congL-congR : {A B C D : Set} → {f : TFun (A × C) D} → {g : TFun B C} → NonExpansive f → Contractive g → (a : TVal A) → (b₁ b₂ : TVal B) 
--                         → Always (H (b₁ ≐ b₂) ⇒ f (a & g b₁) ≐ f (a & g b₂))
-- lem-ne-con-×-congL-congR ne con a b₁ b₂ t Heq = lem-ne-×-congL ne a _ _ t (lem-con-Hʳ-cong con b₁ b₂ t Heq)

-- ---------------------------------------------------------
