{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import Logic

module TemporalLogic

  (Time    : Set)
  (_<'_    : Rel Time)
  (transit : Transitive _<'_)
  (trich   : Trichotomous _<'_)

where

import StrictTotalOrder
open StrictTotalOrder _<'_ transit trich public

-------------------------------

infixr 0  _⇒_
infixr 5  _∨_
infixr 6  _∧_

-------------------------------

TPred = Time → Set
TimePred = TPred

-------------------------------

-- Lifted Logical Operators

_∨_ : TPred → TPred → TPred
(φ ∨ ψ) t = φ t ⊎ ψ t

_∧_ : TPred → TPred → TPred
(φ ∧ ψ) t = φ t × ψ t

_⇒_ : TPred → TPred → TPred
(φ ⇒ ψ) t = φ t → ψ t

⊥ : TPred
⊥ t = False

⊤ : TPred
⊤ t = True

¬_ : TPred → TPred
¬ φ = φ ⇒ ⊥

-----------------------------------------

-- Unary Operators (Strict variants)

-- Global

G : TPred → TPred
G φ t = (t' : Time) → t' > t → φ t'

-- History

H : TPred → TPred
H φ t = (t' : Time) → t' < t → φ t'

-- Future

F : TPred → TPred
F φ t = Σ Time (λ t' → t' > t × φ t')

-- Past

P : TPred → TPred
P φ t = Σ Time (λ t' → t' < t × φ t')


-- Unary Operators (Reflexive variants)

Gʳ : TPred → TPred
Gʳ φ = φ ∧ G φ

Hʳ : TPred → TPred
Hʳ φ = φ ∧ H φ

Fʳ : TPred → TPred
Fʳ φ = φ ∨ F φ

Pʳ : TPred → TPred
Pʳ φ = φ ∨ P φ

--------------------------------------------------

import Interval
open Interval Time _<_ public

Over : TPred → Interval → Set
Over φ i = ∀ t → t ∈ i → φ t

-- Over' : TPred → Interval → Set
-- Over' φ (⟨ t₀ , t₁ ⟩) = (t : Time) → t₀ < t → t < t₁ → φ t
-- Over' φ (⟨ t₀ , t₁ ]) = (t : Time) → t₀ < t → t ≤ t₁ → φ t
-- Over' φ ([ t₀ , t₁ ⟩) = (t : Time) → t₀ ≤ t → t < t₁ → φ t
-- Over' φ ([ t₀ , t₁ ]) = (t : Time) → t₀ ≤ t → t ≤ t₁ → φ t

--------------------------------------------------

-- Until

_U_ : TPred → TPred → TPred
(φ U ψ) t₁ = F (ψ ∧ (λ t₂ → Over φ ⟨ t₁ , t₂ ⟩)) t₁

_Uʳ_ : TPred → TPred → TPred
(φ Uʳ ψ) t₁ = Fʳ (ψ ∧ (λ t₂ → Over φ [ t₁ , t₂ ⟩)) t₁

_U'_ : TPred → TPred → TPred
(φ U' ψ) t₁ = F (ψ ∧ (λ t₂ → Over φ ⟨ t₁ , t₂ ])) t₁

_Uʳ'_ : TPred → TPred → TPred
(φ Uʳ' ψ) t₁ = Fʳ (ψ ∧ (λ t₂ → Over φ [ t₁ , t₂ ])) t₁

-- Since

_S_ : TPred → TPred → TPred
(φ S ψ) t₁ = P (ψ ∧ (λ t₀ → Over φ ⟨ t₀ , t₁ ⟩)) t₁

_Sʳ_ : TPred → TPred → TPred
(φ Sʳ ψ) t₁ = Pʳ (ψ ∧ (λ t₀ → Over φ ⟨ t₀ , t₁ ])) t₁

_S'_ : TPred → TPred → TPred
(φ S' ψ) t₁ = P (ψ ∧ (λ t₀ → Over φ [ t₀ , t₁ ⟩)) t₁

_Sʳ'_ : TPred → TPred → TPred
(φ Sʳ' ψ) t₁ = Pʳ (ψ ∧ (λ t₀ → Over φ [ t₀ , t₁ ])) t₁


-- Weak Until

_W_ : TPred → TPred → TPred
φ W ψ = (φ U ψ) ∨ G φ

_Wʳ_ : TPred → TPred → TPred
φ Wʳ ψ = (φ Uʳ ψ) ∨ Gʳ φ

_W'_ : TPred → TPred → TPred
φ W' ψ = (φ U' ψ) ∨ G φ

_Wʳ'_ : TPred → TPred → TPred
φ Wʳ' ψ = φ Uʳ' ψ ∨ Gʳ φ


-- Weak Since (Back-to)

_B_ : TPred → TPred → TPred
φ B ψ = (φ S ψ) ∨ H φ

_Bʳ_ : TPred → TPred → TPred
φ Bʳ ψ = (φ Sʳ ψ) ∨ Hʳ φ

_B'_ : TPred → TPred → TPred
φ B' ψ = (φ S' ψ) ∨ H φ

_Bʳ'_ : TPred → TPred → TPred
φ Bʳ' ψ = φ Sʳ' ψ ∨ Hʳ φ


---------------------------------------------------------

-- A utility

Always : TPred → Set
Always = Π Time

---------------------------------------------------------

-- Some Properties

FirstPoint : Set
FirstPoint = Always (Pʳ (H ⊥))

EndPoint : Set
EndPoint = Always (Fʳ (G ⊥))

Density : Set
Density = ∀ {φ} → Always (F φ ⇒ F (F φ))

NonBranching : Set
NonBranching = ∀ {φ} →  Always ( (P (F φ) ⇒ (P φ ∨ φ ∨ F φ))
                               ∧ (F (P φ) ⇒ (F φ ∨ φ ∨ P φ)))

---------------------------------------------------------

lem-G : {φ ψ : TPred} → Always (G (φ ⇒ ψ) ⇒ G φ ⇒ G ψ)
lem-G t = S-comb₂

lemGn : {φ ψ : TPred} → Always (G (φ ⇒ ψ) ⇒ ¬ G ψ ⇒ ¬ G φ)
lemGn t φ⇒ψ = argument (lem-G t φ⇒ψ)

lem-GF : {φ ψ : TPred} → Always (G (φ ⇒ ψ) ⇒ F φ ⇒ F ψ)
lem-GF t φ⇒ψ (t' , lt , φ') = t' , lt , φ⇒ψ t' lt φ'

lem-NF⇒GN : {φ : TPred} → Always (¬ F φ ⇒ G (¬ φ))
lem-NF⇒GN t = curry2

lem-NG⇒FN : {φ : TPred} → EM → Always (¬ G φ ⇒ F (¬ φ))
lem-NG⇒FN em t = must-exist-not2 em

lem-NP⇒HN : {φ : TPred} → Always (¬ P φ ⇒ H (¬ φ))
lem-NP⇒HN t = curry2

lem-F⇒NGN : {φ : TPred} → Always (F φ ⇒ ¬ G (¬ φ))
lem-F⇒NGN t = flip uncurry2

lem-G⇒NFN : {φ : TPred} → Always (G φ ⇒ ¬ F (¬ φ))
lem-G⇒NFN t Gφ (t' , gt , nφt) = nφt (Gφ t' gt)

lem-NFN⇒G : {φ : TPred} → EM → Always (¬ F (¬ φ) ⇒ G φ)
lem-NFN⇒G em t = result2' (contradiction em) ∘ curry2

lem-NGN⇒F : {φ : TPred} → EM → Always (¬ G (¬ φ) ⇒ F φ)
lem-NGN⇒F em t = contradiction em ∘ argument curry2

---------------------------------------------------------

lem-G⇒GGʳ : {φ : TPred} → Always (G φ ⇒ G (Gʳ φ))
lem-G⇒GGʳ t Gφ t' lt = Gφ t' lt , reduce-range-> lt Gφ

lem-Gʳ⇒GGʳ : {φ : TPred} → Always (Gʳ φ ⇒ G (Gʳ φ))
lem-Gʳ⇒GGʳ t (_ , Gφ) = lem-G⇒GGʳ t Gφ

lem-G⇒GG : {φ : TPred} → Always (G φ ⇒ G (G φ))
lem-G⇒GG t₂ Gφ t₁ lt₁ = snd (lem-G⇒GGʳ t₂ Gφ t₁ lt₁) 

lem-H⇒HHʳ : {φ : TPred} → Always (H φ ⇒ H (Hʳ φ))
lem-H⇒HHʳ t Hφ t' lt = Hφ t' lt , reduce-range-< lt Hφ

lem-Hʳ⇒HHʳ : {φ : TPred} → Always (Hʳ φ ⇒ H (Hʳ φ))
lem-Hʳ⇒HHʳ t (_ , Hφ) = lem-H⇒HHʳ t Hφ

lem-H⇒HH : {φ : TPred} → Always (H φ ⇒ H (H φ))
lem-H⇒HH t₂ Hφ t₁ lt₁ = snd (lem-H⇒HHʳ t₂ Hφ t₁ lt₁) 

---------------------------------------------------------


lem-H⇒H : {φ ψ : TPred} → Always (H φ ⇒ ψ) → Always (H φ ⇒ H ψ)
lem-H⇒H p t Hq t' lt = p t' (reduce-range-< lt Hq)

lem-H⇒Hʳ : {φ ψ : TPred} → Always (H φ ⇒ ψ) → Always (H φ ⇒ Hʳ ψ)
lem-H⇒Hʳ p t Hq = p t Hq , λ t' lt → p t' (reduce-range-< lt Hq)

lem-Hʳ⇒H : {φ ψ : TPred} → Always (Hʳ φ ⇒ ψ) → Always (H φ ⇒ H ψ)
lem-Hʳ⇒H p t Hq t' lt = p t' (Hq t' lt , reduce-range-< lt Hq)

lem-Hʳ⇒Hʳ : {φ ψ : TPred} → Always (Hʳ φ ⇒ ψ) → Always (Hʳ φ ⇒ Hʳ ψ)
lem-Hʳ⇒Hʳ p t (q , Hq)  = p t (q , Hq) , lem-Hʳ⇒H p t Hq

lem-Hʳ⇒Always : {φ ψ : TPred} → Always (Hʳ φ ⇒ ψ) → Always φ → Always ψ
lem-Hʳ⇒Always p Φ t = p t (Φ t , (λ t' _ → Φ t'))

lem-H⇒Always : {φ ψ : TPred} → Always (H φ ⇒ ψ) → Always φ → Always ψ
lem-H⇒Always p Φ t = p t (λ t' _ → Φ t')

---------------------------------------------------------

lem-Always⇒H : {φ : TPred} → Always φ → Always (H φ)
lem-Always⇒H Φ _ t _ = Φ t

lem-Always⇒Hʳ : {φ : TPred} → Always φ → Always (Hʳ φ)
lem-Always⇒Hʳ Φ t = Φ t , λ t' _ → Φ t'

lem-Always⇒G : {φ : TPred} → Always φ → Always (G φ)
lem-Always⇒G Φ _ t _ = Φ t

lem-Always⇒Gʳ : {φ : TPred} → Always φ → Always (Gʳ φ)
lem-Always⇒Gʳ Φ t = Φ t , λ t' _ → Φ t'

---------------------------------------------------------


-- tl-contpos : {φ ψ : TPred} → Always ((φ ⇒ ψ) ⇒ (¬ ψ ⇒ ¬ φ))
-- tl-contpos t = _⋙_

-- tl-contpos-em : {φ ψ : TPred} → EM → Always ((¬ φ ⇒ ¬ ψ) ⇒ (ψ ⇒ φ))
-- tl-contpos-em {φ} em t p ψt with em {φ t}
-- ... | inl  φt  = φt
-- ... | inr nφt = absurd (p nφt ψt)

-- tl-contr : {φ ψ : TPred} → Always (¬ (φ ∧ ¬ ψ ∧ (φ ⇒ ψ)))
-- tl-contr t (φt , nψt , p) = nψt (p φt)

-- tl-nn-antecdent : {φ ψ : TPred} → Always ((¬ ¬ φ ⇒ ψ) ⇒ (φ ⇒ ψ))
-- tl-nn-antecdent t = nn-antecedent

-- tl-nn-consequent : {φ ψ : TPred} → EM → Always ((φ ⇒ ¬ ¬ ψ) ⇒ (φ ⇒ ψ))
-- tl-nn-consequent {φ} {ψ} em t p φt with em {ψ t}
-- ... | inl ψt   = ψt
-- ... | inr nψt = absurd (p φt nψt)

----------------------------------------------------------

lem-Gr : {φ : TPred} → EM → Always (¬ Gʳ φ ⇒ ¬ φ ∨ ¬ G φ)
lem-Gr {φ} em t nGrφ with em {φ t}
... | inr nφt = inl nφt
... | inl φt  = left (flip (curry nGrφ)) em
{-
----------------------------------------------------------

lem-NGr⇒FrN : {φ : TPred} → EM → Always (¬ Gʳ φ ⇒ Fʳ (¬ φ))
lem-NGr⇒FrN em t nGrφ = right (lem-NG⇒FN em t) (lem-Gr em t nGrφ)


---------------------------------------------------------

lem-Wʳ : {φ ψ : TPred} → EM → Always (Gʳ (φ ⇒ ψ) ⇒ (ψ Wʳ ¬ φ))
lem-Wʳ {φ} em t (φt⇒ψt , Gφ⇒ψ) with em {φ t}
... | inr nφt = inl nφt
... | inl φt with φt⇒ψt φt
...    | ψt = inr (ψt , λ t' gt f u gt1 gt2 → Gφ⇒ψ u gt1 (contradiction em (f u gt1 gt2)))

lem-Wʳn' : {φ ψ : TPred} → EM → Always (Gʳ (¬ φ ⇒ ¬ ψ) ⇒ (¬ ψ Wʳ ¬ ¬ φ))
lem-Wʳn' = lem-Wʳ

lem-Wʳn : {φ ψ : TPred} → EM → Always (Gʳ (¬ φ ⇒ ¬ ψ) ⇒ (¬ ψ Wʳ φ))
lem-Wʳn {φ} {ψ} em t = result (map-⊎ (contradiction em) (second (result2' (argument (result3' NotNot))))) (lem-Wʳn' {φ} {ψ} em t)
-}

---------------------------------------------------------
