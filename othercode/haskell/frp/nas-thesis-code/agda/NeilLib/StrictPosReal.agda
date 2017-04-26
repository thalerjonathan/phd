{-# OPTIONS --type-in-type
   #-}

open import NeilPrelude

module StrictPosReal where

--------------------------------------------------------

infix  3  _<ℜ⁺_ _≤ℜ⁺_ _≥ℜ⁺_ _>ℜ⁺_

postulate ℜ⁺ : Set
postulate ı⁺ : ℜ⁺
postulate halfℜ⁺ : ℜ⁺ → ℜ⁺
postulate _<ℜ⁺_ : Rel ℜ⁺
postulate <ℜ⁺-trich : Trichotomous _<ℜ⁺_
postulate <ℜ⁺-trans : Transitive _<ℜ⁺_

--------------------------------------------------------

Δ : Set
Δ = ℜ⁺

--------------------------------------------------------

import StrictTotalOrder
module STOℜ⁺ = StrictTotalOrder _<ℜ⁺_ <ℜ⁺-trans <ℜ⁺-trich
open STOℜ⁺

Compareℜ⁺ : Rel ℜ⁺
Compareℜ⁺ = OrdCompare

CompareLeqℜ⁺ : Rel ℜ⁺
CompareLeqℜ⁺ = OrdCompareLeq

CompareGeqℜ⁺ : Rel ℜ⁺
CompareGeqℜ⁺ = OrdCompareGeq

compareℜ⁺ : (x y : ℜ⁺) → Compareℜ⁺ x y
compareℜ⁺ = compare

compareLeqℜ⁺ : (x y : ℜ⁺) → CompareLeqℜ⁺ x y
compareLeqℜ⁺ = compareLeq

compareGeqℜ⁺ : (x y : ℜ⁺) → CompareGeqℜ⁺ x y
compareGeqℜ⁺ = compareGeq

_>ℜ⁺_ : Rel ℜ⁺
_>ℜ⁺_ = _>_

_≤ℜ⁺_ : Rel ℜ⁺
_≤ℜ⁺_ = _≤_ 

_≥ℜ⁺_ : Rel ℜ⁺
_≥ℜ⁺_ = _≥_ 

<ℜ⁺-irreflexive : Irreflexive _<ℜ⁺_
<ℜ⁺-irreflexive = irreflex

<ℜ⁺-asym : Asymmetric _<ℜ⁺_
<ℜ⁺-asym = asym

≤ℜ⁺-antisym : Antisymmetric _≤ℜ⁺_
≤ℜ⁺-antisym = antisymmetric

≤ℜ⁺-refl : Reflexive _≤ℜ⁺_
≤ℜ⁺-refl = reflexive

≤ℜ⁺-trans : Transitive _≤ℜ⁺_
≤ℜ⁺-trans = ≤-trans

<≤ℜ⁺-trans : Trans _<ℜ⁺_ _≤ℜ⁺_ _<ℜ⁺_
<≤ℜ⁺-trans = <≤-trans

≤<ℜ⁺-trans : Trans _≤ℜ⁺_ _<ℜ⁺_ _<ℜ⁺_
≤<ℜ⁺-trans = ≤<-trans

<≤ℜ⁺-asym : {x y : ℜ⁺} → x <ℜ⁺ y → Not (y ≤ℜ⁺ x)
<≤ℜ⁺-asym = <≤-asym

≤<ℜ⁺-asym : {x y : ℜ⁺} → x ≤ℜ⁺ y → Not (y <ℜ⁺ x)
≤<ℜ⁺-asym = ≤<-asym

--------------------------------------------------------

ifℜ⁺_≤_thenleq_elsemore_ : {A : Set} → (x y : ℜ⁺) → (x ≤ℜ⁺ y → A) → ((x >ℜ⁺ y) → A) → A
ifℜ⁺_≤_thenleq_elsemore_ = ifleq_≤_thenleq_elsemore_

ifℜ⁺_≥_thengeq_elseless_ : {A : Set} → (x y : ℜ⁺) → (x ≥ℜ⁺ y → A) → ((x <ℜ⁺ y) → A) → A
ifℜ⁺_≥_thengeq_elseless_ = ifgeq_≥_thengeq_elseless_

ifℜ⁺_<_thenless_elsegeq_ : {A : Set} → (x y : ℜ⁺) → (x <ℜ⁺ y → A) → ((x ≥ℜ⁺ y) → A) → A
ifℜ⁺_<_thenless_elsegeq_ = ifless_<_thenless_elsegeq_

ifℜ⁺_>_thenmore_elseleq_ : {A : Set} → (x y : ℜ⁺) → (x >ℜ⁺ y → A) → ((x ≤ℜ⁺ y) → A) → A
ifℜ⁺_>_thenmore_elseleq_ = ifmore_>_thenmore_elseleq_

--------------------------------------------------------

<ℜ⁺-subst : {x₁ y₁ x₂ y₂ : ℜ⁺} → x₁ ≡ x₂ → y₁ ≡ y₂ → x₁ <ℜ⁺ y₁ → x₂ <ℜ⁺ y₂
<ℜ⁺-subst refl refl = id

<ℜ⁺-substL : {x₁ x₂ y : ℜ⁺} → x₁ ≡ x₂ → x₁ <ℜ⁺ y → x₂ <ℜ⁺ y
<ℜ⁺-substL refl = id

<ℜ⁺-substR : {x y₁ y₂ : ℜ⁺} → y₁ ≡ y₂ → x <ℜ⁺ y₁ → x <ℜ⁺ y₂
<ℜ⁺-substR refl = id

≤ℜ⁺-subst : {x₁ y₁ x₂ y₂ : ℜ⁺} → x₁ ≡ x₂ → y₁ ≡ y₂ → x₁ ≤ℜ⁺ y₁ → x₂ ≤ℜ⁺ y₂
≤ℜ⁺-subst refl refl = id

≤ℜ⁺-substL : {x₁ x₂ y : ℜ⁺} → x₁ ≡ x₂ → x₁ ≤ℜ⁺ y → x₂ ≤ℜ⁺ y
≤ℜ⁺-substL refl = id

≤ℜ⁺-substR : {x y₁ y₂ : ℜ⁺} → y₁ ≡ y₂ → x ≤ℜ⁺ y₁ → x ≤ℜ⁺ y₂
≤ℜ⁺-substR refl = id

--------------------------------------------------------

infixl 10 _⁺+⁺_

postulate _⁺+⁺_ : ℜ⁺ → ℜ⁺ → ℜ⁺
postulate ℜ⁺-minus : (x y : ℜ⁺) → y <ℜ⁺ x → ℜ⁺

_⁺-⁺_ : (x y : ℜ⁺) → y <ℜ⁺ x → ℜ⁺
_⁺-⁺_ = ℜ⁺-minus

postulate ⁺+⁺-comm : {x y : ℜ⁺} → (x ⁺+⁺ y) ≡ (y ⁺+⁺ x)

--------------------------------------------------------

postulate lem-ℜ⁺-minus-proofirrelevence : {x y : ℜ⁺} → (p q : y <ℜ⁺ x) → (x ⁺-⁺ y) p ≡ (x ⁺-⁺ y) q

postulate ⁺+⁺-cancL : CancellativeL _⁺+⁺_

⁺+⁺-cancR : CancellativeR _⁺+⁺_
⁺+⁺-cancR eq = ⁺+⁺-cancL (trans2 ⁺+⁺-comm eq ⁺+⁺-comm)

postulate lem-ℜ⁺-+-<-cancellativeL : {x y z : ℜ⁺} → (p : x <ℜ⁺ y) → x ⁺+⁺ z <ℜ⁺ y ⁺+⁺ z

lem-ℜ⁺-+-<-cancellativeR : {x y z : ℜ⁺} → (p : y <ℜ⁺ z) → x ⁺+⁺ y <ℜ⁺ x ⁺+⁺ z
lem-ℜ⁺-+-<-cancellativeR {x} {y} {z} = <ℜ⁺-subst ⁺+⁺-comm ⁺+⁺-comm ∘ lem-ℜ⁺-+-<-cancellativeL {y} {z} {x}

postulate lem-ℜ⁺-minus-<-cancellative : {x y z : ℜ⁺} → (p : z <ℜ⁺ x) → (q : z <ℜ⁺ y) → x <ℜ⁺ y → (x ⁺-⁺ z) p <ℜ⁺ (y ⁺-⁺ z) q

lem-ℜ⁺-minus-≤-cancellative : {x y z : ℜ⁺} → (p : z <ℜ⁺ x) → (q : z <ℜ⁺ y) → x ≤ℜ⁺ y → (x ⁺-⁺ z) p ≤ℜ⁺ (y ⁺-⁺ z) q
lem-ℜ⁺-minus-≤-cancellative p q (inl lt) = inl (lem-ℜ⁺-minus-<-cancellative p q lt)
lem-ℜ⁺-minus-≤-cancellative p q (inr refl) = inr (lem-ℜ⁺-minus-proofirrelevence p q)

postulate lem-ℜ⁺-+-<-increasing : {x y : ℜ⁺} → x <ℜ⁺ (y ⁺+⁺ x)

lem-ℜ⁺-+-<-increasingR : {x y : ℜ⁺} → x <ℜ⁺ (x ⁺+⁺ y) 
lem-ℜ⁺-+-<-increasingR = <ℜ⁺-substR ⁺+⁺-comm lem-ℜ⁺-+-<-increasing

lem-ℜ⁺-+-≱-increasingR : {x y : ℜ⁺} → Not (x ≥ℜ⁺ (x ⁺+⁺ y))
lem-ℜ⁺-+-≱-increasingR p = <≤ℜ⁺-asym lem-ℜ⁺-+-<-increasingR p

--------------------------------------------------------

postulate lem-ℜ⁺-x+[y-x]=y : {x y : ℜ⁺} → (p : x <ℜ⁺ y) → x ⁺+⁺ ((y ⁺-⁺ x) p) ≡ y
postulate lem-ℜ⁺-[x+y]-x=y : {x y : ℜ⁺} → (p : x <ℜ⁺ (x ⁺+⁺ y)) → ((x ⁺+⁺ y) ⁺-⁺ x) p ≡ y
postulate lem-ℜ⁺-x-y<z→x<y+z : {x y z : ℜ⁺} → (p : y <ℜ⁺ x) → ((x ⁺-⁺ y) p <ℜ⁺ z) → x <ℜ⁺ (y ⁺+⁺ z)
postulate lem-ℜ⁺-y<z-x→x+y<z : {x y z : ℜ⁺} → (p : x <ℜ⁺ z) → (y <ℜ⁺ (z ⁺-⁺ x) p) → (x ⁺+⁺ y) <ℜ⁺ z
postulate lem-ℜ⁺-x-[y+z]=[x-y]-z : {x y z : ℜ⁺} → (p : (y ⁺+⁺ z) <ℜ⁺ x) → (q : y <ℜ⁺ x) → (r : z <ℜ⁺ ((x ⁺-⁺ y) q) ) → ((x ⁺-⁺ (y ⁺+⁺ z)) p ≡ ((x ⁺-⁺ y) q ⁺-⁺ z) r)

--------------------------------------------------------

infixl 11 _⁺*⁺_

postulate _⁺*⁺_   : ℜ⁺ → ℜ⁺ → ℜ⁺

--------------------------------------------------------

_⁺<=⁺_ : ℜ⁺ → ℜ⁺ → Bool
_⁺<=⁺_ = _<='_

_⁺>=⁺_ : ℜ⁺ → ℜ⁺ → Bool
_⁺>=⁺_ = _>='_

_⁺<⁺_ : ℜ⁺ → ℜ⁺ → Bool
_⁺<⁺_ = _<'_

_⁺>⁺_ : ℜ⁺ → ℜ⁺ → Bool
_⁺>⁺_ = _>'_

----------------------------------------------------------------
