{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import Logic

module PosReal where

open import StrictPosReal public

--------------------------------------------------------

infix  3  _<ℜ₀_ _≤ℜ₀_ _>ℜ₀_ _≥ℜ₀_

data ℜ₀ : Set where
  O   : ℜ₀
  _>0 : ℜ⁺ → ℜ₀

ℜ₀-elim : {A : Set} → A → (ℜ⁺ → A) → ℜ₀ → A
ℜ₀-elim a f O      = a
ℜ₀-elim a f (x >0) = f x

_<ℜ₀_  : ℜ₀ → ℜ₀ → Set
_    <ℜ₀ O     = False
O  <ℜ₀ _ >0    = True
x >0 <ℜ₀ y >0  = x <ℜ⁺ y

ı₀ : ℜ₀
ı₀ = ı⁺ >0 

halfℜ₀ : ℜ₀ → ℜ₀
halfℜ₀ O      = O
halfℜ₀ (x >0) = halfℜ⁺ x >0

--------------------------------------------------------

>0-inj : Injective _>0
>0-inj refl = refl

lem-ℜ₀-neq : {x : ℜ⁺} → Not (O ≡ x >0)
lem-ℜ₀-neq ()

>0-cong : Congruent _>0
>0-cong = cong _>0

>0-neq-cong : {x y : ℜ⁺} → Not (x ≡ y) → Not (x >0 ≡ y >0)
>0-neq-cong n refl = n refl

--------------------------------------------------------

<ℜ₀-trans : Transitive _<ℜ₀_
<ℜ₀-trans {_} {_} {O} p ()
<ℜ₀-trans {_} {O} {_ >0} () q
<ℜ₀-trans {O} {_ >0} {_ >0} p q = _
<ℜ₀-trans {_ >0} {_ >0} {_ >0} p q = <ℜ⁺-trans p q

<ℜ₀-trich : Trichotomous _<ℜ₀_
<ℜ₀-trich {O} {O} = inl (refl , id , id)
<ℜ₀-trich {O} {y >0} = inrl (lem-ℜ₀-neq , _ , id)
<ℜ₀-trich {x >0} {O} = inrr (lem-ℜ₀-neq ∘ sym , id , _)
<ℜ₀-trich {x >0} {y >0} with <ℜ⁺-trich {x} {y}
... | inl p       = inl (first >0-cong p)
... | inr (inl p) = inrl (first >0-neq-cong p)
... | inr (inr p) = inrr (first >0-neq-cong p)

--------------------------------------------------------

import StrictTotalOrder
module STOℜ₀ = StrictTotalOrder _<ℜ₀_ <ℜ₀-trans <ℜ₀-trich
open STOℜ₀

Compareℜ₀ : Rel ℜ₀
Compareℜ₀ = OrdCompare

CompareLeqℜ₀ : Rel ℜ₀
CompareLeqℜ₀ = OrdCompareLeq

CompareGeqℜ₀ : Rel ℜ₀
CompareGeqℜ₀ = OrdCompareGeq

compareℜ₀ : (x y : ℜ₀) → Compareℜ₀ x y
compareℜ₀ = compare

compareLeqℜ₀ : (x y : ℜ₀) → CompareLeqℜ₀ x y
compareLeqℜ₀ = compareLeq

compareGeqℜ₀ : (x y : ℜ₀) → CompareGeqℜ₀ x y
compareGeqℜ₀ = compareGeq

_>ℜ₀_ : Rel ℜ₀
_>ℜ₀_ = _>_

_≤ℜ₀_ : Rel ℜ₀
_≤ℜ₀_ = _≤_ 

_≥ℜ₀_ : Rel ℜ₀
_≥ℜ₀_ = _≥_ 

<ℜ₀-irreflexive : Irreflexive _<ℜ₀_
<ℜ₀-irreflexive = irreflex

<ℜ₀-asym : Asymmetric _<ℜ₀_
<ℜ₀-asym = asym

≤ℜ₀-antisym : Antisymmetric _≤ℜ₀_
≤ℜ₀-antisym = antisymmetric

≤ℜ₀-refl : Reflexive _≤ℜ₀_
≤ℜ₀-refl = reflexive

≤ℜ₀-trans : Transitive _≤ℜ₀_
≤ℜ₀-trans = ≤-trans

<≤ℜ₀-trans : Trans _<ℜ₀_ _≤ℜ₀_ _<ℜ₀_
<≤ℜ₀-trans = <≤-trans

≤<ℜ₀-trans : Trans _≤ℜ₀_ _<ℜ₀_ _<ℜ₀_
≤<ℜ₀-trans = ≤<-trans

<≤ℜ₀-asym : {x y : ℜ₀} → x <ℜ₀ y → Not (y ≤ℜ₀ x)
<≤ℜ₀-asym = <≤-asym

≤<ℜ₀-asym : {x y : ℜ₀} → x ≤ℜ₀ y → Not (y <ℜ₀ x)
≤<ℜ₀-asym = ≤<-asym

--------------------------------------------------------

ifℜ₀_≤_thenleq_elsemore_ : {A : Set} → (x y : ℜ₀) → (x ≤ℜ₀ y → A) → ((x >ℜ₀ y) → A) → A
ifℜ₀_≤_thenleq_elsemore_ = ifleq_≤_thenleq_elsemore_

ifℜ₀_≥_thengeq_elseless_ : {A : Set} → (x y : ℜ₀) → (x ≥ℜ₀ y → A) → ((x <ℜ₀ y) → A) → A
ifℜ₀_≥_thengeq_elseless_ = ifgeq_≥_thengeq_elseless_

ifℜ₀_<_thenless_elsegeq_ : {A : Set} → (x y : ℜ₀) → (x <ℜ₀ y → A) → ((x ≥ℜ₀ y) → A) → A
ifℜ₀_<_thenless_elsegeq_ = ifless_<_thenless_elsegeq_

ifℜ₀_>_thenmore_elseleq_ : {A : Set} → (x y : ℜ₀) → (x >ℜ₀ y → A) → ((x ≤ℜ₀ y) → A) → A
ifℜ₀_>_thenmore_elseleq_ = ifmore_>_thenmore_elseleq_

--------------------------------------------------------

≤ℜ₀-min : {x : ℜ₀} → O ≤ℜ₀ x
≤ℜ₀-min {O}    = inr refl
≤ℜ₀-min {_ >0} = inl _

-----------------------------------------------------------------

<ℜ₀-subst : {x₁ y₁ x₂ y₂ : ℜ₀} → x₁ ≡ x₂ → y₁ ≡ y₂ → x₁ <ℜ₀ y₁ → x₂ <ℜ₀ y₂
<ℜ₀-subst refl refl = id

<ℜ₀-substL : {x₁ x₂ y : ℜ₀} → x₁ ≡ x₂ → x₁ <ℜ₀ y → x₂ <ℜ₀ y
<ℜ₀-substL refl = id

<ℜ₀-substR : {x y₁ y₂ : ℜ₀} → y₁ ≡ y₂ → x <ℜ₀ y₁ → x <ℜ₀ y₂
<ℜ₀-substR refl = id

≤ℜ₀-subst : {x₁ y₁ x₂ y₂ : ℜ₀} → x₁ ≡ x₂ → y₁ ≡ y₂ → x₁ ≤ℜ₀ y₁ → x₂ ≤ℜ₀ y₂
≤ℜ₀-subst refl refl = id

≤ℜ₀-substL : {x₁ x₂ y : ℜ₀} → x₁ ≡ x₂ → x₁ ≤ℜ₀ y → x₂ ≤ℜ₀ y
≤ℜ₀-substL refl = id

≤ℜ₀-substR : {x y₁ y₂ : ℜ₀} → y₁ ≡ y₂ → x ≤ℜ₀ y₁ → x ≤ℜ₀ y₂
≤ℜ₀-substR refl = id

-----------------------------------------------------------------

>0-inj-≤ : {x y : ℜ⁺} → (x >0) ≤ℜ₀ (y >0) → (x ≤ℜ⁺ y)
>0-inj-≤ = right >0-inj

>0-cong-≤ : {x y : ℜ⁺} → (x ≤ℜ⁺ y) → (x >0) ≤ℜ₀ (y >0)
>0-cong-≤ = right >0-cong

-----------------------------------------------------------------


infixl 10 _₀+₀_ _₀+⁺_ _⁺+₀_

_₀+₀_ : ℜ₀ → ℜ₀ → ℜ₀
O      ₀+₀ y      = y
x      ₀+₀ O      = x
(x >0) ₀+₀ (y >0) = (x ⁺+⁺ y) >0

_₀+⁺_ : ℜ₀ → ℜ⁺ → ℜ⁺
O      ₀+⁺ y  =  y
(x >0) ₀+⁺ y  =  x ⁺+⁺ y

_⁺+₀_ : ℜ⁺ → ℜ₀ → ℜ⁺
x ⁺+₀ O       =  x
x ⁺+₀ (y >0)  =  x ⁺+⁺ y

-----------------------------------------------------------------

₀+₀-comm : {x y : ℜ₀} → (x ₀+₀ y) ≡ (y ₀+₀ x)
₀+₀-comm {O} {O}        = refl
₀+₀-comm {O} {y >0}     = refl
₀+₀-comm {x >0} {O}     = refl
₀+₀-comm {x >0} {y >0}  = >0-cong ⁺+⁺-comm

-----------------------------------------------------------------

lem-ℜ₀-+-<-cancellativeL : {x y z : ℜ₀} → (p : x <ℜ₀ y) → x ₀+₀ z <ℜ₀ y ₀+₀ z
lem-ℜ₀-+-<-cancellativeL {x} {O} ()
lem-ℜ₀-+-<-cancellativeL {O} {y >0} {O} _ = _
lem-ℜ₀-+-<-cancellativeL {O} {y >0} {z >0} _ = lem-ℜ⁺-+-<-increasing
lem-ℜ₀-+-<-cancellativeL {x >0} {y >0} {O} p = p
lem-ℜ₀-+-<-cancellativeL {x >0} {y >0} {z >0} p = lem-ℜ⁺-+-<-cancellativeL p

lem-ℜ₀-+-<-cancellativeR : {x y z : ℜ₀} → (p : y <ℜ₀ z) → x ₀+₀ y <ℜ₀ x ₀+₀ z
lem-ℜ₀-+-<-cancellativeR {x} {y} {z} = <ℜ₀-subst (₀+₀-comm {y}) (₀+₀-comm {z}) ∘ lem-ℜ₀-+-<-cancellativeL {y} {z} {x}

-----------------------------------------------------------------

lem-ℜ₀-+-increasing : {x : ℜ₀} → (y : ℜ₀) → x ≤ℜ₀ (y ₀+₀ x)
lem-ℜ₀-+-increasing {O} _         = ≤ℜ₀-min
lem-ℜ₀-+-increasing {x >0} O      = ≤ℜ₀-refl
lem-ℜ₀-+-increasing {x >0} (y >0) = inl lem-ℜ⁺-+-<-increasing

lem-ℜ₀-+-increasingR : {x : ℜ₀} → (y : ℜ₀) → x ≤ℜ₀ (x ₀+₀ y) 
lem-ℜ₀-+-increasingR y = ≤ℜ₀-substR (₀+₀-comm {y}) (lem-ℜ₀-+-increasing y)

lem-ℜ⁺₀-⁺+₀-increasingR : {x : ℜ⁺} → (y : ℜ₀) → (x >0) ≤ℜ₀ (x ⁺+₀ y) >0
lem-ℜ⁺₀-⁺+₀-increasingR O = inr refl
lem-ℜ⁺₀-⁺+₀-increasingR (y >0) = inl lem-ℜ⁺-+-<-increasingR

lem-ℜ⁺₀-+-increasing : {x : ℜ⁺} → (y : ℜ₀) → x ≤ℜ⁺ (y ₀+⁺ x)
lem-ℜ⁺₀-+-increasing O      = ≤ℜ⁺-refl
lem-ℜ⁺₀-+-increasing (y >0) = inl lem-ℜ⁺-+-<-increasing

lem-ℜ⁺₀-⁺+-increasingR : {x : ℜ⁺} → (y : ℜ₀) → x ≤ℜ⁺ (x ⁺+₀ y)
lem-ℜ⁺₀-⁺+-increasingR O      = ≤ℜ⁺-refl
lem-ℜ⁺₀-⁺+-increasingR (y >0) = inl lem-ℜ⁺-+-<-increasingR

lem-ℜ₀⁺-₀+₀-increasing : {x : ℜ₀} → (y : ℜ⁺) → x <ℜ₀ (y >0 ₀+₀ x)
lem-ℜ₀⁺-₀+₀-increasing {O} y    = _
lem-ℜ₀⁺-₀+₀-increasing {x >0} y = lem-ℜ⁺-+-<-increasing

lem-ℜ₀⁺-₀+₀-increasingR : {x : ℜ₀} → (y : ℜ⁺) → x <ℜ₀ (x ₀+₀ y >0)
lem-ℜ₀⁺-₀+₀-increasingR {O} y = _
lem-ℜ₀⁺-₀+₀-increasingR {x >0} y = lem-ℜ⁺-+-<-increasingR

-----------------------------------------------------------------

lem-ℜ₀-+-x+y≮y : {y : ℜ₀} → (x : ℜ₀) → Not ((x ₀+₀ y) <ℜ₀ y)
lem-ℜ₀-+-x+y≮y x p = absurd (<≤ℜ₀-asym p (lem-ℜ₀-+-increasing x))

lem-ℜ⁺₀-+-≰0 : {x : ℜ⁺} → {y : ℜ₀} → Not (((x ⁺+₀ y) >0) ≤ℜ₀ O)
lem-ℜ⁺₀-+-≰0 (inl ())
lem-ℜ⁺₀-+-≰0 (inr ())

lem-ℜ⁺₀-+-x+y≰x : {x : ℜ⁺} → (y : ℜ₀) → Not ((x ⁺+₀ y) <ℜ⁺ x)
lem-ℜ⁺₀-+-x+y≰x y p = absurd (<≤ℜ⁺-asym p (lem-ℜ⁺₀-⁺+-increasingR y))

lem-ℜ⁺₀-+-x+y≰x₀ : {x : ℜ⁺} → (y : ℜ₀) → Not ((x ⁺+₀ y) >0 <ℜ₀ x >0)
lem-ℜ⁺₀-+-x+y≰x₀ y p = absurd (<≤ℜ⁺-asym p (lem-ℜ⁺₀-⁺+-increasingR y))

lem-ℜ⁺₀-+-z<x→x+y≰z : {x z : ℜ⁺} → (y : ℜ₀) → z <ℜ⁺ x → Not ((x ⁺+₀ y) ≤ℜ⁺ z)
lem-ℜ⁺₀-+-z<x→x+y≰z y lt p = <≤ℜ⁺-asym lt (≤ℜ⁺-trans (lem-ℜ⁺₀-⁺+-increasingR y) p)

lem-ℜ⁺₀-+-z<x→x+y≰z₀ : {x z : ℜ⁺} → (y : ℜ₀) → z <ℜ⁺ x → Not ((x ⁺+₀ y) >0 ≤ℜ₀ z >0)
lem-ℜ⁺₀-+-z<x→x+y≰z₀ y lt = lem-ℜ⁺₀-+-z<x→x+y≰z y lt ∘ >0-inj-≤

lem-ℜ⁺-+-≱-increasingR₀ : {x y : ℜ⁺} → Not (x >0 ≥ℜ₀ (x ⁺+⁺ y) >0)
lem-ℜ⁺-+-≱-increasingR₀ = lem-ℜ⁺-+-≱-increasingR ∘ >0-inj-≤

-----------------------------------------------------------------

lem-ℜ₀-nlt : {x : ℜ⁺} → Not (x >0 ≤ℜ₀ O)
lem-ℜ₀-nlt (inl ())
lem-ℜ₀-nlt (inr ())

ℜ₀⁺⁺-minus : (x : ℜ₀) → (y : ℜ⁺) → y >0 <ℜ₀ x → ℜ⁺
ℜ₀⁺⁺-minus O y ()
ℜ₀⁺⁺-minus (x >0) y p = ℜ⁺-minus x y p

ℜ₀⁺₀-minus : (x : ℜ₀) → (y : ℜ⁺) → y >0 ≤ℜ₀ x → ℜ₀
ℜ₀⁺₀-minus x y (inl p) = ℜ₀⁺⁺-minus x y p >0
ℜ₀⁺₀-minus .(y >0) y (inr refl) = O

ℜ⁺₀⁺-minus : (x : ℜ⁺) → (y : ℜ₀) → y <ℜ₀ x >0 → ℜ⁺
ℜ⁺₀⁺-minus x O p = x
ℜ⁺₀⁺-minus x (y >0) p = ℜ⁺-minus x y p

ℜ⁺₀₀-minus : (x : ℜ⁺) → (y : ℜ₀) → y ≤ℜ₀ x >0 → ℜ₀
ℜ⁺₀₀-minus x y (inl p) = ℜ⁺₀⁺-minus x y p >0
ℜ⁺₀₀-minus x .(x >0) (inr refl) = O

ℜ₀₀⁺-minus : (x y : ℜ₀) → y <ℜ₀ x → ℜ⁺
ℜ₀₀⁺-minus O y ()
ℜ₀₀⁺-minus (x >0) y p = ℜ⁺₀⁺-minus x y p

ℜ₀-minus : (x y : ℜ₀) → y ≤ℜ₀ x → ℜ₀
ℜ₀-minus x y (inl p) = ℜ₀₀⁺-minus x y p >0
ℜ₀-minus x .x (inr refl) = O

-----------------------------------------------------------------

postulate lem-ℜ₀-[x+y]-y=x : {x y : ℜ₀} → (p : y ≤ℜ₀ (x ₀+₀ y)) → ℜ₀-minus (x ₀+₀ y) y p ≡ x
postulate lem-ℜ₀-x⁺+₀y=z→y=z-x : {x : ℜ⁺} → {y z : ℜ₀} → (p : x >0 <ℜ₀ z) → (x ⁺+₀ y) >0 ≡ z → y ≡ (ℜ₀⁺⁺-minus z x p) >0

postulate lem-ℜ₀-x⁺+₀y<z→y<z-x : {x z : ℜ⁺} → {y : ℜ₀} → (p : x <ℜ⁺ z) → (x ⁺+₀ y) <ℜ⁺ z → y <ℜ₀ ((z ⁺-⁺ x) p) >0
postulate lem-ℜ₀-x⁺+₀y≤z→y≤z-x : {x z : ℜ⁺} → {y : ℜ₀} → (p : x <ℜ⁺ z) → (x ⁺+₀ y) ≤ℜ⁺ z → y ≤ℜ₀ ((z ⁺-⁺ x) p) >0
postulate lem-ℜ₀-y≤z⁺-⁺x→x⁺+₀y≤z : {x z : ℜ⁺} → {y : ℜ₀} → (p : x <ℜ⁺ z) → (y ≤ℜ₀ ((z ⁺-⁺ x) p) >0) → (x ⁺+₀ y) >0 ≤ℜ₀ z >0
postulate lem-ℜ₀-y<z⁺-⁺x→x⁺+₀y<z : {x z : ℜ⁺} → {y : ℜ₀} → (p : x <ℜ⁺ z) → (y <ℜ₀ ((z ⁺-⁺ x) p) >0) → (x ⁺+₀ y) <ℜ⁺ z
postulate lem-ℜ₀-y≤z₀-⁺x→x⁺+₀y≤z : {x : ℜ⁺} → {y z : ℜ₀} → (p : (x >0) ≤ℜ₀ z) → y ≤ℜ₀ ℜ₀⁺₀-minus z x p ↔ (x ⁺+₀ y) >0 ≤ℜ₀ z

postulate lem-ℜ₀-minus-<-decreasing : {x : ℜ₀} → {y : ℜ⁺} → (p : (y >0) <ℜ₀ x) → (ℜ₀⁺⁺-minus x y p >0) <ℜ₀ x
postulate lem-ℜ₀-minus-<-decreasing' : {x : ℜ₀} → {y : ℜ⁺} → (p : (y >0) ≤ℜ₀ x) → ℜ₀⁺₀-minus x y p <ℜ₀ x
postulate lem-ℜ₀-minus-≤-decreasing : {x y : ℜ₀} → (p : y ≤ℜ₀ x) → ℜ₀-minus x y p ≤ℜ₀ x

postulate lem-ℜ₀⁺₀-minus-O : {x : ℜ⁺} → (p : x >0 ≤ℜ₀ x >0) → ℜ₀⁺₀-minus (x >0) x p ≡ O

lem-ℜ₀-minus-<-cancellative : {x y z : ℜ₀} → (p : z ≤ℜ₀ x) → (q : z ≤ℜ₀ y) → x <ℜ₀ y → ℜ₀-minus x z p <ℜ₀ ℜ₀-minus y z q
lem-ℜ₀-minus-<-cancellative (inl p) (inr refl) lt = absurd (<ℜ₀-asym p lt)
lem-ℜ₀-minus-<-cancellative (inr refl) (inr refl) lt = absurd (<ℜ₀-irreflexive lt)
lem-ℜ₀-minus-<-cancellative {O} (inl ()) (inl q) lt
lem-ℜ₀-minus-<-cancellative {x >0} {O} (inl p) (inl ()) ()
lem-ℜ₀-minus-<-cancellative {x >0} {y >0} {O} (inl p) (inl q) lt = lt
lem-ℜ₀-minus-<-cancellative {x >0} {y >0} {z >0} (inl p) (inl q) lt = lem-ℜ⁺-minus-<-cancellative p q lt
lem-ℜ₀-minus-<-cancellative {O} (inr refl) (inl q) lt = _
lem-ℜ₀-minus-<-cancellative {x >0} {O} (inr refl) (inl ()) ()
lem-ℜ₀-minus-<-cancellative {x >0} {y >0} (inr refl) (inl q) lt = _

postulate lem-ℜ₀⁺₀-minus-<-cancellative : {x : ℜ₀} → {y z : ℜ⁺} → (p : (y >0) ≤ℜ₀ x) → (q : (z >0) ≤ℜ₀ x) → z <ℜ⁺ y → ℜ₀⁺₀-minus x y p <ℜ₀ ℜ₀⁺₀-minus x z q
postulate lem-ℜ₀⁺₀-minus-≤-cancellative : {x : ℜ₀} → {y z : ℜ⁺} → (p : (y >0) ≤ℜ₀ x) → (q : (z >0) ≤ℜ₀ x) → z ≤ℜ⁺ y → ℜ₀⁺₀-minus x y p ≤ℜ₀ ℜ₀⁺₀-minus x z q
postulate lem-ℜ₀⁺₀-minus-≤-cancellativeL : {x y : ℜ₀} → {z : ℜ⁺} → (p : (z >0) ≤ℜ₀ x) → (q : (z >0) ≤ℜ₀ y) → x ≤ℜ₀ y → ℜ₀⁺₀-minus x z p ≤ℜ₀ ℜ₀⁺₀-minus y z q


lem-ℜ₀-+ı-increasing : {x : ℜ₀} → x <ℜ₀ (x ₀+₀ ı₀)
lem-ℜ₀-+ı-increasing {O}    = _
lem-ℜ₀-+ı-increasing {x >0} = lem-ℜ⁺-+-<-increasingR

postulate ℜ⁺₀⁺-minus-proof-irrelevence : {x : ℜ⁺} → {y : ℜ₀} → (p q : y <ℜ₀ (x >0)) → ℜ⁺₀⁺-minus x y p ≡ ℜ⁺₀⁺-minus x y q
postulate ℜ₀⁺₀-minus-proof-irrelevence : {x : ℜ₀} → {y : ℜ⁺} → (p q : (y >0) ≤ℜ₀ x) → ℜ₀⁺₀-minus x y p ≡ ℜ₀⁺₀-minus x y q

-----------------------------------------------------------------

lem-ℜ₀-minus-<-pos-dif : {x y : ℜ₀} → (p : x <ℜ₀ y) → O <ℜ₀ ℜ₀-minus y x (inl p)
lem-ℜ₀-minus-<-pos-dif {O} p = _
lem-ℜ₀-minus-<-pos-dif {x >0} {O} ()
lem-ℜ₀-minus-<-pos-dif {x >0} {y >0} p = _

-----------------------------------------------------------------

infixl 11 _⁺*₀_ _₀*₀_ _₀*⁺_

_₀*⁺_  : ℜ₀ → ℜ⁺ → ℜ₀
O ₀*⁺ y = O
(x >0) ₀*⁺ y = (x ⁺*⁺ y) >0

_⁺*₀_  : ℜ⁺ → ℜ₀ → ℜ₀
x ⁺*₀ O = O
x ⁺*₀ (y >0) = (x ⁺*⁺ y) >0

_₀*₀_   : ℜ₀ → ℜ₀ → ℜ₀
O ₀*₀ y = O
(x >0) ₀*₀ y = x ⁺*₀ y

-----------------------------------------------------------------

open import List

sumℜ⁺ : List ℜ⁺ → ℜ₀
sumℜ⁺ []       = O
sumℜ⁺ (x ∷ xs) = (x ⁺+₀ sumℜ⁺ xs) >0 

sumℜ₀ : List ℜ₀ → ℜ₀
sumℜ₀ [] = O
sumℜ₀ (x ∷ xs) = x ₀+₀ sumℜ₀ xs


lem-ℜ⁺-sum≥₀head : {x : ℜ⁺} → (xs : List ℜ⁺) → sumℜ⁺ (x ∷ xs) ≥ℜ₀ x >0
lem-ℜ⁺-sum≥₀head [] = inr refl
lem-ℜ⁺-sum≥₀head (y ∷ xs) = inl lem-ℜ⁺-+-<-increasingR

-- sumℜ⁺≥0 : {r : ℜ⁺} → {rs : List ℜ⁺} → sumℜ⁺ (r ∷ rs) >ℜ₀ O
-- sumℜ⁺≥0 = _

----------------------------------------------------------------

_₀<=₀_ : ℜ₀ → ℜ₀ → Bool
_₀<=₀_ = _<='_

_₀>=₀_ : ℜ₀ → ℜ₀ → Bool
_₀>=₀_ = _>='_

_₀<₀_ : ℜ₀ → ℜ₀ → Bool
_₀<₀_ = _<'_

_₀>₀_ : ℜ₀ → ℜ₀ → Bool
_₀>₀_ = _>'_

_₀<=⁺_ : ℜ₀ → ℜ⁺ → Bool
x ₀<=⁺ y = x ₀<=₀ (y >0)

_₀>=⁺_ : ℜ₀ → ℜ⁺ → Bool
x ₀>=⁺ y = x ₀>=₀ (y >0)

_₀<⁺_ : ℜ₀ → ℜ⁺ → Bool
x ₀<⁺ y = x ₀<₀ (y >0)

_₀>⁺_ : ℜ₀ → ℜ⁺ → Bool
x ₀>⁺ y = x ₀>₀ (y >0)

_⁺<=₀_ : ℜ⁺ → ℜ₀ → Bool
x ⁺<=₀ y = y ₀>=⁺ x

_⁺>=₀_ : ℜ⁺ → ℜ₀ → Bool
x ⁺>=₀ y = y ₀<=⁺ x

_⁺<₀_ : ℜ⁺ → ℜ₀ → Bool
x ⁺<₀ y = y ₀>⁺ x

_⁺>₀_ : ℜ⁺ → ℜ₀ → Bool
x ⁺>₀ y = y ₀<⁺ x

----------------------------------------------------------------
