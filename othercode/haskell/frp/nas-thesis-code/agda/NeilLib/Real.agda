{-# OPTIONS --type-in-type
   #-}

open import NeilPrelude
open import Logic

module Real where

open import StrictPosReal public

--------------------------------------------------------

infix  3  _<ℜ_ _≤ℜ_ _>ℜ_ _≥ℜ_

data ℜ : Set where
  O   : ℜ
  _>0 : ℜ⁺ → ℜ
  _<0 : ℜ⁺ → ℜ

_<ℜ_  : Rel ℜ
O <ℜ O           = False
O <ℜ (y >0)      = True
O <ℜ (y <0)      = False
(x >0) <ℜ O      = False
(x >0) <ℜ (y >0) = x <ℜ⁺ y
(x >0) <ℜ (y <0) = False
(x <0) <ℜ O      = True
(x <0) <ℜ (y >0) = True
(x <0) <ℜ (y <0) = y <ℜ⁺ x

ı : ℜ
ı = ı⁺ >0

-ı : ℜ
-ı = ı⁺ <0

halfℜ : ℜ → ℜ
halfℜ O      = O
halfℜ (x >0) = halfℜ⁺ x >0
halfℜ (x <0) = halfℜ⁺ x <0

--------------------------------------------------------

lem-ℜ->eq : {x y : ℜ⁺} → x >0 ≡ y >0 → x ≡ y
lem-ℜ->eq refl = refl

lem-ℜ-<eq : {x y : ℜ⁺} → x <0 ≡ y <0 → x ≡ y
lem-ℜ-<eq refl = refl

>0-cong : {x y : ℜ⁺} → x ≡ y → x >0 ≡ y >0
>0-cong = cong _>0

<0-cong : {x y : ℜ⁺} → x ≡ y → x <0 ≡ y <0
<0-cong = cong _<0

>0-neq-cong : {x y : ℜ⁺} → Not (x ≡ y) → Not (x >0 ≡ y >0)
>0-neq-cong n refl = n refl

<0-neq-cong : {x y : ℜ⁺} → Not (x ≡ y) → Not (x <0 ≡ y <0)
<0-neq-cong n refl = n refl

--------------------------------------------------------

<ℜ-trans : Transitive _<ℜ_
<ℜ-trans {x <0} {y} {O}    p q = _
<ℜ-trans {x <0} {y} {z >0} p q = _
<ℜ-trans {O}    {y} {z >0} p q = _
<ℜ-trans {x} {y >0} {O} p ()
<ℜ-trans {x} {O} {O} p ()
<ℜ-trans {x} {O} {z <0} p ()
<ℜ-trans {x} {y >0} {z <0} p ()
<ℜ-trans {O} {y <0} () q
<ℜ-trans {x >0} {O} () q
<ℜ-trans {x >0} {y <0} () q
<ℜ-trans {x >0} {y >0} {z >0} p q = <ℜ⁺-trans p q
<ℜ-trans {x <0} {y <0} {z <0} p q = <ℜ⁺-trans q p

<ℜ-trich : Trichotomous _<ℜ_
<ℜ-trich {O} {O} = inl (refl , id , id)
<ℜ-trich {O} {y >0} = inrl ((λ ()) , _ , id)
<ℜ-trich {O} {y <0} = inrr ((λ ()) , id , _)
<ℜ-trich {x >0} {O} = inrr ((λ ()) , id , _)
<ℜ-trich {x >0} {y <0} = inrr ((λ ()) , id , _)
<ℜ-trich {x <0} {O} = inrl ((λ ()) , _ , id)
<ℜ-trich {x <0} {y >0} = inrl ((λ ()) , _ , id)
<ℜ-trich {x >0} {y >0} with <ℜ⁺-trich {x} {y}
... | inl p = inl (first >0-cong p)
... | inr (inl p) = inrl (first >0-neq-cong p)
... | inr (inr p) = inrr (first >0-neq-cong p)
<ℜ-trich {x <0} {y <0} with <ℜ⁺-trich {y} {x}
... | inl p = inl (first (<0-cong ∘ sym) p)
... | inr (inl p) = inrl (first (<0-neq-cong ∘ argument sym) p)
... | inr (inr p) = inrr (first (<0-neq-cong ∘ argument sym) p)

--------------------------------------------------------

import StrictTotalOrder
module STOℜ = StrictTotalOrder _<ℜ_ (λ {x} → <ℜ-trans {x}) <ℜ-trich
open STOℜ

Compareℜ : Rel ℜ
Compareℜ = OrdCompare

CompareLeqℜ : Rel ℜ
CompareLeqℜ = OrdCompareLeq

CompareGeqℜ : Rel ℜ
CompareGeqℜ = OrdCompareGeq

compareℜ  : (x y : ℜ) → Compareℜ x y
compareℜ = compare

compareLeqℜ : (x y : ℜ) → CompareLeqℜ x y
compareLeqℜ = compareLeq

compareGeqℜ : (x y : ℜ) → CompareGeqℜ x y
compareGeqℜ = compareGeq

_>ℜ_ : Rel ℜ
_>ℜ_ = _>_

_≤ℜ_ : Rel ℜ
_≤ℜ_ = _≤_ 

_≥ℜ_ : Rel ℜ
_≥ℜ_ = _≥_ 

<ℜ-irreflexive : Irreflexive _<ℜ_
<ℜ-irreflexive {x} = irreflex {x}

<ℜ-asym : Asymmetric _<ℜ_
<ℜ-asym {x} = asym {x}

≤ℜ-antisym : Antisymmetric _≤ℜ_
≤ℜ-antisym = antisymmetric

≤ℜ-refl : Reflexive _≤ℜ_
≤ℜ-refl = reflexive

≤ℜ-trans : Transitive _≤ℜ_
≤ℜ-trans = ≤-trans

<≤ℜ-trans : Trans _<ℜ_ _≤ℜ_ _<ℜ_
<≤ℜ-trans = <≤-trans

≤<ℜ-trans : Trans _≤ℜ_ _<ℜ_ _<ℜ_
≤<ℜ-trans = ≤<-trans

<≤ℜ-asym : {x y : ℜ} → x <ℜ y → Not (y ≤ℜ x)
<≤ℜ-asym = <≤-asym

≤<ℜ-asym : {x y : ℜ} → x ≤ℜ y → Not (y <ℜ x)
≤<ℜ-asym = ≤<-asym

--------------------------------------------------------

ifℜ_≤_thenleq_elsemore_ : {A : Set} → (x y : ℜ) → (x ≤ℜ y → A) → ((x >ℜ y) → A) → A
ifℜ_≤_thenleq_elsemore_ = ifleq_≤_thenleq_elsemore_

ifℜ_≥_thengeq_elseless_ : {A : Set} → (x y : ℜ) → (x ≥ℜ y → A) → ((x <ℜ y) → A) → A
ifℜ_≥_thengeq_elseless_ = ifgeq_≥_thengeq_elseless_

ifℜ_<_thenless_elsegeq_ : {A : Set} → (x y : ℜ) → (x <ℜ y → A) → ((x ≥ℜ y) → A) → A
ifℜ_<_thenless_elsegeq_ = ifless_<_thenless_elsegeq_

ifℜ_>_thenmore_elseleq_ : {A : Set} → (x y : ℜ) → (x >ℜ y → A) → ((x ≤ℜ y) → A) → A
ifℜ_>_thenmore_elseleq_ = ifmore_>_thenmore_elseleq_

--------------------------------------------------------

<ℜ-subst : {x₁ y₁ x₂ y₂ : ℜ} → x₁ ≡ x₂ → y₁ ≡ y₂ → x₁ <ℜ y₁ → x₂ <ℜ y₂
<ℜ-subst refl refl = id

<ℜ-substL : {x₁ x₂ y : ℜ} → x₁ ≡ x₂ → x₁ <ℜ y → x₂ <ℜ y
<ℜ-substL refl = id

<ℜ-substR : {x y₁ y₂ : ℜ} → y₁ ≡ y₂ → x <ℜ y₁ → x <ℜ y₂
<ℜ-substR refl = id

≤ℜ-subst : {x₁ y₁ x₂ y₂ : ℜ} → x₁ ≡ x₂ → y₁ ≡ y₂ → x₁ ≤ℜ y₁ → x₂ ≤ℜ y₂
≤ℜ-subst refl refl = id

≤ℜ-substL : {x₁ x₂ y : ℜ} → x₁ ≡ x₂ → x₁ ≤ℜ y → x₂ ≤ℜ y
≤ℜ-substL refl = id

≤ℜ-substR : {x y₁ y₂ : ℜ} → y₁ ≡ y₂ → x ≤ℜ y₁ → x ≤ℜ y₂
≤ℜ-substR refl = id

--------------------------------------------------------

infixl 10 _+_ _+⁺_ _⁺+_ _+₀_ _₀+_ 

_+⁺_ : ℜ → ℜ⁺ → ℜ
O      +⁺ y = y >0
(x >0) +⁺ y = (x ⁺+⁺ y) >0
(x <0) +⁺ y with compareℜ⁺ x y
(x <0) +⁺ .x | refl   = O
(x <0) +⁺ y  | less p = (y ⁺-⁺ x) p >0
(x <0) +⁺ y  | more p = (x ⁺-⁺ y) p <0

_⁺+_ : ℜ⁺ → ℜ → ℜ
x ⁺+ O      = x >0
x ⁺+ (y >0) = (x ⁺+⁺ y) >0
x ⁺+ (y <0) with compareℜ⁺ x y
x ⁺+ (.x <0) | refl   = O
x ⁺+ (y <0)  | less p = (y ⁺-⁺ x) p <0
x ⁺+ (y <0)  | more p = (x ⁺-⁺ y) p >0

_+_ : ℜ → ℜ → ℜ
O      + y      = y
(x >0) + y      = x ⁺+ y
x      + O      = x
x      + (y >0) = x +⁺ y
(x <0) + (y <0) = (x ⁺+⁺ y) <0

open import PosReal

_₀+_ : ℜ₀ → ℜ → ℜ
O ₀+ y = y
(x >0) ₀+ y = x ⁺+ y

_+₀_ : ℜ → ℜ₀ → ℜ
x +₀ O = x
x +₀ (y >0) = x +⁺ y

--------------------------------------------------------

negate : ℜ → ℜ
negate O      = O
negate (x >0) = x <0
negate (x <0) = x >0

--------------------------------------------------------

infixl 10 _-_ _-⁺_ _⁺-_ _-₀_ _₀-_ 

_-_ : ℜ → ℜ → ℜ
x - y = x + negate y

_-⁺_ : ℜ → ℜ⁺ → ℜ
x -⁺ y = x + (y <0)

_⁺-_ : ℜ⁺ → ℜ → ℜ
x ⁺- y = x ⁺+ negate y

toℜ : ℜ₀ → ℜ
toℜ O      = O
toℜ (x >0) = x >0

_-₀_ : ℜ → ℜ₀ → ℜ
x -₀ y = x - toℜ y

_₀-_ : ℜ₀ → ℜ → ℜ
x ₀- y = toℜ x - y

_₀-₀_ : ℜ₀ → ℜ₀ → ℜ
x ₀-₀ y = toℜ x - toℜ y


--------------------------------------------------------

infixl 10 _*_ _*⁺_ _⁺*_ _*₀_ _₀*_ 

_*⁺_ : ℜ → ℜ⁺ → ℜ
O    *⁺ y = O
x >0 *⁺ y = (x ⁺*⁺ y) >0
x <0 *⁺ y = (x ⁺*⁺ y) <0

_⁺*_ : ℜ⁺ → ℜ → ℜ
x ⁺* O = O
x ⁺* y >0 = (x ⁺*⁺ y) >0
x ⁺* y <0 = (x ⁺*⁺ y) <0

_*_ : ℜ → ℜ → ℜ
O    * y = O
x >0 * y = x ⁺* y
x <0 * y = x ⁺* negate y

_*₀_ : ℜ → ℜ₀ → ℜ
x *₀ O    = O
x *₀ y >0 = x *⁺ y

_₀*_ : ℜ₀ → ℜ → ℜ
O    ₀* y = O
x >0 ₀* y = x ⁺* y

--------------------------------------------------------

open import List

sumℜ : List ℜ → ℜ
sumℜ []       = O
sumℜ (x ∷ xs) = x + sumℜ xs

--------------------------------------------------------

_<=_ : ℜ → ℜ → Bool
_<=_ = _<='_

_>=_ : ℜ → ℜ → Bool
_>=_ = _>='_

_<,_ : ℜ → ℜ → Bool
_<,_ = _<'_

_>,_ : ℜ → ℜ → Bool
_>,_ = _>'_

--------------------------------------------------------
