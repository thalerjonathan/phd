{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module StrictPreOrder {A : Set} (_<'_ : Rel A)

                      (irreflex : Irreflexive _<'_)
                      (transit  : Transitive _<'_)

where

------------------------------------------------------------------

infix 3 _>_ _<_

_<_ : Rel A
_<_ = _<'_

_>_ : Rel A
a > b = b < a

irreflexive : Irreflexive _<_
irreflexive = irreflex

<-trans : Transitive _<_
<-trans = transit

------------------------------------------------------------------

lem-less→Neq : {a b : A} → (a < b) → a ≢ b
lem-less→Neq p refl = irreflex p

lem-more→Neq : {a b : A} → (a > b) → a ≢ b
lem-more→Neq p refl = irreflex p

------------------------------------------------------------------

_≤_ : Rel A
a ≤ b = (a < b) ⊎ a ≡ b

_≥_ : Rel A
a ≥ b = b ≤ a

------------------------------------------------------------------

reflexive : Reflexive _≤_
reflexive = inr refl

≤-refl : Reflexive _≤_
≤-refl = reflexive

≤-trans : Transitive _≤_
≤-trans (inl p) (inl q) = inl (<-trans p q)
≤-trans p (inr refl)    = p
≤-trans (inr refl) q    = q

------------------------------------------------------------------

<≤-trans : Trans _<_ _≤_ _<_
<≤-trans p (inl q)    = <-trans p q
<≤-trans p (inr refl) = p

≤<-trans : Trans _≤_ _<_ _<_
≤<-trans (inl p) q    = <-trans p q
≤<-trans (inr refl) q = q

------------------------------------------------------------------

reduce-range-<-strict : {y : A} → {P : A → Set} → ((x : A) → x ≤ y → P x) → ((x : A) → x < y → P x)
reduce-range-<-strict f x lt = f x (inl lt)

reduce-range->-strict : {y : A} → {P : A → Set} → ((x : A) → x ≥ y → P x) → ((x : A) → x > y → P x)
reduce-range->-strict f x lt = f x (inl lt)

reduce-range-< : {y z : A} → {P : A → Set} → y < z → ((x : A) → x < z → P x) → ((x : A) → x < y → P x)
reduce-range-< lt₁ f x lt₂ = f x (<-trans lt₂ lt₁)

reduce-range-> : {y z : A} → {P : A → Set} → y > z → ((x : A) → x > z → P x) → ((x : A) → x > y → P x)
reduce-range-> lt₁ f x lt₂ = f x (<-trans lt₁ lt₂)

reduce-range-≤ : {y z : A} → {P : A → Set} → y ≤ z → ((x : A) → x ≤ z → P x) → ((x : A) → x ≤ y → P x)
reduce-range-≤ lt₁ f x lt₂ = f x (≤-trans lt₂ lt₁)

-- reduce-range-<<≤ : {y z : A} → {P : A → Set} → y < z → ((x : A) → x < z → P x) → ((x : A) → x ≤ y → P x)
-- reduce-range-<<≤ lt f x leq = f x (≤<-trans leq lt)

reduce-range-<-incl : {y z : A} → {P : A → Set} → y < z → ((x : A) → x < z → P x) → P y × ((x : A) → x < y → P x)
reduce-range-<-incl lt f = f _ lt , reduce-range-< lt f

reduce-range-≤-incl : {y z : A} → {P : A → Set} → y ≤ z → ((x : A) → x ≤ z → P x) → P y × ((x : A) → x ≤ y → P x)
reduce-range-≤-incl lt f = f _ lt , reduce-range-≤ lt f

------------------------------------------------------------------
