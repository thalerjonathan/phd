{-# OPTIONS --type-in-type
   #-}

open import NeilPrelude
open import PosReal

module PosInterval where

------------------------------------------------------

data Interval : Set where
  closedIvl   : ℜ  → Interval
  openIvl     : ℜ⁺ → Interval

------------------------------------------------------

infix 3 _<I_ _>I_ _≤I_ _≥I_

data _<I_ : Interval → Interval → Set where
  <closed     : {r₁ r₂ : ℜ}         → r₁      <ℜ  r₂      → closedIvl r₁ <I closedIvl r₂ 
  <open       : {r₁ r₂ : ℜ⁺}        → r₁      <ℜ⁺ r₂      → openIvl   r₁ <I openIvl   r₂
  <openclosed : {r : ℜ} → {r⁺ : ℜ⁺} → (r⁺ >0) ≤ℜ  r       → openIvl   r⁺ <I closedIvl r
  <closedopen : {r : ℜ} → {r⁺ : ℜ⁺} → r       <ℜ (r⁺ >0)  → closedIvl r  <I openIvl   r⁺

_>I_ : Interval → Interval → Set
i >I j = j <I i

_≤I_ : Interval → Interval → Set
i ≤I j = i <I j ⊎ i ≡ j

_≥I_ : Interval → Interval → Set
i ≥I j = j ≤I i

<openclosed-refl : {r : ℜ⁺} → openIvl r <I closedIvl (r >0)
<openclosed-refl = <openclosed (inr refl)

≤openclosed-refl : {r : ℜ⁺} → openIvl r ≤I closedIvl (r >0)
≤openclosed-refl = inl <openclosed-refl

------------------------------------------------------

≤I-refl : {i : Interval} → i ≤I i
≤I-refl = inr refl

-- I can't be bothered to prove these right now

postulate <I-trans : {i j k : Interval} → i <I j → j <I k → i <I k
postulate ≤I-trans : {i j k : Interval} → i ≤I j → j ≤I k → i ≤I k

------------------------------------------------------

WithinInterval : Interval → ℜ → Set
WithinInterval (closedIvl i) r = r ≤ℜ i
WithinInterval (openIvl   i) r = r <ℜ (i >0)

BeyondInterval : Interval → ℜ → Set
BeyondInterval = Not ∘₂ WithinInterval

withinIntervalLem : {i₁ i₂ : Interval} → i₁ ≤I i₂ → (r : ℜ) → WithinInterval i₁ r → WithinInterval i₂ r
withinIntervalLem (inl (<closed lt1)) r (inl lt2) = inl (<ℜ-trans lt2 lt1)
withinIntervalLem (inl (<closed lt)) r (inr refl) = inl lt
withinIntervalLem (inl (<open lt)) r p = <ℜ-trans p lt
withinIntervalLem (inl (<openclosed (inl lt))) r p = inl (<ℜ-trans p lt)
withinIntervalLem (inl (<openclosed (inr refl))) r p = inl p
withinIntervalLem (inl (<closedopen lt1)) r (inl lt2) = <ℜ-trans lt2 lt1
withinIntervalLem (inl (<closedopen lt)) r (inr refl) = lt
withinIntervalLem (inr refl) r p = p

------------------------------------------------------

CompareIvl : Interval → Interval → Set
CompareIvl = OrdCompare _<I_

compareIvl : (i j : Interval) → CompareIvl i j
compareIvl (closedIvl x) (closedIvl y) with compareℜ x y
compareIvl (closedIvl .y) (closedIvl y) | refl   = refl
compareIvl (closedIvl x)  (closedIvl y) | less p = less (<closed p)
compareIvl (closedIvl x)  (closedIvl y) | more p = more (<closed p)
compareIvl (closedIvl x) (openIvl y) with compareℜ x (y >0)
compareIvl (closedIvl .(y >0)) (openIvl y) | refl   = more <openclosed-refl
compareIvl (closedIvl x) (openIvl y)       | less p = less (<closedopen p)
compareIvl (closedIvl x) (openIvl y)       | more p = more (<openclosed (inl p))
compareIvl (openIvl x)   (closedIvl y) with compareℜ (x >0) y
compareIvl (openIvl x) (closedIvl .(x >0)) | refl   = less <openclosed-refl
compareIvl (openIvl x) (closedIvl y)       | less p = less (<openclosed (inl p))
compareIvl (openIvl x) (closedIvl y)       | more p = more (<closedopen p)
compareIvl (openIvl x)  (openIvl y) with compareℜ⁺ x y
compareIvl (openIvl .y) (openIvl y) | refl   = refl
compareIvl (openIvl x)  (openIvl y) | less p = less (<open p)
compareIvl (openIvl x)  (openIvl y) | more p = more (<open p)

------------------------------------------------------