{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module Interval (A : Set) (_<_ : Rel A) where

data Interval : Set where
  ⟨_,_⟩ : A → A → Interval
  ⟨_,_] : A → A → Interval
  [_,_⟩ : A → A → Interval
  [_,_] : A → A → Interval

private _≤_ : Rel A
        a ≤ b = (a < b) ⊎ (a ≡ b)

_∈_ : A → Interval → Set
t ∈ ⟨ t₁ , t₂ ⟩ = (t₁ < t) × (t < t₂)
t ∈ ⟨ t₁ , t₂ ] = (t₁ < t) × (t ≤ t₂)
t ∈ [ t₁ , t₂ ⟩ = (t₁ ≤ t) × (t < t₂)
t ∈ [ t₁ , t₂ ] = (t₁ ≤ t) × (t ≤ t₂)

leftBound : Interval → A
leftBound ⟨ a , _ ⟩ = a
leftBound ⟨ a , _ ] = a
leftBound [ a , _ ⟩ = a
leftBound [ a , _ ] = a

rightBound : Interval → A
rightBound ⟨ _ , a ⟩ = a
rightBound ⟨ _ , a ] = a
rightBound [ _ , a ⟩ = a
rightBound [ _ , a ] = a