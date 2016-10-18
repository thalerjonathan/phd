{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import PosReal renaming (_₀+₀_ to _+_; _≤ℜ₀_ to _≤_; _≥ℜ₀_ to _≥_; _<ℜ₀_ to _<_; _>ℜ₀_ to _>_; _⁺*₀_ to _*_)

------------------------------------------------------------------------------------------

module MetricSpace

  {A : Set}
  (d : A → A → ℜ₀)  -- distance function

  (indiscernable : {a b : A} → d a b ≡ O ↔ a ≡ b)         -- identity of indiscernables
  (symmetry      : {a b : A} → d a b ≡ d b a)             -- symmetry
  (tri-ineq      : {a b c : A} → d a c ≤ (d a b + d b c)) -- triangle inequality

where

reflexivity : {a : A} → d a a ≡ O
reflexivity = snd indiscernable refl

------------------------------------------------------------------------------------------

import PseudoMetricSpace
open PseudoMetricSpace d reflexivity symmetry tri-ineq public

------------------------------------------------------------------------------------------

WeaklyExpansive : (f : A → A) → Set
WeaklyExpansive f = (a b : A) → d (f a) (f b) > d a b

WeaklyContractive : (f : A → A) → Set
WeaklyContractive f = (a b : A) → d (f a) (f b) < d a b

NonExpansive : (f : A → A) → Set
NonExpansive f = (a b : A) → d (f a) (f b) ≤ d a b

NonContractive : (f : A → A) → Set
NonContractive f = (a b : A) → d (f a) (f b) ≥ d a b

Contractive : (f : A → A) → Set
Contractive f = Σ Δ (λ c → c <ℜ⁺ ı⁺ × ((a b : A) → d (f a) (f b) ≤ c * d a b))

Expansive : (f : A → A) → Set
Expansive f = Σ Δ (λ c → c <ℜ⁺ ı⁺ × ((a b : A) → d (f a) (f b) ≥ c * d a b))

------------------------------------------------------------------------------------------

open import Stream
open import Nat
open import NatOrd hiding (_≤_)

CauchySequence : Set
CauchySequence = Σ (Stream A) (λ s → (ε : Δ) → Σ ℕ (λ n → (i j : ℕ) → n ≤ℕ i → n ≤ℕ j → d (s !! i) (s !! j) ≤ (ε >0)))

------------------------------------------------------------------------------------------
