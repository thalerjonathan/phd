{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import PosReal renaming (_₀+₀_ to _+_; _≤ℜ₀_ to _≤_)

-- In an ultrametric space all triangles are isosceles,
-- and furthermore, the two equal edges are at least the length of shorter edge

module UltraMetricSpace

  {A : Set}
  (d : A → A → ℜ₀)  -- distance function

  (indiscernable : {a b : A} → d a b ≡ O ↔ a ≡ b)                    -- identity of indiscernables
  (symmetry      : Comm d)                                           -- symmetry
  (ultra-ineq    : {a b c : A} → (d a c ≤ d a b) ⊎ (d a c ≤ d b c))  -- ultrametric inequality
  -- alternatively d a c ≤ max (d a b , d b c)

where

tri-ineq : {a b c : A} → d a c ≤ (d a b + d b c)
tri-ineq {a} {b} {c} with ultra-ineq {a} {b} {c}
... | inl p = ≤ℜ₀-trans p (lem-ℜ₀-+-≤-increasingR _)
... | inr p = ≤ℜ₀-trans p (lem-ℜ₀-+-≤-increasing (d a b))

import MetricSpace
open MetricSpace d indiscernable symmetry tri-ineq public
