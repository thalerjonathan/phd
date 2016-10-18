{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import PosReal renaming (_₀+₀_ to _+_; _≤ℜ₀_ to _≤_)

module PseudoMetricSpace

  {A : Set}
  (d : A → A → ℜ₀)  -- distance function

  (reflexivity : {a : A} → d a a ≡ O)                    -- reflexivity
  (symmetry     : {a b : A} → d a b ≡ d b a)             -- symmetry
  (tri-ineq     : {a b c : A} → d a c ≤ (d a b + d b c)) -- triangle inequality

where

