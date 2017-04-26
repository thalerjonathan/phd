{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module Decoupled where

----------------------------------------------------------------------

-- Dec is isomorphic to Bool

data Dec : Set where
  cau : Dec
  dec : Dec

----------------------------------------------------------------------

infixr 5  _∨_
infixr 6  _∧_

_∨_ : Dec → Dec → Dec
dec ∨ d  = dec
cau ∨ d  = d

_∧_ : Dec → Dec → Dec
dec ∧ d  = d
cau ∧ d  = cau

----------------------------------------------------------------------

∨-assoc : {b c : Dec} → (a : Dec) → (a ∨ b) ∨ c ≡ a ∨ b ∨ c
∨-assoc cau = refl
∨-assoc dec = refl

∨-comm : {b : Dec} → (a : Dec) → a ∨ b ≡ b ∨ a
∨-comm {cau} cau = refl
∨-comm {dec} cau = refl
∨-comm {cau} dec = refl
∨-comm {dec} dec = refl

∨-idem : {b : Dec} → b ∨ b ≡ b
∨-idem {cau} = refl
∨-idem {dec} = refl

∨-split : {a b : Dec} → a ∨ b ≡ cau → a ≡ cau × b ≡ cau
∨-split {cau} eq = refl , eq
∨-split {dec} ()

∨-split2 : {a b c : Dec} → a ∨ b ∨ c ≡ cau → a ≡ cau × b ≡ cau × c ≡ cau
∨-split2 = second ∨-split ∘ ∨-split


∧-assoc : {b c : Dec} → (a : Dec) → (a ∧ b) ∧ c ≡ a ∧ b ∧ c
∧-assoc cau = refl
∧-assoc dec = refl

∧-comm : {b : Dec} → (a : Dec) → a ∧ b ≡ b ∧ a
∧-comm {cau} cau = refl
∧-comm {dec} cau = refl
∧-comm {cau} dec = refl
∧-comm {dec} dec = refl 

∧-idem : {b : Dec} → b ∧ b ≡ b
∧-idem {cau} = refl
∧-idem {dec} = refl

∧-split : {a b : Dec} → a ∧ b ≡ dec → a ≡ dec × b ≡ dec
∧-split {cau} ()
∧-split {dec} eq = refl , eq

∧-split2 : {a b c : Dec} → a ∧ b ∧ c ≡ dec → a ≡ dec × b ≡ dec × c ≡ dec
∧-split2 = second ∧-split ∘ ∧-split

∧∨-distr : {b c : Dec} → (a : Dec) → a ∧ (b ∨ c) ≡ a ∧ b ∨ a ∧ c
∧∨-distr cau = refl
∧∨-distr dec = refl

import CommSemiRing
module DecProps = CommSemiRing _∨_ (λ {b} → ∨-assoc b) (λ {b} → ∨-comm b) cau refl _∧_ (λ {b} →  ∧-assoc b) (λ {b} → ∧-comm b) refl dec refl (λ {b} → ∧∨-distr b)
