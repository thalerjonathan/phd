{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

-- ERROR! Division not prperly defined
-- Zero should be an annihilator!
-- but it may follow from the other axioms

module Field {A : Set}

       (_⊕_ : A → A → A)
       (⊕assoc : {b c : A} → (a : A) → (a ⊕ b) ⊕ c ≡ a ⊕ (b ⊕ c))
       (⊕comm : {b : A} → (a : A) → a ⊕ b ≡ b ⊕ a)
       (zero : A) (unit⊕ : {a : A} → zero ⊕ a ≡ a)
       (minus : A → A) (⊕inverse : {a : A} → a ⊕ minus a ≡ zero)

       (_⊗_ : A → A → A)
       (⊗assoc : {b c : A} → (a : A) → (a ⊗ b) ⊗ c ≡ a ⊗ (b ⊗ c))
       (⊗comm : {b : A} → (a : A) → a ⊗ b ≡ b ⊗ a)
       (one : A) (unit⊗ : {a : A} → one ⊗ a ≡ a)
       (divide : A → A) (⊗inverse : {a : A} → a ⊗ divide a ≡ one)

       (distrL : {b c : A} → (a : A) → a ⊗ (b ⊕ c) ≡ (a ⊗ b) ⊕ (a ⊗ c))

  where

inverse⊗ : {a : A} → divide a ⊗ a ≡ one
inverse⊗ {a} = ⊗inverse ∘≡ ⊗comm (divide a)

import CommRing
open module CRing = CommRing _⊕_ ⊕assoc ⊕comm zero unit⊕ minus ⊕inverse _⊗_ ⊗assoc ⊗comm one unit⊗ distrL public

import DivisionRing
open module DRing = DivisionRing _⊕_ ⊕assoc ⊕comm zero unit⊕ minus ⊕inverse _⊗_ ⊗assoc one unit⊗ ⊗unit divide ⊗inverse inverse⊗ distrL distrR
