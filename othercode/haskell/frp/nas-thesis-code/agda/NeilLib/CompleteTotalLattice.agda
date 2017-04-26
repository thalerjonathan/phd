{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module CompleteTotalLattice {A : Set}

                    (_≤_ : A → A → Set)

                    (_⊔_ : A → A → A)
                    (_⊓_ : A → A → A)

                    (⊥ : A)
                    (⊤ : A)

                    (antisym  : {a b : A} → a ≤ b → b ≤ a → a ≡ b)
                    (transit  : {a b c : A} → a ≤ b → b ≤ c → a ≤ c)
                    (total    : {a b : A} → (a ≤ b) ⊎ (b ≤ a))

                    (supL     : {a b : A} → a ≤ (a ⊔ b))
                    (supR     : {a b : A} → a ≤ (b ⊔ a))
                    (leastSup : {x a b : A} → a ≤ x → b ≤ x → (a ⊔ b) ≤ x)

                    (infL     : {a b : A} → (a ⊓ b) ≤ a)
                    (infR     : {a b : A} → (b ⊓ a) ≤ a)
                    (mostInf  : {x a b : A} → x ≤ a → x ≤ b → x ≤ (a ⊓ b))

                    (bottom   : {a : A} → ⊥ ≤ a)
                    (top      : {a : A} → a ≤ ⊤)

where

import CompleteTotalOrder
open module CTO-⊔ = CompleteTotalOrder _≤_ _⊔_ ⊥ antisym transit total supL supR leastSup bottom
open module CTO-⊓ = CompleteTotalOrder _≥_ _⊓_ ⊤ (flip antisym) (flip transit) total infL infR mostInf top


