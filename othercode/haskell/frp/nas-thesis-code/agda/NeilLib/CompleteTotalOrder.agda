{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module CompleteTotalOrder {A : Set}

                    (_≤_ : A → A → Set)

                    (_⊔_ : A → A → A)

                    (⊥ : A)

                    (antisym  : {a b : A} → a ≤ b → b ≤ a → a ≡ b)
                    (transit  : {a b c : A} → a ≤ b → b ≤ c → a ≤ c)
                    (total   : {a b : A} → (a ≤ b) ⊎ (b ≤ a))

                    (supL     : {a b : A} → a ≤ (a ⊔ b))
                    (supR     : {a b : A} → a ≤ (b ⊔ a))

                    (leastSup : {x a b : A} → a ≤ x → b ≤ x → (a ⊔ b) ≤ x)

                    (bottom   : {a : A} → ⊥ ≤ a)

where

import TotalOrder
open TotalOrder _≤_ antisym transit total

import CompletePartialOrder
open CompletePartialOrder _≤_ _⊔_ ⊥ reflexive antisym transit supL supR leastSup bottom public
