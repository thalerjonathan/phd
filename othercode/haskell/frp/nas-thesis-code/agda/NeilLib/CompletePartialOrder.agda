{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module CompletePartialOrder {A : Set}

                    (_≤_ : A → A → Set)

                    (_⊔_ : A → A → A)

                    (⊥ : A)

                    (reflex   : {a : A} → a ≤ a)
                    (antisym  : {a b : A} → a ≤ b → b ≤ a → a ≡ b)
                    (transit  : {a b c : A} → a ≤ b → b ≤ c → a ≤ c)

                    (supL     : {a b : A} → a ≤ (a ⊔ b))
                    (supR     : {a b : A} → a ≤ (b ⊔ a))

                    (leastSup : {x a b : A} → a ≤ x → b ≤ x → (a ⊔ b) ≤ x)

                    (bottom   : {a : A} → ⊥ ≤ a)

where

import DirectedCompletePartialOrder
open DirectedCompletePartialOrder _≤_ _⊔_ reflex antisym transit supL supR leastSup public

unitL : {a : A} → ⊥ ⊔ a ≡ a
unitL = antisym (leastSup bottom reflex) supR

import CommMonoid
open CommMonoid _⊔_ associative commutative ⊥ unitL public

≤bot : {a : A} → a ≤ ⊥ → a ≡ ⊥
≤bot p = antisym p bottom

botSplit : {a b : A} → (a ⊔ b) ≡ ⊥ → (a ≡ ⊥) × (b ≡ ⊥)
botSplit = (≤bot ∥ ≤bot) ∘ leastSupR ∘ ≡implies≤

