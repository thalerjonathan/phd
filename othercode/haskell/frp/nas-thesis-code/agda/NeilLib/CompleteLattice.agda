{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module CompleteLattice {A : Set}

                    (_≤_ : A → A → Set)

                    (_⊔_ : A → A → A)
                    (_⊓_ : A → A → A)

                    (⊥ : A)
                    (⊤ : A)

                    (reflex   : {a : A} → a ≤ a)
                    (antisym  : {a b : A} → a ≤ b → b ≤ a → a ≡ b)
                    (transit  : {a b c : A} → a ≤ b → b ≤ c → a ≤ c)

                    (supL     : {a b : A} → a ≤ (a ⊔ b))
                    (supR     : {a b : A} → a ≤ (b ⊔ a))
                    (leastSup : {x a b : A} → a ≤ x → b ≤ x → (a ⊔ b) ≤ x)

                    (infL     : {a b : A} → (a ⊓ b) ≤ a)
                    (infR     : {a b : A} → (b ⊓ a) ≤ a)
                    (mostInf  : {x a b : A} → x ≤ a → x ≤ b → x ≤ (a ⊓ b))

                    (bottom   : {a : A} → ⊥ ≤ a)
                    (top      : {a : A} → a ≤ ⊤)

where

-- when I have time,
-- see "Lattice", "Semilattice" and "Bounded Lattice" to clean up my lattice modules

-- distributive : {a b c : A} → a ⊔ (b ⊓ c) ≡ (a ⊔ b) ⊓ (a ⊔ c)
-- distributive = {!!}

absorptive : {b a : A} → a ⊔ (a ⊓ b) ≡ a
absorptive = antisym (leastSup reflex infL) supL

import CompletePartialOrder
open module CPO-⊔ = CompletePartialOrder _≤_ _⊔_ ⊥ reflex antisym transit supL supR leastSup bottom
open module CPO-⊓ = CompletePartialOrder _≥_ _⊓_ ⊤ reflex (flip antisym) (flip transit) infL infR mostInf top
