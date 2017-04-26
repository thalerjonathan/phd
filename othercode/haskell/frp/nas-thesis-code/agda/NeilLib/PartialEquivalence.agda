{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module PartialEquivalence {A : Set} (_∼_ : A → A → Set)

                    (symmetric : {a b : A} → a ∼ b → b ∼ a)
                    (transit   : {a b c : A} → a ∼ b → b ∼ c → a ∼ c)

where

symmetry : {a b : A} → a ∼ b → b ∼ a
symmetry = symmetric

