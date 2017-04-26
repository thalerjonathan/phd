{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module Dependency {A : Set} (_∼_ : A → A → Set)

                    (reflex    : {a : A} → a ∼ a)
                    (symmetric : {a b : A} → a ∼ b → b ∼ a)
where
