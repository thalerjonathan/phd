{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module SemiRing {A : Set}

       (_⊕_ : Op A)
       (⊕assoc : Associative _⊕_)
       (⊕comm : Commutative _⊕_)
       (zero : A) (unit⊕ : {a : A} → zero ⊕ a ≡ a)

       (_⊗_ : Op A)
       (⊗assoc : Associative _⊗_)
       (zero⊗ : {a : A} → zero ⊗ a ≡ zero) (⊗zero : {a : A} → a ⊗ zero ≡ zero)

       (distrL : {a b c : A} → a ⊗ (b ⊕ c) ≡ (a ⊗ b) ⊕ (a ⊗ c))
       (distrR : {a b c : A} → (a ⊕ b) ⊗ c ≡ (a ⊗ c) ⊕ (b ⊗ c))
  where

distr : {a b c : A} → a ⊗ (b ⊕ c) ≡ (a ⊗ b) ⊕ (a ⊗ c)
distr = distrL

import CommMonoid
open CommMonoid _⊕_ ⊕assoc ⊕comm zero unit⊕ public

import SemiGroup
open SemiGroup _⊗_ ⊗assoc

lem-[a⊕0]⊗b=a⊗b : {a b : A} → (a ⊕ zero) ⊗ b ≡ a ⊗ b
lem-[a⊕0]⊗b=a⊗b = cong2R _⊗_ ⊕unit
