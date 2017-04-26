{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude


module SemiRingUnity {A : Set}

       (_⊕_ : Op A)
       (⊕assoc : Associative _⊕_)
       (⊕comm : Commutative _⊕_)
       (zero : A) (unit⊕ : {a : A} → zero ⊕ a ≡ a)

       (_⊗_ : Op A)
       (⊗assoc : Associative _⊗_)
       (zero⊗ : {a : A} → zero ⊗ a ≡ zero) (⊗zero : {a : A} → a ⊗ zero ≡ zero)
       (one : A) (unit⊗ : {a : A} -> one ⊗ a ≡ a) (⊗unit : {a : A} -> a ⊗ one ≡ a)

       (distrL : {a b c : A} -> a ⊗ (b ⊕ c) ≡ (a ⊗ b) ⊕ (a ⊗ c))
       (distrR : {a b c : A} -> (a ⊕ b) ⊗ c ≡ (a ⊗ c) ⊕ (b ⊗ c))
  where

import SemiRing
open SemiRing _⊕_ ⊕assoc ⊕comm zero unit⊕ _⊗_ ⊗assoc zero⊗ ⊗zero distrL distrR public

import Monoid
open Monoid _⊗_ ⊗assoc one