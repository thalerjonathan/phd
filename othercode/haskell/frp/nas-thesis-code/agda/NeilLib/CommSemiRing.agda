{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude


module CommSemiRing {A : Set}


       (_⊕_ : Op A)
       (⊕assoc : Associative _⊕_)
       (⊕comm : Commutative _⊕_)
       (zero : A) (unit⊕ : {a : A} → zero ⊕ a ≡ a)

       (_⊗_ : Op A)
       (⊗assoc : Associative _⊗_)
       (⊗comm : Commutative _⊗_)
       (zero⊗ : {a : A} → zero ⊗ a ≡ zero)
       (one : A) (unit⊗ : {a : A} → one ⊗ a ≡ a)

       (distrL : {a b c : A} → a ⊗ (b ⊕ c) ≡ (a ⊗ b) ⊕ (a ⊗ c))
  where

import CommMonoid
open CommMonoid _⊗_ ⊗assoc ⊗comm one

⊗zero : {a : A} → a ⊗ zero ≡ zero
⊗zero = trans ⊗comm zero⊗

⊗unit : {a : A} → a ⊗ one ≡ a
⊗unit = ⊕unit unit⊗

distrR : {a b c : A} → (a ⊕ b) ⊗ c ≡ (a ⊗ c) ⊕ (b ⊗ c)
distrR = trans2 ⊗comm distrL (cong2 _⊕_ ⊗comm ⊗comm)

import SemiRingUnity
open SemiRingUnity _⊕_ ⊕assoc ⊕comm zero unit⊕ _⊗_ ⊗assoc zero⊗ ⊗zero one unit⊗ ⊗unit distrL distrR public

