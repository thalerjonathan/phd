{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude


module RingUnity {A : Set}

       (_⊕_ : A -> A -> A)
       (⊕assoc : {b c : A} -> (a : A) -> (a ⊕ b) ⊕ c ≡ a ⊕ (b ⊕ c))
       (⊕comm : {b : A} -> (a : A) -> a ⊕ b ≡ b ⊕ a)
       (zero : A) (unit⊕ : {a : A} -> zero ⊕ a ≡ a)
       (inverse : A -> A) (⊕inverse : {a : A} -> a ⊕ inverse a ≡ zero)

       (_⊗_ : A -> A -> A)
       (⊗assoc : {b c : A} -> (a : A) -> (a ⊗ b) ⊗ c ≡ a ⊗ (b ⊗ c))
       (one : A) (unit⊗ : {a : A} -> one ⊗ a ≡ a) (⊗unit : {a : A} -> a ⊗ one ≡ a)

       (distrL : {b c : A} -> (a : A) -> a ⊗ (b ⊕ c) ≡ (a ⊗ b) ⊕ (a ⊗ c))
       (distrR : {b c : A} -> (a : A) -> (a ⊕ b) ⊗ c ≡ (a ⊗ c) ⊕ (b ⊗ c))

  where

import Ring
open module Rin = Ring _⊕_ ⊕assoc ⊕comm zero unit⊕ inverse ⊕inverse _⊗_ ⊗assoc distrL distrR public

import SemiRingUnity
open module SRingU = SemiRingUnity _⊕_ ⊕assoc ⊕comm zero unit⊕ _⊗_ ⊗assoc zero⊗ ⊗zero one unit⊗ ⊗unit distrL distrR


