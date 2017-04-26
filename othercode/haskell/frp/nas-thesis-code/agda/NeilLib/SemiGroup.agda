{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module SemiGroup {A : Set}

       (_⊕_  : Op A)
       (asso : Associative _⊕_)

where

import Magma
open Magma _⊕_

assoc : Associative _⊕_
assoc = asso

assocR : AssociativeR _⊕_
assocR = sym assoc 

assocB : {a b c d e : A} → (a ⊕ b) ⊕ c ≡ (a ⊕ d) ⊕ e → a ⊕ (b ⊕ c) ≡ a ⊕ (d ⊕ e)
assocB eq = trans2 assocR eq assoc

assocBR : {a b c d e : A} → a ⊕ (b ⊕ c) ≡ a ⊕ (d ⊕ e) → (a ⊕ b) ⊕ c ≡ (a ⊕ d) ⊕ e
assocBR eq = trans2 assoc eq assocR

assocLL : {a b c d : A} → ((a ⊕ b) ⊕ c) ⊕ d ≡ a ⊕ (b ⊕ (c ⊕ d))
assocLL = trans assoc assoc

assocRR : {a b c d : A} → a ⊕ (b ⊕ (c ⊕ d)) ≡ ((a ⊕ b) ⊕ c) ⊕ d
assocRR = trans assocR assocR

assocRL : {a b c d : A} → a ⊕ (b ⊕ (c ⊕ d)) ≡ (a ⊕ (b ⊕ c)) ⊕ d
assocRL = trans assocRR (cong2R _⊕_ assoc)

assocLR : {a b c d : A} → (a ⊕ (b ⊕ c)) ⊕ d ≡ a ⊕ (b ⊕ (c ⊕ d))
assocLR = sym assocRL

