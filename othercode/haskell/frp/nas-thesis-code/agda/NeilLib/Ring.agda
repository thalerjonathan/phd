{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude


module Ring {A : Set}

       (_⊕_ : A -> A -> A)
       (⊕assoc : {b c : A} -> (a : A) -> (a ⊕ b) ⊕ c ≡ a ⊕ (b ⊕ c))
       (⊕comm : {b : A} -> (a : A) -> a ⊕ b ≡ b ⊕ a)
       (zero : A) (unit⊕ : {a : A} -> zero ⊕ a ≡ a)
       (inverse : A -> A) (⊕inverse : {a : A} -> a ⊕ inverse a ≡ zero)

       (_⊗_ : A -> A -> A)
       (⊗assoc : {b c : A} -> (a : A) -> (a ⊗ b) ⊗ c ≡ a ⊗ (b ⊗ c))

       (distrL : {b c : A} -> (a : A) -> a ⊗ (b ⊕ c) ≡ (a ⊗ b) ⊕ (a ⊗ c))
       (distrR : {b c : A} -> (a : A) -> (a ⊕ b) ⊗ c ≡ (a ⊗ c) ⊕ (b ⊗ c))

  where

import CommGroup
open CommGroup _⊕_ ⊕assoc ⊕comm zero unit⊕ inverse ⊕inverse public

⊗zero : (a : A) -> a ⊗ zero ≡ zero
⊗zero a = ⊕canc (a ⊗ zero) ((distrL a ∘≡∘ comm ⊕unit) (resp2 _⊗_ refl unit⊕))

zero⊗ : {a : A} -> zero ⊗ a ≡ zero
zero⊗ {a} = ⊕canc (zero ⊗ a) ((distrR zero ∘≡∘ comm ⊕unit) (resp2 _⊗_ unit⊕ refl))

import SemiRing
open SemiRing _⊕_ ⊕assoc ⊕comm zero unit⊕ _⊗_ ⊗assoc zero⊗ ⊗zero distrL distrR


