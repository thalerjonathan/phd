{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude


module DivisionRing {A : Set}

       (_⊕_ : A -> A -> A)
       (⊕assoc : {b c : A} -> (a : A) -> (a ⊕ b) ⊕ c ≡ a ⊕ (b ⊕ c))
       (⊕comm : {b : A} -> (a : A) -> a ⊕ b ≡ b ⊕ a)
       (zero : A) (unit⊕ : {a : A} -> zero ⊕ a ≡ a)
       (minus : A -> A) (⊕inverse : {a : A} -> a ⊕ minus a ≡ zero)

       (_⊗_ : A -> A -> A)
       (⊗assoc : {b c : A} -> (a : A) -> (a ⊗ b) ⊗ c ≡ a ⊗ (b ⊗ c))
       (one : A) (unit⊗ : {a : A} -> one ⊗ a ≡ a) (⊗unit : {a : A} -> a ⊗ one ≡ a)
       (divide : A -> A) (⊗inverse : {a : A} -> a ⊗ divide a ≡ one) (inverse⊗ : {a : A} -> divide a ⊗ a ≡ one)

       (distrL : {b c : A} -> (a : A) -> a ⊗ (b ⊕ c) ≡ (a ⊗ b) ⊕ (a ⊗ c))
       (distrR : {b c : A} -> (a : A) -> (a ⊕ b) ⊗ c ≡ (a ⊗ c) ⊕ (b ⊗ c))

  where


import RingUnity
open module RingU = RingUnity  _⊕_ ⊕assoc ⊕comm zero unit⊕ minus ⊕inverse _⊗_ ⊗assoc one unit⊗ ⊗unit distrL distrR public