{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude


module CommRing {A : Set}

       (_⊕_ : A -> A -> A)
       (⊕assoc : {b c : A} -> (a : A) -> (a ⊕ b) ⊕ c ≡ a ⊕ (b ⊕ c))
       (⊕comm : {b : A} -> (a : A) -> a ⊕ b ≡ b ⊕ a)
       (zero : A) (unit⊕ : {a : A} -> zero ⊕ a ≡ a)
       (inverse : A -> A) (⊕inverse : {a : A} -> a ⊕ inverse a ≡ zero)

       (_⊗_ : A -> A -> A)
       (⊗assoc : {b c : A} -> (a : A) -> (a ⊗ b) ⊗ c ≡ a ⊗ (b ⊗ c))
       (⊗comm : {b : A} -> (a : A) -> a ⊗ b ≡ b ⊗ a)
       (one : A) (unit⊗ : {a : A} -> one ⊗ a ≡ a)

       (distrL : {b c : A} -> (a : A) -> a ⊗ (b ⊕ c) ≡ (a ⊗ b) ⊕ (a ⊗ c))
  where

-- copied from CommSemiring as we can't have mutually recursive modules

⊗unit' : {a : A} -> a ⊗ one ≡ a
⊗unit' {a} = unit⊗ ∘≡ ⊗comm a

distrR' : {b c : A} -> (a : A) -> (a ⊕ b) ⊗ c ≡ (a ⊗ c) ⊕ (b ⊗ c)
distrR' {b} {c} a = (resp2 _⊕_ (⊗comm c) (⊗comm c) ∘≡ distrL c) ∘≡ ⊗comm (a ⊕ b)

--

import RingUnity
open module RingU = RingUnity _⊕_ ⊕assoc ⊕comm zero unit⊕ inverse ⊕inverse _⊗_ ⊗assoc one unit⊗ ⊗unit' distrL distrR'

import CommSemiRing
open module CSR = CommSemiRing _⊕_ ⊕assoc ⊕comm zero unit⊕ _⊗_ ⊗assoc ⊗comm zero⊗ one unit⊗ distrL public


