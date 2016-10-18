{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module CommSemiGroup {A : Set}

       (_⊕_   : Op A)
       (assoc : Associative _⊕_)
       (comm' : Commutative _⊕_)

where

import SemiGroup
open SemiGroup _⊕_ assoc public

comm : Commutative _⊕_
comm = comm'

-- ⊕commR : {a : A} → (b : A) → a ⊕ b ≡ b ⊕ a
-- ⊕commR b = comm (⊕comm b)

ascomR : {a b c : A} → (a ⊕ b) ⊕ c ≡ (a ⊕ c) ⊕ b
ascomR = trans2 assoc (cong2L _⊕_ comm) assocR

ascomL : {a b c : A} → a ⊕ (b ⊕ c) ≡ b ⊕ (a ⊕ c)
ascomL = trans2 assocR (cong2R _⊕_ comm) assoc

ascomM : {a b c d : A} → (a ⊕ b) ⊕ (c ⊕ d) ≡ (a ⊕ c) ⊕ (b ⊕ d)
ascomM = trans2 assoc (cong2L _⊕_ ascomL) assocR

ascomRL : {a b c : A} → a ⊕ (b ⊕ c) ≡ b ⊕ (c ⊕ a)
ascomRL = trans comm assoc