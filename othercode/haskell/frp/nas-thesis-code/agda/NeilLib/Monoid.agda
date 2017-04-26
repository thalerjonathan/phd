{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module Monoid {A : Set}

       (_⊕_    : Op A)
       (assoc  : Associative _⊕_)
       (unit   : A)
       (unit⊕' : {a : A} → unit ⊕ a ≡ a)
       (⊕unit' : {a : A} → a ⊕ unit ≡ a)

  where

import SemiGroup
open module SG = SemiGroup _⊕_ assoc

unit⊕ : {a : A} → unit ⊕ a ≡ a
unit⊕ = unit⊕'

⊕unit : {a : A} → a ⊕ unit ≡ a
⊕unit = ⊕unit'

⊕⊕unit : {a b : A} → a ⊕ (b ⊕ unit) ≡ a ⊕ b
⊕⊕unit = trans assocR ⊕unit

unit⊕⊕ : {a b : A} → (unit ⊕ a) ⊕ b ≡ a ⊕ b
unit⊕⊕ = trans assoc unit⊕

