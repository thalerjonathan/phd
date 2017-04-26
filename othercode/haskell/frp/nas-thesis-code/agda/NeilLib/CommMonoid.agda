{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module CommMonoid {A : Set}

       (_⊕_ : Op A)
       (assoc : Associative _⊕_)
       (comm : Commutative _⊕_)
       (unit : A) (unit⊕ : {a : A} -> unit ⊕ a ≡ a)
  where

⊕unit' : {a : A} -> a ⊕ unit ≡ a
⊕unit' {a} = trans comm unit⊕

import Monoid
open Monoid _⊕_ assoc unit unit⊕ ⊕unit' public

import CommSemiGroup
open CommSemiGroup _⊕_ assoc comm public
