{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module Group {A : Set}

       (_⊕_ : A -> A -> A)
       (assoc : {b c : A} -> (a : A) -> (a ⊕ b) ⊕ c ≡ a ⊕ (b ⊕ c))
       (unit : A) (unit⊕ : {a : A} -> unit ⊕ a ≡ a) (⊕unit : {a : A} -> a ⊕ unit ≡ a)
       (inverse : A -> A) (⊕inverse : {a : A} -> a ⊕ inverse a ≡ unit) (inverse⊕ : {a : A} -> inverse a ⊕ a ≡ unit)

  where

import Monoid
open Monoid _⊕_ assoc unit unit⊕ ⊕unit public

import SemiGroup
open SemiGroup _⊕_ assoc public

⊕eq' : {b c : A} -> (a : A) -> a ⊕ b ≡ a ⊕ c -> b ≡ c
⊕eq' a eq = (((resp2 _⊕_ refl (comm eq) ≡∘ (assoc (inverse a)  ∘≡ resp2 _⊕_ (comm inverse⊕) refl) ∘≡ comm unit⊕)
                                        ∘≡  assoc (inverse a)) ∘≡ resp2 _⊕_ (comm inverse⊕) refl) ∘≡ comm unit⊕

eq⊕' : {b c : A} -> (a : A) -> a ⊕ c ≡ b ⊕ c -> a ≡ b
eq⊕' {b} a eq = ((resp2 _⊕_ (comm eq) refl ≡∘ assocR b  ∘≡ resp2 _⊕_ refl (comm ⊕inverse) ∘≡ comm ⊕unit)
                                           ∘≡ assocR a) ∘≡ resp2 _⊕_ refl (comm ⊕inverse) ∘≡ comm ⊕unit

import Cancellative
open Cancellative _⊕_ ⊕eq' eq⊕' public