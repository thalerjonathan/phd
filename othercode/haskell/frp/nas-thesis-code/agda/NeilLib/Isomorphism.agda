{-# OPTIONS --type-in-type
    #-}

-- Some isomorphic properties can be found in Functor

open import NeilPrelude

module Isomorphism where

≅refl : {A : Set} → A ≅ A
≅refl = (id & id) & refl & refl

≅symm : {A B : Set} → A ≅ B → B ≅ A
≅symm ((f & f⁻¹) & p & q) = (f⁻¹ & f) & (λ {_} → q) & λ {_} → p

≅trans : {A B C : Set} → A ≅ B → B ≅ C → A ≅ C
≅trans ((f & f⁻¹) & p & q)  ((g & g⁻¹) & r & s) = (g ∘ f & f⁻¹ ∘ g⁻¹) & (λ {_} → r ∘≡ resp g   p)
                                                                      & (λ {_} → q ∘≡ resp f⁻¹ s)

import Equivalence
open Equivalence _≅_ ≅refl ≅symm ≅trans public

-------------------------------------------------------------------------------------------------

unitAux : {A : Set} → {f : Unit → A} → (u : Unit) → f unit ≡ f u
unitAux unit = refl

≅UnitA : {A : Set} → Extensionality → (Unit → A) ≅ A
≅UnitA ext = (applyTo unit & const) & refl & (λ {_} → ext unitAux)

-------------------------------------------------------------------------------------------------
