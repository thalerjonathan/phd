{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module FunctorProps (F : Set → Set)

       (fmap : {A B : Set} → (A → B) → F A → F B)

-- Functor Laws
       
       (1stLaw : {A : Set} → {fa : F A} → fmap id fa ≡ fa)

       (2ndLaw : {A B C : Set} → {f : A → B} → {g : B → C} → {fa : F A} →
                 fmap (g ∘ f) fa ≡ (fmap g ∘ fmap f) fa)

where

import Functor
open Functor F fmap


fmapId : {A : Set} → {fa : F A} → fmap id fa ≡ fa
fmapId = 1stLaw

fmapId' : {A : Set} → Extensionality → fmap {A} id ≡ id
fmapId' ext = ext (λ _ → fmapId)


naturality : {A B C : Set} → {f : A → B} → {g : B → C} → {fa : F A} → fmap (g ∘ f) fa ≡ (fmap g ∘ fmap f) fa
naturality = 2ndLaw

naturality' : {A B C : Set} → {f : A → B} → {g : B → C} → Extensionality → fmap (g ∘ f) ≡ fmap g ∘ fmap f
naturality' ext = ext (λ _ → 2ndLaw)
