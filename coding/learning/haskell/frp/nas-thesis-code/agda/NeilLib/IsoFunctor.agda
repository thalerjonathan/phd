{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude
open import Isomorphism

module IsoFunctor (F : Set → Set) (fmap : {A B : Set} → (A → B) → F A → F B)

       (1stLaw : {A : Set} → {fa : F A} → Extensionality → fmap id fa ≡ fa)

       (2ndLaw : {A B C : Set} → {f : A → B} → {g : B → C} → {fa : F A} → Extensionality → fmap (g ∘ f) fa ≡ (fmap g ∘ fmap f) fa)
                  

where

import Functor
open Functor F fmap 1stLaw 2ndLaw public

fmapinverse : {A B : Set} → {f : A → B} → {f⁻¹ : B → A} → Extensionality → ({a : A} → f⁻¹ (f a) ≡ a) → ({fa : F A} → fmap f⁻¹ (fmap f fa) ≡ fa)
fmapinverse ext finv = 1stLaw ext ∘≡ resp2 fmap (ext (λ _ → finv)) refl ∘≡ comm (2ndLaw ext)

≅resp : {A B : Set} → Extensionality → A ≅ B → F A ≅ F B
≅resp ext ((f & f⁻¹) & p & q) = (fmap f & fmap f⁻¹) & (λ {_} → fmapinverse ext (λ {_} → p))
                                                    & (λ {_} → fmapinverse ext (λ {_} → q))

≅subst : {A B : Set} → A ≅ B → F A → F B
≅subst = fmap ∘ fst ∘ fst
