{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module PointedProps (F : Set → Set)

       (fmap  : {A B : Set} → (A → B) → F A → F B)
       (pure  : {A : Set} → A → F A)

-- Functor Laws
       
       (1stLaw : {A : Set} → {fa : F A} → fmap id fa ≡ fa)

       (2ndLaw : {A B C : Set} → {f : A → B} → {g : B → C} → {fa : F A} →
                 fmap (g ∘ f) fa ≡ (fmap g ∘ fmap f) fa)

-- Pointed Law

       (pointedLaw : {A B : Set} → {f : A → B} → {a : A} → fmap f (pure a) ≡ pure (f a))

where

import Pointed
open Pointed F fmap

import FunctorProps
open FunctorProps F fmap 1stLaw 2ndLaw public

------------------------------------------------------------

