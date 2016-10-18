{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import Maybe

module MaybeMonad where

maybeBind : {A B : Set} → Maybe A → (A → Maybe B) → Maybe B
maybeBind nothing  f = nothing
maybeBind (just a) f = f a

maybePlus : {A : Set} → Maybe A → Maybe A → Maybe A
maybePlus nothing ma = ma
maybePlus (just a) _ = (just a)

import MonadPlus
open MonadPlus Maybe maybeBind just maybePlus nothing public

mm2ndLaw : {A : Set} → {ma : Maybe A} → maybeBind ma just ≡ ma
mm2ndLaw {_} {nothing} = refl
mm2ndLaw {_} {just _} = refl

mm3rdLaw : {A B C : Set} → {f : A → Maybe B} → {g : B → Maybe C} → (ma : Maybe A)
            → maybeBind (maybeBind ma f) g ≡ maybeBind ma (\a → maybeBind (f a) g)
mm3rdLaw nothing = refl
mm3rdLaw (just a) = refl

mm5thLaw : {A B : Set} → (ma : Maybe A) → maybeBind {A} {B} ma (\_ → nothing) ≡ nothing
mm5thLaw nothing  = refl
mm5thLaw (just _) = refl

mm7thLaw : {A : Set} → {ma : Maybe A} → maybePlus ma nothing ≡ ma
mm7thLaw {_} {nothing} = refl
mm7thLaw {_} {just _} = refl

import MonadPlusProps
open MonadPlusProps Maybe maybeBind just maybePlus nothing refl mm2ndLaw mm3rdLaw refl mm5thLaw refl mm7thLaw public

