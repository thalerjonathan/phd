{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module StateMonad (S : Set) where

State : Set → Set
State A = S → (S × A)

sm2ndLaw' : {A B : Set} → (ab : A × B) → (uncurry (flip _&_) ∘ swap) ab ≡ ab
sm2ndLaw' (a & b) = refl

sm2ndLaw : {A : Set} → {ma : State A} → Extensionality → uncurry (flip _&_) ∘ swap ∘ ma ≡ ma
sm2ndLaw {_} {ma} ext = ext (sm2ndLaw' ∘' ma)

sm3rdLaw' : {A B C : Set} → {f : A → State B} → {g : B → State C} → (ma : State A)
            → (s : S) → (uncurry g ∘ swap ∘ uncurry f ∘ swap ∘ ma) s ≡ (uncurry (\b → uncurry g ∘ swap ∘ f b) ∘ swap ∘ ma) s
sm3rdLaw' ma s with ma s
... | l & r = refl

sm3rdLaw : {A B C : Set} → {f : A → State B} → {g : B → State C} → Extensionality
           → (ma : State A) → uncurry g ∘ swap ∘ uncurry f ∘ swap ∘ ma ≡ uncurry (\ b → uncurry g ∘ swap ∘ f b) ∘ swap ∘ ma
sm3rdLaw ext ma = ext (sm3rdLaw' ma)

import Monad
open Monad State (\ma f → uncurry f ∘ swap ∘ ma) (flip _&_) refl sm2ndLaw sm3rdLaw public

get : State S
get = fork

put : S → State Unit
put s _ = s & unit

modify : (S → S) → State Unit
modify f = get >>= put ∘ f

