{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module FixedPoint where

postulate fix : {A : Set} → (A → A) → A

postulate unfold : {A : Set} → (f : A → A) → fix f ≡ f (fix f)

-- I think Fixed Point Induction is also called Scott Induction

postulate fixpoint-induction : {A : Set} → {f : A → A} → {P : A → Set} → ((a : A) → P (f a)) → P (fix f)

