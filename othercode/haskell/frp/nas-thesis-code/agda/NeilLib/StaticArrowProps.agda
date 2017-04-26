{-# OPTIONS --type-in-type #-}

open import NeilPrelude hiding (result ; apply) renaming (first to first` ; second to second`)

module StaticArrowProps (Arr : Set → Set → Set)

       (arr    : {A B : Set} → (A → B) → Arr A B) 
       (_>>>_  : {A B C : Set} → Arr A B → Arr B C → Arr A C)
       (first  : {A B C : Set} → Arr A B → Arr (A × C) (B × C))
       (delay  : {A B : Set} → Arr A B → Arr Unit (A → B))
       (force  : {A B : Set} → Arr Unit (A → B) → Arr A B)

-- Arrow Laws

       (1stLaw : {A B : Set} → {f : Arr A B} → (arr id >>> f) ≡ f)
       (3rdLaw : {A B C D : Set} → {f : Arr A B} → {g : Arr B C} → {h : Arr C D} → ((f >>> g) >>> h) ≡ (f >>> (g >>> h)))
       (4thLaw : {A B C : Set} → {f : A → B} → {g : B → C} → arr (g ∘ f) ≡ arr f >>> arr g)
       (5thLaw : {A B C : Set} → {f : A → B} → first {_} {_} {C} (arr f) ≡ arr (first` f))
       (6thLaw : {A B C D : Set} → {f : Arr A B} → {g : Arr B C} → first {_} {_} {D} (f >>> g) ≡ first f >>> first g)
       (7thLaw : {A B C D : Set} → {f : Arr A B} → {g : C → D} → first f >>> arr (second` g) ≡ arr (second` g) >>> first f)
       (8thLaw : {A B C : Set} → {f : Arr A B} → first {_} {_} {C} f >>> arr fst ≡ arr fst >>> f)
       (9thLaw : {A B C D : Set} → {f : Arr A B} → first {A × C} {_} {D} (first f) >>> arr ×-assocR ≡ arr ×-assocR >>> first f)

-- Static Arrow Laws

       (1stStaticLaw : {A B : Set} → {f : Arr A B} → force (delay f) ≡ f)
       (2ndStaticLaw : {A B : Set} → {f : Arr Unit (A → B)} → delay (force f) ≡ f)

where

----------------------------------------------

import StaticArrow
open StaticArrow Arr arr _>>>_ first delay

----------------------------------------------

import ArrowProps
open ArrowProps Arr arr _>>>_ first 1stLaw 3rdLaw 4thLaw 5thLaw 6thLaw 7thLaw 8thLaw 9thLaw public

----------------------------------------------

-- All Static Arrows are Idioms (Applicative Functors)

-- fmapLaw : {A B : Set} → {g : A → B} → {a : I A} → result g a ≡ apply (constant g) a
-- fmapLaw = {!!}

-- identityLaw : {A : Set} → {a : I A} → apply (constant id) a ≡ a
-- identityLaw = {!!}

-- compositionLaw  : {A B C : Set} → {a : I A} → {fbc : I (B → C)} → {fab : I (A → B)} →
--                    apply (apply (apply (constant (_∘_)) fbc) fab) a ≡ apply fbc (apply fab a)
-- compositionLaw = {!!}

-- homomorphismLaw : {A B X : Set} → {f : A → B} → {a : A} → apply {X} (constant f) (constant a) ≡ constant (f a)
-- homomorphismLaw = {!!}

-- interchangeLaw : {A B : Set} → {fab : I (A → B)} → {a : A} → apply fab (constant a) ≡ apply (constant (λ g → g a)) fab
-- interchangeLaw = {!!}

-- import ApplicativeProps
-- open ApplicativeProps I result constant apply fmapLaw identityLaw compositionLaw homomorphismLaw interchangeLaw
