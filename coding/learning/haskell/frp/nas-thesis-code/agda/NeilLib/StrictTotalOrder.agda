{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module StrictTotalOrder {A : Set} (_<_ : Rel A)

                                  (transit : Transitive _<_)
                                  (trich : Trichotomous _<_)

where

irreflex : Irreflexive _<_
irreflex = trichcases (λ _ _ → id) (λ _ _ → id) (\_ f _ → f) trich

asym : Asymmetric _<_
asym ab ba = trichcases (\_ abf _ → abf ab) (\_ _ baf → baf ba) (\_ abf _ → abf ab) trich

import StrictPartialOrder
open StrictPartialOrder _<_ irreflex asym transit public

totality : Total _≤_
totality = trichcases (λ eq _ _ → inlr eq) (λ _ lt _ → inll lt) (λ _ _ → inrl) trich

---------------------------------------------------------------

data OrdCompare : A → A → Set where
  refl : {a : A}           → OrdCompare a a
  less : {a b : A} → a < b → OrdCompare a b
  more : {a b : A} → b < a → OrdCompare a b

data OrdCompareLeq (a b : A) : Set where
  leq  : a ≤ b → OrdCompareLeq a b
  more : a > b → OrdCompareLeq a b

data OrdCompareGeq (a b : A) : Set where
  less : a < b → OrdCompareGeq a b
  geq  : a ≥ b → OrdCompareGeq a b

---------------------------------------------------------------

compare : (a b : A) → OrdCompare a b
compare a b with trich {a} {b}
compare a .a | inl (refl , _)        = refl
compare a b  | inr (inl (_ , p , _)) = less p
compare a b  | inr (inr (_ , _ , p)) = more p

compareLeq : (a b : A) → OrdCompareLeq a b
compareLeq a b with compare a b
compareLeq a .a | refl   = leq (inr refl)
compareLeq a b  | less p = leq (inl p)
compareLeq a b  | more p = more p

compareGeq : (a b : A) → OrdCompareGeq a b
compareGeq a b with compare a b
compareGeq a .a | refl   = geq (inr refl)
compareGeq a b  | less p = less p
compareGeq a b  | more p = geq (inl p)

compareEq : (a b : A) → CompareEq a b
compareEq a b with compare a b
compareEq a .a | refl   = refl
compareEq a b  | less p = neq (lem-less→Neq p)
compareEq a b  | more p = neq (lem-more→Neq p)

simpleCompare : (a b : A) → SimpleCompare a b
simpleCompare a b with compare a b
simpleCompare a .a | refl   = refl
simpleCompare a b  | less p = neq
simpleCompare a b  | more p = neq

---------------------------------------------------------------

ifleq_≤_thenleq_elsemore_ : {X : Set} → (a b : A) → (a ≤ b → X) → ((a > b) → X) → X
ifleq a ≤ b thenleq xt elsemore xe with compareLeq a b
... | leq  p = xt p
... | more p = xe p

ifgeq_≥_thengeq_elseless_ : {X : Set} → (a b : A) → (a ≥ b → X) → ((a < b) → X) → X
ifgeq a ≥ b thengeq xt elseless xe with compareGeq a b
... | geq  p = xt p
... | less p = xe p

ifless_<_thenless_elsegeq_ : {X : Set} → (a b : A) → (a < b → X) → ((a ≥ b) → X) → X
ifless a < b thenless xt elsegeq xe with compareGeq a b
... | less p = xt p
... | geq  p = xe p

ifmore_>_thenmore_elseleq_ : {X : Set} → (a b : A) → (a > b → X) → ((a ≤ b) → X) → X
ifmore a > b thenmore xt elseleq xe with compareLeq a b
... | more p = xt p
... | leq  p = xe p

---------------------------------------------------------------

lem-LeqEqGeq : {b : A} → {P : A → Set} → ((a : A) → (a < b) → P a) → P b → ((a : A) → (a > b) → P a) → ((a : A) → P a)
lem-LeqEqGeq {b} p eq q a with compare a b
lem-LeqEqGeq p eq q a | refl    = eq
lem-LeqEqGeq p eq q a | less lt = p a lt
lem-LeqEqGeq p eq q a | more gt = q a gt

---------------------------------------------------------------

_<='_ : A → A → Bool
a <=' b with compareLeq a b
... | leq _  = true
... | more _ = false

_>='_ : A → A → Bool
a >=' b = b <=' a

open import Bool

_<'_ : A → A → Bool
a <' b = not (b <=' a)

_>'_ : A → A → Bool
a >' b = b <' a

---------------------------------------------------------------
