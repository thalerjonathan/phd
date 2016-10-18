{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude
open import Nat
open import List
open import Logic

module Recursion where

-- Primitive Recursion -----------------------------------------------

listrec : {A : Set} → {P : List A → Set} → P [] → ({a : A} → {as : List A} → P as → P (a ∷ as)) → Π (List A) P
listrec base step [] = base
listrec {_} {P} base step (_ ∷ as) = step (listrec {_} {P} base step as)

listrec' : {A : Set} → {P : List A → Set} → P [] → ((a : A) → (as : List A) → P as → P (a ∷ as)) → Π (List A) P
listrec' base step = listrec base (λ {a} {as} → step a as)

-----------------------------------------------------

natrec : {P : ℕ → Set} → P O → ({m : ℕ} → P m → P (S m)) → Π ℕ P
natrec     base step O = base
natrec {P} base step (S n) = step (natrec {P} base step n)

-- Primitive Recursion on two arguments.  I'm not sure if this is the best/right way to do this, but it seems to work

natrec2 : {P : ℕ → ℕ → Set} → ((n : ℕ) → P O n) → ((m : ℕ) → P m O) → ({m n : ℕ} → P m n → P (S m) n → P m (S n) → P (S m) (S n)) → (m n : ℕ) → P m n
natrec2 baseL baseR step O n = baseL n
natrec2 baseL baseR step m O = baseR m
natrec2 {P} baseL baseR step (S m) (S n) = step (natrec2 {P} baseL baseR step m n)
                                                (natrec2 {P} baseL baseR step (S m) n)
                                                (natrec2 {P} baseL baseR step m (S n))


natrec2simpl : {P : ℕ → ℕ → Set} → ((n : ℕ) → P O n) → ((m : ℕ) → P m O) → ({m n : ℕ} → P m n → P (S m) (S n)) → (m n : ℕ) → P m n
natrec2simpl baseL baseR step O n = baseL n
natrec2simpl baseL baseR step m O = baseR m
natrec2simpl {P} baseL baseR step (S m) (S n) = step (natrec2simpl {P} baseL baseR step m n)


-- Course of Values (or Complete) Recursion {May also be called transfinite induction} ------------------------

CRec : (ℕ → Set) → ℕ → Set
CRec P O     = True
CRec P (S n) = P n × CRec P n

mutual

  comrec : {P : ℕ → Set} → ((m : ℕ) → CRec P m → P m) → Π ℕ P
  comrec step n = step n (covrec step n)
  
  covrec : {P : ℕ → Set} → ((m : ℕ) → CRec P m → P m) → Π ℕ (CRec P)
  covrec step O     = _
  covrec step (S n) = comrec step n , covrec step n


-- Complete Recursion on Lists

CReclist : {A : Set} → (List A → Set) → List A → Set
CReclist P []       = True
CReclist P (_ ∷ as) = P as × CReclist P as

mutual

  comreclist : {A : Set} → {P : List A → Set} → ((bs : List A) → CReclist P bs → P bs) → Π (List A) P
  comreclist step as = step as (covreclist step as)

  covreclist : {A : Set} → {P : List A → Set} → ((bs : List A) → CReclist P bs → P bs) → Π (List A) (CReclist P)
  covreclist step []       = _
  covreclist step (_ ∷ as) = comreclist step as , covreclist step as


-- Well Founded Recursion --------------------------------------------

data W (A : Set) (B : A → Set) : Set where
  sup : (a : A) → (B a → W A B) → W A B

wrec : {A : Set} → {B : A → Set} → (P : W A B → Set) → ((a : A) → (f : B a → W A B) → ((b : B a) → P (f b)) → P (sup a f)) → Π (W A B) P
wrec P step (sup a f) = step a f (λ b → wrec P step (f b))

initialW : {A : Set} → {B : A → Set} → Σ A (Not ∘ B) → W A B
initialW (a , nb) = sup a (absurd ∘ nb) 

terminatingW : {A : Set} → {B : A → Set} → W A B → Not (Π A B)
terminatingW (sup a fw) fb = terminatingW (fw (fb a)) fb


-- Data-type of Accessible Elements

data Acc (A : Set) (_≺_ : A → A → Set) (a : A) : Set where
  acc : ((y : A) → y ≺ a → Acc A _≺_ y) → Acc A _≺_ a

mutual

  wfrec : {A : Set} → {_≺_ : A → A → Set} → {P : A → Set} → (a : A) → ((y : A) → Acc A _≺_ y → ((z : A) → z ≺ y → P z) → P y) → Acc A _≺_ a → P a
  wfrec a step Aa = step a Aa (wfrec' step Aa)
  
  wfrec' : {A : Set} → {_≺_ : A → A → Set} → {P : A → Set} → {a : A} → ((y : A) → Acc A _≺_ y → ((z : A) → z ≺ y → P z) → P y) → Acc A _≺_ a → (z : A) → z ≺ a → P z
  wfrec' step (acc f) z za = wfrec z step (f z za)


acc-init : {A : Set} → {_≺_ : A → A → Set} → {a : A} → ((y : A) → Not (y ≺ a)) → Acc A _≺_ a
acc-init f = acc (λ y → absurd ∘ f y)


{-

-- Making more arguments implicit makes for a simpler definition
-- It matches up closer with W types now as well

data Acc' (A : Set) (_≺_ : A → A → Set) (a : A) : Set where
  acc : ({y : A} → y ≺ a → Acc' A _≺_ y) → Acc' A _≺_ a

rec : {A : Set} → {_≺_ : A → A → Set} → {P : A → Set} → {a : A} → ({y : A} → Acc' A _≺_ y → ({z : A} → z ≺ y → P z) → P y) → Acc' A _≺_ a → P a
rec step (acc f) = step (acc f) (\lt → rec step (f lt))

-}