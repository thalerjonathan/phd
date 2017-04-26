{-# OPTIONS --type-in-type  #-}

open import NeilPrelude
open import RealTime

module TimeOrderedList where

------------------------------------------------------------

data TimeOrderedList (A : Set) (t₀ : Time) : Set where
  []    : TimeOrderedList A t₀
  cons  : (t : Time) → A → (tas : TimeOrderedList A t) → t₀ <ℜ t → TimeOrderedList A t₀

TList : Set → Time → Set
TList = TimeOrderedList

TSList : Set → Set
TSList A = Σ Time (TList A)

------------------------------------------------------------

TimeAfter : Time → Set
TimeAfter t₀ = Σ Time (λ t₁ → t₀ <ℜ t₁)

ValAfter : Set → Time → Set
ValAfter A t = A × TimeAfter t

import OrdCompare
open OrdCompare _<ℜ_ <ℜ-irreflexive <ℜ-asym <ℜ-trans compareℜ

module BasicTListFunctions {A : Set} where

  head : {t₀ : Time} → TList A t₀ → Maybe (ValAfter A t₀)
  head [] = nothing
  head (cons t a tas lt) = just (a & t & lt)

  tail : {t₀ : Time} → TList A t₀ → Σ Time (λ t₁ → t₀ ≤ℜ t₁ × TList A t₁)
  tail {t₀} [] = t₀ & ≤ℜ-refl & []
  tail (cons t₁ a tas lt) = t₁ & inl lt & tas

  safelast : {t₀ t₁ : Time} → ValAfter A t₀ → TList A t₁ → t₀ <ℜ t₁ → ValAfter A t₀
  safelast ta [] q = ta
  safelast ta (cons t a tas p) q = safelast (a & t & <ℜ-trans q p) tas (<ℜ-trans q p)

  last : {t₀ : Time} → TList A t₀ → Maybe (ValAfter A t₀)
  last [] = nothing
  last (cons t a tas lt) = just (safelast (a & t & lt) tas lt)

  init : {t₀ : Time} → TList A t₀ → TList A t₀
  init [] = []
  init (cons t a [] q) = []
  init (cons t a tas q) = cons t a (init tas) q

  lookup : {t₀ : Time} → Time → TList A t₀ → Maybe A
  lookup x [] = nothing
  lookup x (cons t a tas p) with compareℜ x t
  lookup x (cons .x a tas p) | refl   = just a
  lookup x (cons t a tas p)  | less q = nothing
  lookup x (cons t a tas p)  | more q = lookup x tas

  map : {t₀ : Time} → (A → A) → TList A t₀ → TList A t₀
  map f [] = []
  map f (cons t a tas p) = cons t (f a) (map f tas) p


  -- delaying increases the time stamps

  delayTL : {t₀ : Time} → (d : Time) → TList A t₀ → TList A (t₀ + d)
  delayTL d [] = []
  delayTL d (cons t a tas lt) = cons (t + d) a (delayTL d tas) (lem-+-<ℜ-cancellative lt)

  -- advancing reduces the time stamps (the point of observation advances forwards)

  advanceTL-less : {t₀ : Time} → (d : Time) → TList A t₀ → (lt : d <ℜ t₀) → TList A ((t₀ - d) (inl lt))
  advanceTL-less d [] q = []
  advanceTL-less d (cons t a tas p) q with (<ℜ-trans q p)
  ... | r = cons ((t - d) (inl r)) a (advanceTL-less d tas r) (lem-<ℜminusCanc (inl q) (inl r) p)

  advanceTL-eq : (d : Time) → TList A d → TList A ∅
  advanceTL-eq d [] = []
  advanceTL-eq d (cons t a tas p) = cons ((t - d) (inl p)) a (advanceTL-less d tas p) (lem-<ℜminuspos p)

  advanceTL-more : {t₀ : Time} → (d : Time) → TList A t₀ → d >ℜ t₀ → TList A ∅
  advanceTL-more d [] q = []
  advanceTL-more d (cons t a tas p) q with compareℜ d t
  advanceTL-more d (cons .d a tas p) q | refl   = advanceTL-eq d tas
  advanceTL-more d (cons t a tas p) q  | less r = cons ((t - d) (inl r)) a (advanceTL-less d tas r) (lem-<ℜminuspos r)
  advanceTL-more d (cons t a tas p) q  | more r = advanceTL-more d tas r

  advanceTSL : Time → TSList A → TSList A
  advanceTSL d (t & tas) with compareℜ d t
  advanceTSL d (.d & tas) | refl   = ∅ & advanceTL-eq d tas
  advanceTSL d (t & tas)  | less p = (t - d) (inl p) & advanceTL-less d tas p
  advanceTSL d (t & tas)  | more p = ∅ & advanceTL-more d tas p

------------------------------------------------------------

open BasicTListFunctions public

------------------------------------------------------------
