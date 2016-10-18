{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module VectorSpace {S V : Set}

       (_⊕_ : V → V → V)
       (assoc : {v w : V} → (u : V) → (u ⊕ v) ⊕ w ≡ u ⊕ (v ⊕ w))
       (⊕comm : {v : V} → (u : V) → u ⊕ v ≡ v ⊕ u)
       (origin : V) (origin⊕ : {v : V} → origin ⊕ v ≡ v)
       (inverse : V → V) (⊕inverse : {v : V} → v ⊕ inverse v ≡ origin)

       (_⊗_ : S → V → V)
       (_+_ : S → S → S)
       (_X_ : S → S → S)
       (distrL : {u v : V} → (s : S) → s ⊗ (u ⊕ v) ≡ (s ⊗ u) ⊕ (s ⊗ v))
       (distrR : {t : S} → {v : V} → (s : S) → (s + t) ⊗ v ≡ (s ⊗ v) ⊕ (t ⊗ v))
       (compat : {t : S} → {v : V} → (s : S) → s ⊗ (t ⊗ v) ≡ (s X t) ⊗ v)
       (unit : S) (unitX : {v : V} → unit ⊗ v ≡ v)       

  where

import CommGroup
open module CGroup = CommGroup _⊕_ assoc ⊕comm origin origin⊕ inverse ⊕inverse

