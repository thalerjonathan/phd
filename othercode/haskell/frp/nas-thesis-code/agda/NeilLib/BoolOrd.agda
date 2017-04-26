{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude
open import BoolProps
open import Ord

module BoolOrd where

infix 3 _≤B_ _<B_

infix 8 _<=B_

--------------------------------

_<=B_ : Op Bool
false <=B _     = true
true  <=B b     = b

--------------------------------

data _<B_ : Bool → Bool → Set where
  f<t : false <B true

<transB : Transitive _<B_
<transB f<t ()

<trichB : Trichotomous _<B_
<trichB {false} {false} = inl (refl , fork (\ ()))
<trichB {false} {true}  = inr (inl ((\ ()) , f<t , \ ()))
<trichB {true} {false}  = inr (inr ( (\ ()) , (\ ()) , f<t))
<trichB {true} {true}   = inl (refl , fork (\ ()))

import StrictTotalOrder
open StrictTotalOrder _<B_ <transB <trichB public

_≤B_ : Rel Bool
_≤B_ = _≤_

f≤t : false ≤B true
f≤t = inl f<t

≤false : {b : Bool} → false ≤B b
≤false {false} = ≤-refl
≤false {true}  = f≤t

≤true : {b : Bool} → b ≤B true
≤true {false} = f≤t
≤true {true}  = ≤-refl

≤B-antisym : Antisymmetric _≤B_
≤B-antisym = antisymmetric

≤B-trans : Transitive _≤B_
≤B-trans = ≤-trans

≤B-total : Total _≤B_
≤B-total = totality

≤B-supL : {a b : Bool} → a ≤B a ∨ b
≤B-supL {false} = ≤false
≤B-supL {true} = ≤-refl

≤B-supR : {a b : Bool} → b ≤B a ∨ b
≤B-supR {false} = ≤-refl
≤B-supR {true}  = ≤true

≤B-leastSup : {x a b : Bool} → a ≤B x → b ≤B x → a ∨ b ≤B x
≤B-leastSup (inl f<t) q = q
≤B-leastSup (inr refl) (inl f<t) = ≤-refl
≤B-leastSup (inr refl) (inr refl) = inr ∨idem

≥B-supL : {a b : Bool} → a ∧ b ≤B a
≥B-supL {false} = ≤-refl
≥B-supL {true}  = ≤true

≥B-supR : {a b : Bool} → a ∧ b ≤B b
≥B-supR {false} = ≤false
≥B-supR {true}  = ≤-refl

≥B-mostInf : {x a b : Bool} → x ≤B a → x ≤B b → x ≤B a ∧ b
≥B-mostInf (inl f<t) q = q
≥B-mostInf (inr refl) (inl f<t) = ≤-refl
≥B-mostInf (inr refl) (inr refl) = inr (sym ∧idem)

-- import TotalOrder
-- open TotalOrder _≤B_ ≤B-antisym ≤B-trans ≤B-total public

import CompleteTotalLattice
open CompleteTotalLattice _≤_ _∨_ _∧_ false true ≤B-antisym ≤B-trans ≤B-total ≤B-supL (λ {a} {b} → ≤B-supR {b} {a}) ≤B-leastSup ≥B-supL (λ {a} {b} → ≥B-supR {b} {a}) ≥B-mostInf ≤false ≤true public


-------------------------------------------------------

∨lem1 : {c : Bool} → (a b : Bool) → (b ∨ b ∨ c ≤B a ∨ b ∨ c)
∨lem1 false false = ≤-refl
∨lem1 false true  = ≤-refl
∨lem1 true false  = ≤true
∨lem1 true true   = ≤-refl 
