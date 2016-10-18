{-# OPTIONS --type-in-type
   #-}

open import NeilPrelude
open import List
open import ListProps


module Prefix {A : Set} where

data _⊑_ : List A → List A → Set where
 []   : {bs : List A} → [] ⊑ bs
 cons : {a : A} → {as bs : List A} → as ⊑ bs → (a ∷ as) ⊑ (a ∷ bs)

⊑-refl : {as : List A} → as ⊑ as
⊑-refl {[]} = []
⊑-refl {_ ∷ as} = cons (⊑-refl)

⊑-trans : {as bs cs : List A} → as ⊑ bs → bs ⊑ cs → as ⊑ cs
⊑-trans [] lt2 = []
⊑-trans (cons bs) (cons cs) = cons (⊑-trans bs cs)

⊑-antisym : {as bs : List A} → as ⊑ bs → bs ⊑ as → as ≡ bs
⊑-antisym [] [] = refl
⊑-antisym (cons p) (cons q) = tail-resp (⊑-antisym p q)

import PartialOrder
module PrefixPO = PartialOrder _⊑_ ⊑-refl ⊑-antisym ⊑-trans