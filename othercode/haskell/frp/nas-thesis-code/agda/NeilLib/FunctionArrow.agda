{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module FunctionArrow (extensionality : {A B : Set} -> {f g : A -> B} -> ((a : A) -> f a ≡ g a) -> f ≡ g) where

5thLaw : {A B C : Set} -> {f : A -> B} -> (ac : A × C) -> first f ac ≡ (f ∥ id) ac
5thLaw (a & c) = refl

6thLaw : {A B C D : Set} -> {f : A -> B} -> {g : B -> C} -> (ad : A × D) -> first (\a -> g (f a)) ad ≡ first g (first f ad)
6thLaw (a & d) = refl

7thLaw : {A B C D : Set} -> {f : A -> B} -> {g : C -> D} -> (ac : A × C) -> (id ∥ g) (first f ac) ≡ first f ((id ∥ g) ac)
7thLaw (a & c) = refl

8thLaw : {A B C : Set} -> {f : A -> B} -> (ac : A × C)  -> fst (first f ac) ≡ f (fst ac)
8thLaw (a & c) = refl

9thLaw : {A B C D : Set} -> {f : A -> B} -> (acd : (A × C) × D) -> ×assoc (first (first f) acd) ≡ first f (×assoc acd)
9thLaw ((a & c) & d) = refl

import Arrow
open module FA = Arrow (\A B -> A -> B) id (flip _∘_) first
                       refl refl refl
                       (extensionality 5thLaw)
                       (extensionality 6thLaw)
                       (extensionality 7thLaw)
                       (extensionality 8thLaw)
                       (extensionality 9thLaw)

