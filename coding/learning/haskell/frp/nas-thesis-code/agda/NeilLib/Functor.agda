{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module Functor (F : Set → Set)

       (fmap' : {A B : Set} → (A → B) → F A → F B)

where

fmap : {A B : Set} → (A → B) → F A → F B
fmap = fmap'
