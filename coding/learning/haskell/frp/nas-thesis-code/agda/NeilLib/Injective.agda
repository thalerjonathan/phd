{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module Injective {A : Set} {B : Set}

       (f    : A → B)
       (inj' : Injective f)

  where

inj : Injective f
inj = inj'
