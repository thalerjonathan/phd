{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module PartialOrder {A : Set} (_≤_ : Rel A)

                    (reflex  : Reflexive _≤_)
                    (antisym : Antisymmetric _≤_)
                    (transit : Transitive _≤_)

where

import PreOrder
open PreOrder _≤_ reflex transit public

antisymmetric : Antisymmetric _≤_
antisymmetric = antisym

