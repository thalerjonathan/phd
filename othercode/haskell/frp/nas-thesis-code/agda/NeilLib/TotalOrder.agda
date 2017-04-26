{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module TotalOrder {A : Set} (_≤_ : Rel A)

                    (antisym : Antisymmetric _≤_)
                    (transit : Transitive _≤_)
                    (total   : Total _≤_) 

where

import PartialOrder
open PartialOrder _≤_ (case id id total) antisym transit public

totality : Total _≤_
totality = total
