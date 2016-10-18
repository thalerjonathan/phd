{-# OPTIONS --type-in-type
   #-}

open import NeilPrelude

module Setoid
              (Carrier : Set)
              (_≈_     : Rel Carrier)
              (equiv   : Equivalence _≈_)

where

