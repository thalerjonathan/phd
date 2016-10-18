{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module Equivalence {A : Set} (_~_ : Rel A)

                    (reflex   : Reflexive _~_)
                    (sym      : Symmetric _~_)
                    (transit  : Transitive _~_)

where

import PreOrder
open PreOrder _~_ reflex transit public

import PartialEquivalence
open PartialEquivalence _~_ sym transit public

import Dependency
open Dependency _~_ reflex sym public
