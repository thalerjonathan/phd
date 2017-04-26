{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module Pointed (F : Set → Set)

       (fmap   : {A B : Set} → (A → B) → F A → F B)
       (pure'  : {A : Set} → A → F A)

where

import Functor
open Functor F fmap public

pure : {A : Set} → A → F A
pure = pure'
