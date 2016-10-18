{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import List
open import Bool

module MonadPlus (M : Set → Set)

       (_>>=_  : {A B : Set} → M A → (A → M B) → M B)
       (return : {A : Set} → A → M A)
       (plus   : {A : Set} → M A → M A → M A)
       (zero   : {A : Set} → M A)

where

import Monad
open Monad M _>>=_ return public

mplus : {A : Set} → M A → M A → M A
mplus = plus

mzero : {A : Set} → M A
mzero = zero

msum : {A : Set} → List (M A) → M A
msum = foldl mplus mzero

mfilter : {A : Set} → (A → Bool) → M A → M A
mfilter p ma = ma >>= λ a → if p a then return a else mzero

guard : Bool → M Unit
guard false = mzero
guard true  = return unit