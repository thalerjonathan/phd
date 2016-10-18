{-# OPTIONS --type-in-type #-}

open import NeilPrelude hiding (first ; second ; result ; apply)

module StaticArrow (Arr : Set → Set → Set)

       (arr'    : {A B : Set} → (A → B) → Arr A B) 
       (_>>>'_  : {A B C : Set} → Arr A B → Arr B C → Arr A C)
       (first'  : {A B C : Set} → Arr A B → Arr (A × C) (B × C))
       (delay'  : {A B : Set} → Arr A B → Arr Unit (A → B))

where

----------------------------------------------

import Arrow
open Arrow Arr arr' _>>>'_ first' public

----------------------------------------------

delay : {A B : Set} → Arr A B → Arr Unit (A → B)
delay = delay'

----------------------------------------------

-- All Static Arrows are Idioms (Applicative Functors)

-- Commented out to avoid a cycle

-- I : Set → Set
-- I A = Arr Unit A

-- import Applicative
-- open Applicative I result constant apply