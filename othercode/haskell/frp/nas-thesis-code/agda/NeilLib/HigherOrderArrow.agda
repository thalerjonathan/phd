{-# OPTIONS --type-in-type #-}

open import NeilPrelude hiding (first)

module HigherOrderArrow (Arr : Set → Set → Set)

       (arr    : {A B : Set} → (A → B) → Arr A B) 
       (_>>>'_ : {A B C : Set} → Arr A B → Arr B C → Arr A C)
       (first  : {A B C : Set} → Arr A B → Arr (A × C) (B × C))
       (app'   : {A B : Set} → Arr (Arr A B × A) B)

where

----------------------------------------------

app : {A B : Set} → Arr (Arr A B × A) B
app = app'

import Arrow
open Arrow Arr arr _>>>'_ first public

----------------------------------------------

-- Higher Order Arrows are Monads

-- M : Set → Set
-- M A = Arr Unit A

-- hoaBind : {A B : Set} → Arr Unit A → (A → Arr Unit B) → Arr Unit B
-- hoaBind a f = arr fork >>> first (a >>> arr f) >>> app

-- import Monad
-- open Monad M hoaBind constant