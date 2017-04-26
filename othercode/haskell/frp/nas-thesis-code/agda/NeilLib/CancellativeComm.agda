{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module CancellativeComm {A : Set} {B : Set}

       (_⊕_ : A → A → B)

       (⊕comm : {a b : A} → a ⊕ b ≡ b ⊕ a)
       (cancL : CancellativeL _⊕_)

  where

cancR : CancellativeR _⊕_
cancR eq = cancL (trans2 ⊕comm eq ⊕comm)

import Cancellative
open Cancellative _⊕_ cancL cancR public
