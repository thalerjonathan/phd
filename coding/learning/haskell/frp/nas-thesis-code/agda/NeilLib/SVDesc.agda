{-# OPTIONS --type-in-type #-}

module SVDesc where

open import NeilPrelude

---------------------------------------------------------

infixr 2 _,_

data SVDesc : Set where
  C   : Set → SVDesc
  E   : Set → SVDesc
  S   : Set → SVDesc
  _,_ : SVDesc → SVDesc → SVDesc

---------------------------------------------------------

Sample : SVDesc → Set
Sample (C A)      = A
Sample (E A)      = Maybe A
Sample (S A)      = A
Sample (as , bs)  = Sample as × Sample bs

---------------------------------------------------------
