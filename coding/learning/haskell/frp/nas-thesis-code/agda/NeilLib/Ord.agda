{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude
open import Bool


module Ord {A : Set} (_<='_ : A → A → Bool) where


infixr 5 _⊔_
infixr 6 _⊓_
infix  8 _<=_ _<_ _>=_ _>_

_<=_    : A → A → Bool
_<=_    = _<='_

_<_     : A → A → Bool
a < b   = not (b <= a)

_>_     : A → A → Bool
a > b   = b < a

_>=_    : A → A → Bool
a >= b  = b <= a

_⊔_     : A → A → A
a ⊔ b   = if a <= b then b else a

_⊓_     : A → A → A
a ⊓ b   = if a <= b then a else b


