{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module Bool where

infix  0  if_then_else_

infixr 5  _∨_ _xor_
infixr 6  _∧_

----------------------------------------

not : Bool → Bool
not false = true
not true  = false

if_then_else_ : {A : Set} → Bool → A → A → A
if_then_else_ false t e = e
if_then_else_ true t e = t

ifte : {A : Set} → A → A → Bool → A
ifte t e false = e
ifte t e true  = t

_∧_ : Bool → Bool → Bool
true  ∧ b = b
false ∧ b = false

_∨_ : Bool → Bool → Bool
true  ∨ b = true
false ∨ b = b

_xor_ : Bool → Bool → Bool
true  xor b = not b
false xor b = b

----------------------------------------
