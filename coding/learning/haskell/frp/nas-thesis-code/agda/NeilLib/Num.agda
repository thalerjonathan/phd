{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module Num where

infixl 10 _+'_ _-'_
infixl 11 _*'_

-- Numeric Universe --

data NumType : Set where
  nat : NumType
  int : NumType

numSet : NumType → Set
numSet nat = ℕ
numSet int = ℤ

toℤ : {t : NumType} → numSet t → ℤ
toℤ {nat} O     = Z
toℤ {nat} (S n) = +S n 
toℤ {int} i     = i

negate : {t : NumType} → numSet t → ℤ
negate {nat} O      = Z
negate {nat} (S n)  = -S n
negate {int} (+S n) = -S n
negate {int} Z      = Z
negate {int} (-S n) = +S n

abs : {t : NumType} → numSet t → ℕ
abs {nat} n      = n
abs {int} (+S n) = S n
abs {int} Z      = O
abs {int} (-S n) = S n

succ : {t : NumType} → numSet t → numSet t
succ {nat} n          = S n
succ {int} (+S n)     = +S (S n)
succ {int} Z          = +S O
succ {int} (-S O)     = Z
succ {int} (-S (S n)) = -S n


_+'_ : {s t : NumType} → numSet s → numSet t → ℤ

_+'_ {nat} O     y  = toℤ y
_+'_ {nat} (S n) y  = succ (n +' y)

_+'_ {int} (+S n)     y = succ (n +' y)
_+'_ {int} Z          y = toℤ y
_+'_ {int} (-S O)     y = pred (toℤ y)
_+'_ {int} (-S (S n)) y = pred (-S n +' y)


_-'_ : {s t : NumType} → numSet s → numSet t → ℤ
x -' y = negate (toℤ y) +' x


_*'_ : {s t : NumType} → numSet s → numSet t → ℤ
_*'_ {nat} O      y = Z
_*'_ {nat} (S n)  y = y +' n *' y
_*'_ {int} (+S n) y = y +' n *' y
_*'_ {int} Z      y = Z
_*'_ {int} (-S n) y = negate (y +' n *' y)


nonZero : {t : NumType} → numSet t → Bool
nonZero {nat} O = false
nonZero {int} Z = false
nonZero       _ = true

isZero : {t : NumType} → numSet t → Bool
isZero = not ∘ nonZero 

private

  even' : ℕ → Bool
  even' O = true
  even' (S O) = false
  even' (S (S n)) = even' n

even : {t : NumType} → numSet t → Bool
even {nat} = even'
even {int} = even' ∘ abs

odd : {t : NumType} → numSet t → Bool
odd = ¬ even
