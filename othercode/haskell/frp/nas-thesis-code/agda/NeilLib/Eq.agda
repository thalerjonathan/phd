{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module Eq where

infix  8  _==_ _!=_

-- Equality Universe --

data EqUni : Set where
  bool    : EqUni
  nat     : EqUni
  int     : EqUni
  maybeEq : EqUni → EqUni
  list    : EqUni → EqUni
  vector  : EqUni → ℕ → EqUni
  fin     : ℕ → EqUni

eqSet : EqUni → Set
eqSet bool = Bool
eqSet nat = ℕ
eqSet int = ℤ
eqSet (maybeEq t) = Maybe (eqSet t)
eqSet (list t) = List (eqSet t)
eqSet (vector t n) = Vec (eqSet t) n
eqSet (fin n) = Fin n

_==_ : {t : EqUni} → eqSet t → eqSet t → Bool

_==_ {bool} false b = not b
_==_ {bool} true b = b

_==_ {nat} O O = true
_==_ {nat} (S m) (S n) = m == n
_==_ {nat} _ _ = false

_==_ {int} Z Z = true
_==_ {int} (+S m) (+S n) = m == n
_==_ {int} (-S m) (-S n) = m == n
_==_ {int} _ _ = false

_==_ {maybeEq t} nothing nothing = true
_==_ {maybeEq t} (just a) (just b) = a == b
_==_ {maybeEq t} _ _ = false

_==_ {list t} [] [] = true
_==_ {list t} (a ∷ as) (b ∷ bs) = a == b ∧ as == bs
_==_ {list t} _ _ = false

_==_ {vector t O} [] [] = true
_==_ {vector t (S n)} (a ∷ as) (b ∷ bs) = a == b ∧ as == bs

_==_ {fin O} () () 
_==_ {fin (S n)} fO fO = true
_==_ {fin (S n)} (fS fm) (fS fn) = fm == fn
_==_ {fin (S n)} _ _ = false


_!=_ : {t : EqUni} → eqSet t → eqSet t → Bool
a != b = not (a == b)

