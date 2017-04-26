{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module Monad (M : Set → Set)

       (_>>='_ : {A B : Set} → M A → (A → M B) → M B)
       (ret    : {A : Set} → A → M A)

where

infixl 4 _>>=_ _>>_

_>>=_ : {A B : Set} → M A → (A → M B) → M B
_>>=_ = _>>='_

_=<<_ : {A B : Set} → (A → M B) → M A → M B
_=<<_ = flip _>>=_

return : {A : Set} → A → M A
return = ret

ap : {A B : Set} → M (A → B) → M A → M B
ap mf ma = mf >>= λ f → ma >>= return ∘ f

liftM : {A B : Set} → (A → B) → M A → M B
liftM f = ap (return f)

join : {A : Set} → M (M A) → M A
join ma = ma >>= id

_>>_ : {A B : Set} → M A → M B → M B
ma >> mb = ma >>= const mb

_<<_ : {A B : Set} → M B → M A → M B
_<<_ = flip _>>_

import Applicative
open Applicative M liftM return ap public

------------------------------------------------------

-- Monads are Higher Order Arrows

-- Arr : Set → Set → Set
-- Arr A B = A → M B

-- marr : {A B : Set} → (A → B) → A → M B
-- marr f a = return (f a)

-- mseq : {A B C : Set} → (A → M B) → (B → M C) → A → M C
-- mseq f g a = f a >>= g

-- mfirst : {A B C : Set} → (A → M B) → A × C → M (B × C)
-- mfirst f (a , c) = liftM (λ b → b , c) (f a)

-- mapply : {A B : Set} → (A → M B) × A → M B
-- mapply = ×-apply

-- import HigherOrderArrow
-- open HigherOrderArrow Arr marr mseq mfirst mapply
