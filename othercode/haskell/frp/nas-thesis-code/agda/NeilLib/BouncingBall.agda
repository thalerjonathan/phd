{-# OPTIONS --type-in-type #-}

module BouncingBall where

open import NeilPrelude
open import Real renaming (_<,_ to _<_)
open import Bool renaming (_∧_ to _&&_)

------------------------------------------

Acceleration = ℜ
Velocity = ℜ
Height = ℜ

Ball = Height × Velocity

postulate g : Acceleration

------------------------------------------

detectImpact : Ball → Bool
detectImpact (h , v) = h <= O

negateVel : Ball → Ball
negateVel (h , v) = (h , negate v)

------------------------------------------
