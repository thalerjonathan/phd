{-# OPTIONS --type-in-type
   #-}

open import NeilPrelude

module Integer where

data ℤ : Set where
  +S : ℕ → ℤ  
  Z  : ℤ
  -S : ℕ → ℤ

pred : ℤ → ℤ
pred (+S O)     = Z
pred (+S (S n)) = +S n
pred Z          = -S O
pred (-S n)     = -S (S n)