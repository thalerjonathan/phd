{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module Nat where

infixl 10 _+_ _-_
infixl 11 _*_
infixr 12 _^_


data ℕ : Set where
  O : ℕ
  S : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}
{-# BUILTIN ZERO    O #-}
{-# BUILTIN SUC     S #-}

natcases : {A : ℕ → Set} → A O → ((n : ℕ) → A (S n)) → Π ℕ A
natcases a f O     = a
natcases a f (S n) = f n

-----------------------------------------------

_+_         : Op ℕ
O + n       = n
S m + n     = S (m + n)

{-# BUILTIN NATPLUS _+_ #-}

_-_ : Op ℕ
m   - O   = m
O   - n   = O
S m - S n = m - n

{-# BUILTIN NATMINUS _-_ #-}

_*_ : Op ℕ
O   * n = O
S m * n = n + m * n

{-# BUILTIN NATTIMES _*_ #-}

_^_ : Op ℕ
m ^ O = 1
m ^ (S n) = m * m ^ n


data ℕ⁺ : Set where
  S : ℕ → ℕ⁺

_-1 : ℕ⁺ → ℕ
(S n) -1 = n

ℕ⁺toℕ : ℕ⁺ → ℕ
ℕ⁺toℕ (S n) = S n



open import List

replicate : {A : Set} → ℕ → A → List A
replicate O     a = []
replicate (S n) a = a ∷ replicate n a

length : {A : Set} → List A → ℕ
length = foldr (const S) O

sum : List ℕ → ℕ
sum = foldr _+_ 0

product : List ℕ → ℕ
product = foldr _*_ 1

replicateL : {A : Set} → ℕ → List A → List A
replicateL n = concat ∘ replicate n

open import Bool

count : {A : Set} → (A → Bool) → List A → ℕ
count p = foldr (\ a → if p a then S else id) O

