{-# OPTIONS --type-in-type
   #-}

open import NeilPrelude

module Cnst where

--- Const n ---

record Product (A B : Set) : Set where
  field fst' : A
        snd' : B

Vec' : Set → ℕ → Set
Vec' A O     = True
Vec' A (S n) = Product A (Vec' A n)

cnstType : Set → (n : ℕ) → Vec' Set n → Set
cnstType A O _ = A
cnstType A (S n) v = let open Product v in fst' → cnstType A n snd'

cnst : {A : Set} → (n : ℕ) → A → {v : Vec' Set n} → cnstType A n v
cnst O     a = a
cnst (S n) a = λ _ → cnst n a

---------------