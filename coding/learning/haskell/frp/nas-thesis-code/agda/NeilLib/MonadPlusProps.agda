{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module MonadPlusProps (M : Set → Set)

       (_>>=_  : {A B : Set} → M A → (A → M B) → M B)
       (return : {A : Set} → A → M A)
       (plus   : {A : Set} → M A → M A → M A)
       (zero   : {A : Set} → M A)

-- monad laws

       (1stLaw : {A B : Set} → {a : A} → {f : A → M B} → return a >>= f ≡ f a)
       (2ndLaw : {A : Set} → {ma : M A} → ma >>= return ≡ ma)
       (3rdLaw : {A B C : Set} → {f : A → M B} → {g : B → M C} → (ma : M A) → (ma >>= f) >>= g ≡ ma >>= (λ b → f b >>= g))

       (4thLaw : {A B : Set} → {f : A → M B} → zero >>= f ≡ zero)
       (5thLaw : {A B : Set} → (ma : M A) → ma >>= (λ a → zero {B}) ≡ zero {B})

       (6thLaw : {A : Set} → {ma : M A} → plus zero ma ≡ ma)
       (7thLaw : {A : Set} → {ma : M A} → plus ma zero ≡ ma)

where

import MonadPlus
open MonadPlus M _>>=_ return plus zero

import MonadProps
open MonadProps M _>>=_ return 1stLaw 2ndLaw 3rdLaw public
