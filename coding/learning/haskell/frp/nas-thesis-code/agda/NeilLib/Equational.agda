{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module Equational where


infixr 1 _≡⟨_⟩_
infix  2 _QED

_≡⟨_⟩_ : {A : Set} → {b c : A} → (a : A) → a ≡ b → b ≡ c → a ≡ c
_ ≡⟨ refl ⟩ refl = refl

_QED : {A : Set} → (a : A) → a ≡ a
_ QED = refl

open import NatProps
open import Nat

exampleLem1 : (n : ℕ) → (n + O) + n ≡ (n + n) + O
exampleLem1 n =  n + O + n
                    ≡⟨ comm {n + O}⟩
                 n + (n + O)
                    ≡⟨ assocR {n} ⟩
                 (n + n) + O 
               QED
