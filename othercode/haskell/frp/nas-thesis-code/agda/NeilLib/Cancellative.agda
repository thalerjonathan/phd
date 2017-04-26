{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module Cancellative {A : Set} {B : Set} {C : Set}

       (_⊕_ : A → B → C)

       (cancL : CancellativeL _⊕_)
       (cancR : CancellativeR _⊕_)

  where

⊕canc : CancellativeL _⊕_
⊕canc = cancL

canc⊕ : CancellativeR _⊕_
canc⊕ = cancR

⊕⊕canc : {a₁ a₂ : A} → {b₁ b₂ : B} → a₁ ⊕ b₁ ≡ a₁ ⊕ b₂ → a₂ ⊕ b₁ ≡ a₂ ⊕ b₂
⊕⊕canc = cong2L _⊕_ ∘ ⊕canc

canc⊕⊕ : {a₁ a₂ : A} → {b₁ b₂ : B} → a₁ ⊕ b₁ ≡ a₂ ⊕ b₁ → a₁ ⊕ b₂ ≡ a₂ ⊕ b₂
canc⊕⊕ = cong2R _⊕_ ∘ canc⊕

