{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module CommGroup {A : Set}

       (_⊕_ : A → A → A)
       (assoc : {b c : A} → (a : A) → (a ⊕ b) ⊕ c ≡ a ⊕ (b ⊕ c))
       (⊕comm : {b : A} → (a : A) → a ⊕ b ≡ b ⊕ a)
       (unit : A) (unit⊕ : {a : A} → unit ⊕ a ≡ a)
       (inverse : A → A) (⊕inverse : {a : A} → a ⊕ inverse a ≡ unit)

  where

inverse⊕ : {a : A} → inverse a ⊕ a ≡ unit
inverse⊕ {a} = ⊕inverse ∘≡ ⊕comm (inverse a)

import CommMonoid
open module CMoid = CommMonoid _⊕_ assoc ⊕comm unit unit⊕

import Group
open module G⊕ = Group  _⊕_ assoc unit unit⊕ ⊕unit inverse ⊕inverse inverse⊕ public


