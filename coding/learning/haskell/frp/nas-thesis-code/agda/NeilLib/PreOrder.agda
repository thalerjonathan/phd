{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module PreOrder {A : Set} (_≤'_ : A → A → Set)

                    (reflex  : Reflexive _≤'_)
                    (transit : Transitive _≤'_)

where

infix 3 _≤_ _≥_

_≤_ : A → A → Set
_≤_ = _≤'_

_≥_ : A → A → Set
a ≥ b = b ≤ a

reflexive : Reflexive _≤_
reflexive = reflex

transitive : Transitive _≤_
transitive = transit

transitive2 : Transitive2 _≤_
transitive2 ab bc cd = transitive ab (transitive bc cd)

transitive3 : Transitive3 _≤_
transitive3 ab bc cd de = transitive2 ab bc (transitive cd de)

≡implies≤ : {a b : A} → a ≡ b → a ≤ b
≡implies≤ refl = reflex

≤subst : {a b c d : A} → b ≡ a → d ≡ c → a ≤ c → b ≤ d
≤subst refl refl = id

≤substL : {a b c : A} → b ≡ a → a ≤ c → b ≤ c
≤substL = flip ≤subst refl

≤substR : {a b c : A} → c ≡ b → a ≤ b → a ≤ c
≤substR = ≤subst refl

-----------------------------------------------------------------

infixr 1 _≤⟨_⟩_
infix  2 _QED

_≤⟨_⟩_ : {b c : A} → (a : A) → a ≤ b → b ≤ c → a ≤ c
_ ≤⟨ ab ⟩ bc = transitive ab bc

_QED : (a : A) → a ≤ a
_ QED = reflex

-----------------------------------------------------------------
