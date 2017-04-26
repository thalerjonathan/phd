{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module BoolProps where

open import Bool public

∨assoc : {b c : Bool} → (a : Bool) → (a ∨ b) ∨ c ≡ a ∨ b ∨ c
∨assoc false = refl
∨assoc true = refl

∨comm : {b : Bool} → (a : Bool) → a ∨ b ≡ b ∨ a
∨comm {false} false = refl
∨comm {true} false = refl
∨comm {false} true = refl
∨comm {true} true = refl

∨idem : {b : Bool} → b ∨ b ≡ b
∨idem {false} = refl
∨idem {true} = refl

∨split : {a b : Bool} → a ∨ b ≡ false → a ≡ false × b ≡ false
∨split {false} eq = refl , eq
∨split {true} ()

∨split2 : {a b c : Bool} → a ∨ b ∨ c ≡ false → a ≡ false × b ≡ false × c ≡ false
∨split2 = second ∨split ∘ ∨split


∧assoc : {b c : Bool} → (a : Bool) → (a ∧ b) ∧ c ≡ a ∧ b ∧ c
∧assoc false = refl
∧assoc true = refl

∧comm : {b : Bool} → (a : Bool) → a ∧ b ≡ b ∧ a
∧comm {false} false = refl
∧comm {true} false = refl
∧comm {false} true = refl
∧comm {true} true = refl 

∧idem : {b : Bool} → b ∧ b ≡ b
∧idem {false} = refl
∧idem {true} = refl

∧split : {a b : Bool} → a ∧ b ≡ true → a ≡ true × b ≡ true
∧split {false} ()
∧split {true} eq = refl , eq

∧split2 : {a b c : Bool} → a ∧ b ∧ c ≡ true → a ≡ true × b ≡ true × c ≡ true
∧split2 = second ∧split ∘ ∧split


∧∨distr : {b c : Bool} → (a : Bool) → a ∧ (b ∨ c) ≡ a ∧ b ∨ a ∧ c
∧∨distr false = refl
∧∨distr true = refl


import CommSemiRing
open CommSemiRing _∨_ (λ {b} → ∨assoc b) (λ {b} → ∨comm b) false refl _∧_ (λ {b} →  ∧assoc b) (λ {b} → ∧comm b) refl true refl (λ {b} → ∧∨distr b) public



∨true : (b : Bool) → b ∨ true ≡ true
∨true = ∨comm

∨∨true : (a b : Bool) → a ∨ b ∨ true ≡ true
∨∨true false b = ∨comm b
∨∨true true  b = refl


notnot : {b : Bool} → not (not b) ≡ b
notnot {false} = refl
notnot {true}  = refl

xorAssoc : {b c : Bool} → (a : Bool) → (a xor b) xor c ≡ a xor b xor c
xorAssoc false = refl
xorAssoc {false} true = refl
xorAssoc {true} true = sym notnot

xorcomm : {b : Bool} → (a : Bool) → a xor b ≡ b xor a
xorcomm {false} false = refl
xorcomm {true} false = refl
xorcomm {false} true = refl
xorcomm {true} true = refl

xorfalse : {b : Bool} → b xor false ≡ b
xorfalse {false} = refl
xorfalse {true}  = refl

xorNotIdem : (b : Bool) → b xor b ≡ false
xorNotIdem false = refl
xorNotIdem true  = refl

xor∧distr : {b c : Bool} → (a : Bool) → a ∧ (b xor c) ≡ (a ∧ b) xor (a ∧ c)
xor∧distr false = refl
xor∧distr true  = refl