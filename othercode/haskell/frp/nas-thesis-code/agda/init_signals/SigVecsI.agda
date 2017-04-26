{-# OPTIONS --type-in-type #-}

module SigVecsI where

open import NeilPrelude
open import RealTime
open import TimeDeltaList

open import InitDesc public

------------------------------------------------------

-- The change prefixes are inclusive of the time argument
-- (i.e. any changes at that point in time will appear in the list)

ChangeList = TimeDeltaList

ChangePrefix : Set → Set
ChangePrefix A = Time → ChangeList A

SigVec : SVDesc → Set
SigVec (C ini A)  =  Time → A
SigVec (C uni A)  =  Time⁺ → A
SigVec (E A)      =  Maybe A × ChangePrefix A
SigVec (S A)      =  A × ChangePrefix A
SigVec (as , bs)  =  SigVec as × SigVec bs

SF : SVDesc → SVDesc → Set
SF as bs = SigVec as → SigVec bs

------------------------------------------------------

weakenC : {A : Set} → {i₁ i₂ : Init} → i₁ ⟨: i₂ → SigVec (C i₁ A) → SigVec (C i₂ A)
weakenC {A} {ini} {ini} p s = s
weakenC {A} {ini} {uni} p s = λ t → s (t >0)
weakenC {A} {uni} {ini} () s
weakenC {A} {uni} {uni} p s = s

weakenSV : {as bs : SVDesc} → as <: bs → SigVec as → SigVec bs
weakenSV {C i₁ A} {C i₂ .A} (p , refl) = weakenC p
weakenSV {E A} {E .A} refl = id
weakenSV {S A} {S .A} refl = id
weakenSV {as₁ , bs₁} {as₂ , bs₂} (p , q) = weakenSV {as₁} {as₂} p ∥ weakenSV {bs₁} {bs₂} q
weakenSV {C _ _} {E _} ()
weakenSV {C _ _} {S _} ()
weakenSV {C _ _} {_ , _} ()
weakenSV {E _ } {C _ _} ()
weakenSV {E _ } {S _} ()
weakenSV {E _} {_ , _} ()
weakenSV {S _} {C _ _} ()
weakenSV {S _} {E _} ()
weakenSV {S _} {_ , _} ()
weakenSV {_ , _} {C _ _} ()
weakenSV {_ , _} {E _} ()
weakenSV {_ , _} {S _} ()

------------------------------------------------------
