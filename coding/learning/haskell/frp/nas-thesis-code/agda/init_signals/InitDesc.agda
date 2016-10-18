{-# OPTIONS --type-in-type #-}

module InitDesc where

open import NeilPrelude
open import Logic

------------------------------------------------------------------------

data Init : Set where
  ini  : Init  -- initialised signal
  uni  : Init  -- uninitialised signal

_⊓_ : Init → Init → Init
ini  ⊓ ini  = ini
_    ⊓ _    = uni

_⟨:_ : Init → Init → Set
uni  ⟨: ini  = False
_    ⟨: _    = True

------------------------------------------------------------------------

⟨:-refl : {i : Init} → i ⟨: i
⟨:-refl {ini} = _
⟨:-refl {uni} = _

------------------------------------------------------------------------

data SVDesc : Set where
  C   : Init → Set → SVDesc
  E   : Set → SVDesc
  S   : Set → SVDesc
  _,_ : SVDesc → SVDesc → SVDesc

------------------------------------------------------

iniSV : SVDesc → SVDesc
iniSV (C _ A)   = C ini A
iniSV (E A)     = E A
iniSV (S A)     = S A
iniSV (as , bs) = (iniSV as , iniSV bs)

uniSV : SVDesc → SVDesc
uniSV (C _ A)   = C uni A
uniSV (E A)     = E A
uniSV (S A)     = S A
uniSV (as , bs) = (uniSV as , uniSV bs)

------------------------------------------------------

_<:_ : SVDesc → SVDesc → Set
C i₁ A      <: C i₂ B      = (i₁ ⟨: i₂) × (A ≡ B)
E A         <: E B         = A ≡ B
S A         <: S B         = A ≡ B
(as₁ , bs₁) <: (as₂ , bs₂) = (as₁ <: as₂) × (bs₁ <: bs₂)
_           <: _           = False

------------------------------------------------------------------------

<:-refl : {as : SVDesc} → as <: as
<:-refl {C i _} = (⟨:-refl , refl)
<:-refl {E _} = refl
<:-refl {S _} = refl
<:-refl {as , bs} = (<:-refl {as} , <:-refl {bs})

<:-refl' : {as bs : SVDesc} → as ≡ bs → as <: bs
<:-refl' {as} refl = <:-refl {as}

lem-iniSub : {as : SVDesc} → (iniSV as <: as)
lem-iniSub {C _ _} = (_ , refl)
lem-iniSub {E _} = refl
lem-iniSub {S _} = refl
lem-iniSub {as , bs} = lem-iniSub {as} , lem-iniSub {bs}

lem-uniSub : {as : SVDesc} → (as <: uniSV as)
lem-uniSub {C ini _} = (_ , refl)
lem-uniSub {C uni _} = (_ , refl)
lem-uniSub {E _} = refl
lem-uniSub {S _} = refl
lem-uniSub {as , bs} = lem-uniSub {as} , lem-uniSub {bs}

lemE-subL : {as : SVDesc} → {A : Set} → E A <: as → as ≡ E A
lemE-subL {C _ _} ()
lemE-subL {E _} refl = refl
lemE-subL {S _} ()
lemE-subL {_ , _} ()

lemS-subL : {as : SVDesc} → {A : Set} → S A <: as → as ≡ S A
lemS-subL {C _ _} ()
lemS-subL {E _} ()
lemS-subL {S _} refl = refl
lemS-subL {_ , _} ()

lemE-subR : {as : SVDesc} → {A : Set} → as <: E A → as ≡ E A
lemE-subR {C _ _} ()
lemE-subR {E _} refl = refl
lemE-subR {S _} ()
lemE-subR {_ , _} ()

lemS-subR : {as : SVDesc} → {A : Set} → as <: S A → as ≡ S A
lemS-subR {C _ _} ()
lemS-subR {E _} ()
lemS-subR {S _} refl = refl
lemS-subR {_ , _} ()

------------------------------------------------------------------------
