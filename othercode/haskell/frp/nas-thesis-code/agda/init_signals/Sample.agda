{-# OPTIONS --type-in-type #-}

module Sample where

open import NeilPrelude
open import InitDesc

------------------------------------------------------------------------

Sample₀ : SVDesc → Set
Sample₀ (C ini A) = A
Sample₀ (C uni A) = Unit
Sample₀ (E A)     = Maybe A
Sample₀ (S A)     = A
Sample₀ (as , bs) = Sample₀ as × Sample₀ bs

------------------------------------------------------------------------

weakenSample₀ : {as as' : SVDesc} → (as <: as') → Sample₀ as → Sample₀ as'
weakenSample₀ {E _} {as'} p s rewrite lemE-subL {as'} p = s
weakenSample₀ {S _} {as'} p s rewrite lemS-subL {as'} p = s
weakenSample₀ {as} {E _} p s rewrite lemE-subR {as} p = s
weakenSample₀ {as} {S _} p s rewrite lemS-subR {as} p = s
weakenSample₀ {C ini _} {C ini ._} (p , refl) s = s
weakenSample₀ {C ini _} {C uni ._} (p , refl) s = unit
weakenSample₀ {C uni _} {C ini ._} (() , refl) s
weakenSample₀ {C uni _} {C uni ._} (p , refl) s = s
weakenSample₀ {C _ _} {_ , _} () s
weakenSample₀ {_ , _} {C _ _} () s
weakenSample₀ {as , bs} {as' , bs'} (p , q) (s₁ , s₂) = weakenSample₀ {as} {as'} p s₁ , weakenSample₀ {bs} {bs'} q s₂

weakenIniSample : {as : SVDesc} → Sample₀ (iniSV as) → Sample₀ as
weakenIniSample {as} = weakenSample₀ {iniSV as} {as} (lem-iniSub {as})

-- coerceSample : {as as' : SVDesc} → (as <: as') ⊎ (as' <: as) → Sample' as → Sample' as'
-- coerceSample {C ini _} {C ini ._} (inl (p , refl)) s = s
-- coerceSample {C ini _} {C ini ._} (inr (p , refl)) s = s
-- coerceSample {C ini _} {C uni ._} (inl (p , refl)) s = s
-- coerceSample {C ini _} {C uni ._} (inr (p , refl)) s = s
-- coerceSample {C uni _} {C ini ._} (inl (() , refl)) s
-- coerceSample {C uni _} {C ini ._} (inr (p , refl)) s = s
-- coerceSample {C uni _} {C uni ._} (inl (p , refl)) s = s
-- coerceSample {C uni _} {C uni ._} (inr (p , refl)) s = s
-- coerceSample {C _ _} {E _} (inl ()) s
-- coerceSample {C _ _} {E _} (inr ()) s
-- coerceSample {C _ _} {S _} (inl ()) s
-- coerceSample {C _ _} {S _} (inr ()) s
-- coerceSample {C _ _} {_ , _} (inl ()) s
-- coerceSample {C _ _} {_ , _} (inr ()) s
-- coerceSample {E _} {C _ _} (inl ()) s
-- coerceSample {E _} {C _ _} (inr ()) s
-- coerceSample {E _} {E ._} (inl refl) s = s
-- coerceSample {E _} {E ._} (inr refl) s = s
-- coerceSample {E _} {S _} (inl ()) s
-- coerceSample {E _} {S _} (inr ()) s
-- coerceSample {E _} {_ , _} (inl ()) s
-- coerceSample {E _} {_ , _} (inr ()) s
-- coerceSample {S _} {C _ _} (inl ()) s
-- coerceSample {S _} {C _ _} (inr ()) s
-- coerceSample {S _} {E _} (inl ()) s
-- coerceSample {S _} {E _} (inr ()) s
-- coerceSample {S _} {S ._} (inl refl) s = s
-- coerceSample {S _} {S ._} (inr refl) s = s
-- coerceSample {S _} {_ , _} (inl ()) s
-- coerceSample {S _} {_ , _} (inr ()) s
-- coerceSample {_ , _} {C _ _} (inl ()) s
-- coerceSample {_ , _} {C _ _} (inr ()) s
-- coerceSample {_ , _} {E _} (inl ()) s
-- coerceSample {_ , _} {E _} (inr ()) s
-- coerceSample {_ , _} {S _} (inl ()) s
-- coerceSample {_ , _} {S _} (inr ()) s
-- coerceSample {as , bs} {as' , bs'} (inl (p , q)) (s₁ , s₂) = coerceSample {as} {as'} (inl p) s₁ , coerceSample {bs} {bs'} (inl q) s₂
-- coerceSample {as , bs} {as' , bs'} (inr (p , q)) (s₁ , s₂) = coerceSample {as} {as'} (inr p) s₁ , coerceSample {bs} {bs'} (inr q) s₂

-- weakenSample' : {as as' : SVDesc} → (as <: as') → Sample' as → Sample' as'
-- weakenSample' {as} {as'} p = coerceSample {as} {as'} (inl p)

------------------------------------------------------------------------

-- weakenSample : {as : SVDesc} → Sample' as → Sample₀ as
-- weakenSample {C ini _} s = s
-- weakenSample {C uni _} s = unit
-- weakenSample {E _}     s = s
-- weakenSample {S _}     s = s
-- weakenSample {as , bs} (s₁ , s₂) = (weakenSample {as} s₁ , weakenSample {bs} s₂)

-- strengthenSample : {as : SVDesc} → Sample₀ (iniSV as) → Sample' as
-- strengthenSample {C ini _} s = s
-- strengthenSample {C uni _} s = s
-- strengthenSample {E _} s = s
-- strengthenSample {S _} s = s
-- strengthenSample {as , bs} (s₁ , s₂) = (strengthenSample {as} s₁ , strengthenSample {bs} s₂)

------------------------------------------------------------------------

data SVDesc' : Set where
  C   : Set → SVDesc'
  E   : Set → SVDesc'
  S   : Set → SVDesc'
  _,_ : SVDesc' → SVDesc' → SVDesc'

------------------------------------------------------------------------

Sample' : SVDesc' → Set
Sample' (C A)      = A
Sample' (E A)      = Maybe A
Sample' (S A)      = A
Sample' (as , bs)  = Sample' as × Sample' bs

------------------------------------------------------------------------

-- if we ever translate back, it will be after time zero so everything will be initialised

svd'Tosvd : SVDesc' → SVDesc
svd'Tosvd (C A) = C ini A
svd'Tosvd (E A) = E A
svd'Tosvd (S A) = S A
svd'Tosvd (as , bs) = (svd'Tosvd as , svd'Tosvd bs)

svdTosvd' : SVDesc → SVDesc'
svdTosvd' (C _ A) = C A
svdTosvd' (E A) = E A
svdTosvd' (S A) = S A
svdTosvd' (as , bs) = (svdTosvd' as , svdTosvd' bs)

------------------------------------------------------------------------

sv-cong : Congruent2 {SVDesc} {SVDesc} {SVDesc} _,_
sv-cong refl refl = refl

sv-congL : Congruent2L {SVDesc} {SVDesc} {SVDesc} _,_
sv-congL refl = refl

sv-congR : Congruent2R {SVDesc} {SVDesc} {SVDesc} _,_
sv-congR refl = refl

sv'-cong : Congruent2 {SVDesc'} {SVDesc'} {SVDesc'} _,_
sv'-cong refl refl = refl

sv'-congL : Congruent2L {SVDesc'} {SVDesc'} {SVDesc'} _,_
sv'-congL refl = refl

sv'-congR : Congruent2R {SVDesc'} {SVDesc'} {SVDesc'} _,_
sv'-congR refl = refl

------------------------------------------------------------------------

lem-svd' : {as : SVDesc'} → svdTosvd' (svd'Tosvd as) ≡ as
lem-svd' {C A} = refl
lem-svd' {E A} = refl
lem-svd' {S A} = refl
lem-svd' {as , bs} = sv'-cong (lem-svd' {as}) (lem-svd' {bs})

lem-svd : {as : SVDesc} → svd'Tosvd (svdTosvd' as) ≡ iniSV as
lem-svd {C _ _} = refl
lem-svd {E _} = refl
lem-svd {S _} = refl
lem-svd {as , bs} = sv-cong (lem-svd {as}) (lem-svd {bs})

lem-svd-ini : {as : SVDesc'} → iniSV (svd'Tosvd as) ≡ svd'Tosvd as
lem-svd-ini {C _} = refl
lem-svd-ini {E _} = refl
lem-svd-ini {S _} = refl
lem-svd-ini {as , bs} = sv-cong (lem-svd-ini {as}) (lem-svd-ini {bs})

lem-svd-sub : {as : SVDesc} → svd'Tosvd (svdTosvd' as) <: as
lem-svd-sub {C _ _} = _ , refl
lem-svd-sub {E _} = refl
lem-svd-sub {S _} = refl
lem-svd-sub {as , bs} = lem-svd-sub {as} , lem-svd-sub {bs}

lem-svd'-ini : {as : SVDesc} → svdTosvd' (iniSV as) ≡ svdTosvd' as
lem-svd'-ini {C _ _} = refl
lem-svd'-ini {E _} = refl
lem-svd'-ini {S _} = refl
lem-svd'-ini {as , bs} = sv'-cong (lem-svd'-ini {as}) (lem-svd'-ini {bs})

------------------------------------------------------------------------

lem-svd-ini-sub : {as : SVDesc} → iniSV as <: svd'Tosvd (svdTosvd' as)
lem-svd-ini-sub {as} = <:-refl' (sym (lem-svd {as}))

------------------------------------------------------------------------

lem-svd'-sub-eq : {as as' : SVDesc} → as <: as' → svdTosvd' as ≡ svdTosvd' as'
lem-svd'-sub-eq {E _} {as'} p rewrite lemE-subL {as'} p = refl
lem-svd'-sub-eq {S _} {as'} p rewrite lemS-subL {as'} p = refl
lem-svd'-sub-eq {as} {E _} p rewrite lemE-subR {as} p = refl
lem-svd'-sub-eq {as} {S _} p rewrite lemS-subR {as} p = refl
lem-svd'-sub-eq {C i _} {C i' ._} (_ , refl) = refl
lem-svd'-sub-eq {C _ _} {_ , _} ()
lem-svd'-sub-eq {_ , _} {C _ _} ()
lem-svd'-sub-eq {as , bs} {as' , bs'} (p , q) = sv'-cong (lem-svd'-sub-eq {as} {as'} p) (lem-svd'-sub-eq {bs} {bs'} q)

------------------------------------------------------------------------

sample'ToSample₀ : {as : SVDesc'} → Sample' as → Sample₀ (svd'Tosvd as)
sample'ToSample₀ {C A} s = s
sample'ToSample₀ {E A} s = s
sample'ToSample₀ {S A} s = s
sample'ToSample₀ {as , bs} (s₁ , s₂) = sample'ToSample₀ {as} s₁ , sample'ToSample₀ {bs} s₂

sample'ToSample₀2 : {as : SVDesc} → Sample' (svdTosvd' as) → Sample₀ (iniSV as)
sample'ToSample₀2 {C _ _} s = s
sample'ToSample₀2 {E _} s = s
sample'ToSample₀2 {S _} s = s
sample'ToSample₀2 {as , bs} (s₁ , s₂) = sample'ToSample₀2 {as} s₁ , sample'ToSample₀2 {bs} s₂

sample₀ToSample' : {as : SVDesc'} → Sample₀ (svd'Tosvd as) → Sample' as
sample₀ToSample' {C A} s = s
sample₀ToSample' {E A} s = s
sample₀ToSample' {S A} s = s
sample₀ToSample' {as , bs} (s₁ , s₂) = sample₀ToSample' {as} s₁ , sample₀ToSample' {bs} s₂

sample₀ToSample'2 : {as : SVDesc} → Sample₀ (iniSV as) → Sample' (svdTosvd' as)
sample₀ToSample'2 {C _ _} s = s
sample₀ToSample'2 {E _} s = s
sample₀ToSample'2 {S _} s = s
sample₀ToSample'2 {as , bs} (s₁ , s₂) = sample₀ToSample'2 {as} s₁ , sample₀ToSample'2 {bs} s₂

weakenSample'ToSample₀ : {as : SVDesc} → Sample' (svdTosvd' as) → Sample₀ as
weakenSample'ToSample₀ {C ini A} s = s
weakenSample'ToSample₀ {C uni A} s = unit
weakenSample'ToSample₀ {E A} s = s
weakenSample'ToSample₀ {S A} s = s
weakenSample'ToSample₀ {as , bs} (s₁ , s₂) = weakenSample'ToSample₀ {as} s₁ , weakenSample'ToSample₀ {bs} s₂

------------------------------------------------------------------------

subst-Sample' : {as : SVDesc} → Sample' (svdTosvd' as) → Sample' (svdTosvd' (iniSV as))
subst-Sample' {as} = subst (cong Sample' (sym (lem-svd'-ini {as})))

subst-Sample'R : {as : SVDesc} → Sample' (svdTosvd' (iniSV as)) → Sample' (svdTosvd' as)
subst-Sample'R {as} = subst (cong Sample' (lem-svd'-ini {as}))

------------------------------------------------------------------------

caseSample : ∀ {i A B} → B → (A → B) → Sample₀ (C i A) → B
caseSample {ini} b f a    = f a
caseSample {uni} b f unit = b

------------------------------------------------------------------------