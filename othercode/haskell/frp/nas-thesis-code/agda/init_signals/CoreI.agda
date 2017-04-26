{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import InitDesc
open import Sample

module CoreI where

import RealTime
open RealTime public

---------------------------------------------------------

data Node (as bs : SVDesc') : Set where
  node   : ∀ {Q} → (Δt → Q → Sample' as → Q × Sample' bs) → Q → Node as bs

stepNode : ∀ {as bs} → Δt → Node as bs → Sample' as → Node as bs × Sample' bs
stepNode δ (node f q) sa = first (node f) (f δ q sa)

---------------------------------------------------------

data AtomicRouter₀ : SVDesc → SVDesc → Set where
  sfId      : ∀ {as}     → AtomicRouter₀ as as
  fstProj   : ∀ {as bs}  → AtomicRouter₀ (as , bs) as
  sndProj   : ∀ {as bs}  → AtomicRouter₀ (as , bs) bs

stepARouter₀ : ∀ {as bs} → AtomicRouter₀ as bs → Sample₀ as → Sample₀ bs
stepARouter₀ sfId     sa         = sa
stepARouter₀ fstProj  (sa₁ , _)  = sa₁
stepARouter₀ sndProj  (_ , sa₂)  = sa₂

data AtomicRouter' : SVDesc' → SVDesc' → Set where
  sfId      : ∀ {as}     → AtomicRouter' as as
  fstProj   : ∀ {as bs}  → AtomicRouter' (as , bs) as
  sndProj   : ∀ {as bs}  → AtomicRouter' (as , bs) bs

stepARouter' : ∀ {as bs} → AtomicRouter' as bs → Sample' as → Sample' bs
stepARouter' sfId     sa         = sa
stepARouter' fstProj  (sa₁ , _)  = sa₁
stepARouter' sndProj  (_ , sa₂)  = sa₂

routerToRouter' : ∀ {as bs} → AtomicRouter₀ as bs → AtomicRouter' (svdTosvd' as) (svdTosvd' bs)
routerToRouter' sfId    = sfId
routerToRouter' fstProj = fstProj
routerToRouter' sndProj = sndProj

router'ToRouter : ∀ {as bs} → AtomicRouter' as bs → AtomicRouter₀ (svd'Tosvd as) (svd'Tosvd bs)
router'ToRouter sfId    = sfId
router'ToRouter fstProj = fstProj
router'ToRouter sndProj = sndProj

---------------------------------------------------------

data SF : SVDesc → SVDesc → Set where
  prim       : ∀ {as bs}     → (Sample₀ as → Node (svdTosvd' as) (svdTosvd' bs) × Sample₀ bs)   → SF as bs
  arouter    : ∀ {as bs}     → AtomicRouter₀ as bs                                                    → SF as bs
  seq        : ∀ {as bs cs}  → SF as bs → SF bs cs                                              → SF as cs
  fan        : ∀ {as bs cs}  → SF as bs → SF as cs                                              → SF as (bs , cs)
  rswitcher  : ∀ {as bs A}   → (SF as (bs , E A)) → (A → SF as (iniSV bs , E A))                → SF as bs
  freezer    : ∀ {as bs}     → SF as bs                                                         → SF as (bs , C ini (SF (iniSV as) bs))
  weaken     : ∀ {as as' bs bs'} → as <: as' → bs' <: bs → SF as' bs'                           → SF as bs

data SF' : SVDesc' → SVDesc' → Set where
  prim       : ∀ {as bs}     → Node as bs                                                          → SF' as bs
  arouter    : ∀ {as bs}     → AtomicRouter' as bs                                                       → SF' as bs
  seq        : ∀ {as bs cs}  → SF' as bs → SF' bs cs                                               → SF' as cs
  fan        : ∀ {as bs cs}  → SF' as bs → SF' as cs                                               → SF' as (bs , cs)
  rswitcher  : ∀ {as bs A}   → (SF' as (bs , E A)) → (A → SF (svd'Tosvd as) (svd'Tosvd bs , E A))  → SF' as bs
  freezer    : ∀ {as bs}     → SF' as bs                                                           → SF' as (bs , C (SF (svd'Tosvd as) (svd'Tosvd bs)))

---------------------------------------------------------

substSF : ∀ {as as' bs bs'} → as ≡ as' → bs ≡ bs' → SF as bs → SF as' bs'
substSF refl refl = id

substSFin : ∀ {as as' bs} → as ≡ as' → SF as bs → SF as' bs
substSFin refl = id

substSFout : ∀ {as bs bs'} → bs ≡ bs' → SF as bs → SF as bs'
substSFout refl = id

substSF' : ∀ {as as' bs bs'} → as ≡ as' → bs ≡ bs' → SF' as bs → SF' as' bs'
substSF' refl refl = id

substSF'in : ∀ {as as' bs} → as ≡ as' → SF' as bs → SF' as' bs
substSF'in refl = id

substSF'out : ∀ {as bs bs'} → bs ≡ bs' → SF' as bs → SF' as bs'
substSF'out refl = id

---------------------------------------------------------

weakenIn : ∀ {as as' bs} → as' <: as → SF as bs → SF as' bs
weakenIn {_} {_} {bs} p sf = weaken p (<:-refl {bs}) sf

weakenOut : ∀ {as bs bs'} → bs <: bs' → SF as bs → SF as bs'
weakenOut {as} p = weaken (<:-refl {as}) p

weakenOutIni : ∀ {as bs} → SF as (iniSV bs) → SF as bs
weakenOutIni {as} {bs} = weakenOut (lem-iniSub {bs})

weakenInIni : ∀ {as bs} → SF as bs → SF (iniSV as) bs
weakenInIni {as} = weakenIn (lem-iniSub {as})

weakenSwitchingFunction : ∀ {as bs A} → (A → SF as (iniSV bs , E A)) → A → SF (svd'Tosvd (svdTosvd' as)) (svd'Tosvd (svdTosvd' bs) , E A)
weakenSwitchingFunction {as} {bs} = result (weaken (lem-svd-sub {as}) (lem-svd-ini-sub {bs} , refl))

toSecondC : ∀ {as bs A B} → (A → B) → SF' as (bs , C A) → SF' as (bs , C B)
toSecondC f sf = seq sf (prim (node (λ _ _ bsa → unit , second f bsa) unit))

---------------------------------------------------------

freezeSF : ∀ {as bs} → Δt → SF' as bs → SF (svd'Tosvd as) (svd'Tosvd bs)
freezeSF δ (prim {as} {bs} (node f q)) rewrite lem-svd' {as} | lem-svd' {bs} = prim (λ sa → ((subst (cong2 Node (sym (lem-svd' {as})) (sym (lem-svd' {bs}))) ∘ node f) ∥ sample'ToSample₀ {bs}) (f δ q (sample₀ToSample' {as} sa)))
freezeSF δ (arouter r) = arouter (router'ToRouter r)
freezeSF δ (seq sf₁ sf₂) = seq (freezeSF δ sf₁) (freezeSF δ sf₂)
freezeSF δ (fan sf₁ sf₂) = fan (freezeSF δ sf₁) (freezeSF δ sf₂)
freezeSF δ (rswitcher {as} {bs} sf f) = rswitcher (freezeSF δ sf) (result (substSFout (sv-congR (sym (lem-svd-ini {bs})))) f)
freezeSF δ (freezer {as} sf) = substSFout (sv-congL (cong (C ini) (cong2R SF (lem-svd-ini {as})))) (freezer (freezeSF δ sf))

---------------------------------------------------------

step₀ : ∀ {as bs} → SF as bs → Sample₀ as → SF' (svdTosvd' as) (svdTosvd' bs) × Sample₀ bs
step₀ (prim f) sa = first prim (f sa)
step₀ (arouter r) sa = (arouter (routerToRouter' r) , stepARouter₀ r sa)
step₀ (seq sf₁ sf₂) sa with step₀ sf₁ sa
... | (sf₁' , sb) with step₀ sf₂ sb
...    | (sf₂' , sc) = (seq sf₁' sf₂' , sc)
step₀ (fan sf₁ sf₂) sa with step₀ sf₁ sa | step₀ sf₂ sa
... | (sf₁' , sb) | (sf₂' , sc) = (fan sf₁' sf₂' , (sb , sc))
step₀ {as} {bs} (rswitcher sf f) sa with step₀ sf sa
... | (sf' , (sb , nothing)) = (rswitcher sf' (weakenSwitchingFunction f) , sb)
... | (_   , (_  , just e)) with step₀ (f e) sa
...    | (sf' , (sb , _)) = rswitcher (substSF'out (sv'-congR (lem-svd'-ini {bs})) sf') (weakenSwitchingFunction f) , weakenIniSample {bs} sb
step₀ {as} {bs} (weaken p q sf) sa with step₀ sf (weakenSample₀ {as} p sa)
... | (sf' , sb) = substSF' (sym (lem-svd'-sub-eq p)) (lem-svd'-sub-eq q) sf' , weakenSample₀ {_} {bs} q sb
step₀ (freezer {as} {bs} sf) sa with step₀ sf sa
... | (sf' , sb) = toSecondC (weaken (lem-svd-ini-sub {as}) (lem-svd-sub {bs})) (freezer sf') , sb , weakenInIni {as} sf

---------------------------------------------------------

step' : ∀ {as bs} → Δt → SF' as bs → Sample' as → SF' as bs × Sample' bs
step' δ (prim n) sa = first prim (stepNode δ n sa)
step' δ (arouter r) sa = (arouter r , stepARouter' r sa)
step' δ (seq sf₁ sf₂) sa with step' δ sf₁ sa
... | (sf₁' , sb) with step' δ sf₂ sb
...    | (sf₂' , sc) = (seq sf₁' sf₂' , sc)
step' δ (fan sf₁ sf₂) sa with step' δ sf₁ sa | step' δ sf₂ sa
... | (sf₁' , sb) | (sf₂' , sc) = (fan sf₁' sf₂' , (sb , sc))
step' {as} {bs} δ (rswitcher sf f) sa with step' δ sf sa
... | (sf' , (sb , nothing)) = (rswitcher sf' f , sb)
... | (_   , (_  , just e)) with step₀ (f e) (sample'ToSample₀ {as} sa)
...    | (sf' , (sb , _)) rewrite lem-svd' {as} | lem-svd' {bs} = rswitcher sf' f , sample₀ToSample' {bs} sb
step' δ (freezer sf) sa with step' δ sf sa
... | (sf' , sb) = (freezer sf' , (sb , freezeSF δ sf))

---------------------------------------------------------
