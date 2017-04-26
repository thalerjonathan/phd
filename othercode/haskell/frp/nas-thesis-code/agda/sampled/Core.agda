{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module Core where

import RealTime
open RealTime public

import SVDesc
open SVDesc public

---------------------------------------------------------

data Node (as bs : SVDesc) : Set where
  node : ∀ {Q} → (Δt → Q → Sample as → Q × Sample bs) → Q → Node as bs

stepNode : ∀ {as bs} → Δt → Node as bs → Sample as → Node as bs × Sample bs
stepNode δ (node f q) sa = first (node f) (f δ q sa)

---------------------------------------------------------

-- Shallow Embedding

module Shallow where

  shallowId : ∀ {as} → Node as as
  shallowId = node (λ _ _ sa → (unit , sa)) unit

  shallowFst : ∀ {as bs} → Node (as , bs) as
  shallowFst = node (λ _ _ sa → (unit , fst sa)) unit

  shallowSnd : ∀ {as bs} → Node (as , bs) bs
  shallowSnd = node (λ _ _ sa → (unit , snd sa)) unit

  shallowSeq : ∀ {as bs cs} → Node as bs → Node bs cs → Node as cs
  shallowSeq {as} {_} {cs} (node {Q₁} f₁ q₁) (node {Q₂} f₂ q₂) = node seqAux (q₁ , q₂)
    where
      seqAux : Δt → Q₁ × Q₂ → Sample as → (Q₁ × Q₂) × Sample cs
      seqAux δ (q₁ , q₂) sa with f₁ δ q₁ sa
      ... | (q₁' , sb) with f₂ δ q₂ sb
      ...    | (q₂' , sc) = (q₁' , q₂') , sc

  shallowFan : ∀ {as bs cs} → Node as bs → Node as cs → Node as (bs , cs)
  shallowFan {as} {bs} {cs} (node {Q₁} f₁ q₁) (node {Q₂} f₂ q₂) = node fanAux (q₁ , q₂)
    where
      fanAux : Δt → Q₁ × Q₂ → Sample as → (Q₁ × Q₂) × Sample (bs , cs)
      fanAux δ (q₁ , q₂) sa with f₁ δ q₁ sa | f₂ δ q₂ sa
      ... | (q₁' , sb) | (q₂' , sc) = ((q₁' , q₂') , (sb , sc))

  shallowFreeze : ∀ {as bs} → Node as bs → Node as (bs , C (Node as bs))
  shallowFreeze {as} {bs} (node {Q} f q) = node freezeAux q
    where
      freezeAux : Δt → Q → Sample as → Q × Sample (bs , C (Node as bs))
      freezeAux δ q₁ sa with f δ q₁ sa
      ... | (q₂ , sb) = (q₂ , (sb , node f q₁))

  shallowSwitch : {as bs : SVDesc} → {A : Set} → Node as (bs , E A) → (A → Node as bs) → Node as bs
  shallowSwitch {as} {bs} (node {Q} f q) r = node switchAux (inl q)
    where
      stepResidual : Δt → Node as bs → Sample as → (Q ⊎ Node as bs) × Sample bs
      stepResidual δ (node f₂ q₂) sa with f₂ δ q₂ sa
      ... | (q₂' , sb) = inr (node f₂ q₂') , sb

      switchAux : Δt → (Q ⊎ Node as bs) → Sample as → (Q ⊎ Node as bs) × Sample bs
      switchAux δ (inl q₁) sa with f δ q₁ sa
      ... | (q₁' , (sb , nothing)) = (inl q₁' , sb)
      ... | (q₁' , (sb , just e))  = stepResidual δ (r e) sa 
      switchAux δ (inr n) sa = stepResidual δ n sa

---------------------------------------------------------

data AtomicRouter : SVDesc → SVDesc → Set where
  sfId      : ∀ {as}     → AtomicRouter as as
  fstProj   : ∀ {as bs}  → AtomicRouter (as , bs) as
  sndProj   : ∀ {as bs}  → AtomicRouter (as , bs) bs

stepARouter : ∀ {as bs} → AtomicRouter as bs → Sample as → Sample bs
stepARouter sfId     sa         = sa
stepARouter fstProj  (sa₁ , _)  = sa₁
stepARouter sndProj  (_ , sa₂)  = sa₂

---------------------------------------------------------

module Deep where

  data SF : SVDesc → SVDesc → Set where
    prim       : ∀ {as bs}     → Node as bs                         → SF as bs
    arouter    : ∀ {as bs}     → AtomicRouter as bs                 → SF as bs    
    seq        : ∀ {as bs cs}  → SF as bs → SF bs cs                → SF as cs
    fan        : ∀ {as bs cs}  → SF as bs → SF as cs                → SF as (bs , cs)
    switcher   : ∀ {as bs A}   → SF as (bs , E A) → (A → SF as bs)  → SF as bs
    freezer    : ∀ {as bs}     → SF as bs                           → SF as (bs , C (SF as bs))

  -- step : ∀ {as bs} → Δt → SF as bs → Sample as → SF as bs × Sample bs
  -- step δ (prim n) sa = first prim (stepNode δ n sa)
  -- step δ (arouter r) sa = (arouter r , stepARouter r sa)
  -- step δ (seq sf₁ sf₂) sa with step δ sf₁ sa
  -- ... | (sf₁' , sb) with step δ sf₂ sb
  -- ...    | (sf₂' , sc) = (seq sf₁' sf₂' , sc)
  -- step δ (fan sf₁ sf₂) sa with step δ sf₁ sa | step δ sf₂ sa
  -- ... | (sf₁' , sb) | (sf₂' , sc) = (fan sf₁' sf₂' , (sb , sc))
  -- step δ (switcher sf f) sa with step δ sf sa -- note that rswitcher is used below instead
  -- ... | (sf' , (sb , nothing)) = (switcher sf' f , sb)
  -- ... | (_   , (_  , just e))  = step {!O!} (f e) sa
  -- step δ (freezer sf) sa with step δ sf sa
  -- ... | (sf' , sb) = (freezer sf' , (sb , sf))

---------------------------------------------------------

data SF : SVDesc → SVDesc → Set where
  prim       : ∀ {as bs}     → (Sample as → Node as bs × Sample bs)       → SF as bs
  arouter    : ∀ {as bs}     → AtomicRouter as bs                         → SF as bs
  seq        : ∀ {as bs cs}  → SF as bs → SF bs cs                        → SF as cs
  fan        : ∀ {as bs cs}  → SF as bs → SF as cs                        → SF as (bs , cs)
  rswitcher  : ∀ {as bs A}   → SF as (bs , E A) → (A → SF as (bs , E A))  → SF as bs
  freezer    : ∀ {as bs}     → SF as bs                                   → SF as (bs , C (SF as bs))

data SF' : SVDesc → SVDesc → Set where
  prim       : ∀ {as bs}     → Node as bs                                  → SF' as bs
  arouter    : ∀ {as bs}     → AtomicRouter as bs                          → SF' as bs
  seq        : ∀ {as bs cs}  → SF' as bs → SF' bs cs                       → SF' as cs
  fan        : ∀ {as bs cs}  → SF' as bs → SF' as cs                       → SF' as (bs , cs)
  rswitcher  : ∀ {as bs A}   → SF' as (bs , E A) → (A → SF as (bs , E A))  → SF' as bs
  freezer    : ∀ {as bs}     → SF' as bs                                   → SF' as (bs , C (SF as bs))

---------------------------------------------------------

step₀ : ∀ {as bs} → SF as bs → Sample as → SF' as bs × Sample bs
step₀ (prim f) sa = first prim (f sa)
step₀ (arouter r) sa = (arouter r , stepARouter r sa)
step₀ (seq sf₁ sf₂) sa with step₀ sf₁ sa
... | (sf₁' , sb) with step₀ sf₂ sb
...    | (sf₂' , sc) = (seq sf₁' sf₂' , sc)
step₀ (fan sf₁ sf₂) sa with step₀ sf₁ sa | step₀ sf₂ sa
... | (sf₁' , sb) | (sf₂' , sc) = (fan sf₁' sf₂' , (sb , sc))
step₀ (rswitcher sf f) sa with step₀ sf sa
... | (sf' , (sb , nothing)) = (rswitcher sf' f , sb)
... | (_   , (_  , just e)) with step₀ (f e) sa
...    | (sf' , (sb , _)) = (rswitcher sf' f , sb)
step₀ (freezer sf) sa with step₀ sf sa
... | (sf' , sb) = (freezer sf' , (sb , sf))

---------------------------------------------------------

step' : ∀ {as bs} → Δt → SF' as bs → Sample as → SF' as bs × Sample bs
step' δ (prim n) sa = first prim (stepNode δ n sa)
step' δ (arouter r) sa = (arouter r , stepARouter r sa)
step' δ (seq sf₁ sf₂) sa with step' δ sf₁ sa
... | (sf₁' , sb) with step' δ sf₂ sb
...    | (sf₂' , sc) = (seq sf₁' sf₂' , sc)
step' δ (fan sf₁ sf₂) sa with step' δ sf₁ sa | step' δ sf₂ sa
... | (sf₁' , sb) | (sf₂' , sc) = (fan sf₁' sf₂' , (sb , sc))
step' δ (rswitcher sf f) sa with step' δ sf sa
... | (sf' , (sb , nothing)) = (rswitcher sf' f , sb)
... | (_   , (_  , just e)) with step₀ (f e) sa
...    | (sf' , (sb , _)) = (rswitcher sf' f , sb)
step' δ (freezer sf) sa with step' δ sf sa
... | (sf' , sb) = (freezer sf' , (sb , freezeSF δ sf))
  where
    freezeSF : ∀ {as bs} → Δt → SF' as bs → SF as bs
    freezeSF δ (prim n)          = prim (stepNode δ n)
    freezeSF δ (arouter r)       = arouter r
    freezeSF δ (seq sf₁ sf₂)     = seq (freezeSF δ sf₁) (freezeSF δ sf₂)
    freezeSF δ (fan sf₁ sf₂)     = fan (freezeSF δ sf₁) (freezeSF δ sf₂)
    freezeSF δ (rswitcher sf f)  = rswitcher (freezeSF δ sf) f
    freezeSF δ (freezer sf)      = freezer (freezeSF δ sf)

---------------------------------------------------------
