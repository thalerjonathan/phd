{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module CoreD where

import RealTime
open RealTime public

import SVDesc
open SVDesc public

import Decoupled
open Decoupled public

---------------------------------------------------------

data Node (as bs : SVDesc) : Dec → Set where
  cnode  : ∀ {Q} → (Δt → Q →  Sample as → Q  × Sample bs)  → Q → Node as bs cau
  dnode  : ∀ {Q} → (Δt → Q → (Sample as → Q) × Sample bs)  → Q → Node as bs dec

stepNode : ∀ {as bs d} → Δt → Node as bs d → Sample as → Node as bs d × Sample bs
stepNode δ (cnode f q) sa  = first (cnode f) (f δ q sa)
stepNode δ (dnode f q) sa  = first (λ g → dnode f (g sa)) (f δ q)

dstepNode : ∀ {as bs} → Δt → Node as bs dec → (Sample as → Node as bs dec) × Sample bs
dstepNode δ (dnode f q) = first (λ g sa → dnode f (g sa)) (f δ q)

---------------------------------------------------------

data AtomicRouter : SVDesc → SVDesc → Set where
    sfId      : ∀ {as}     → AtomicRouter as as
    fstProj   : ∀ {as bs}  → AtomicRouter (as , bs) as
    sndProj   : ∀ {as bs}  → AtomicRouter (as , bs) bs

stepARouter : ∀ {as bs} → AtomicRouter as bs → Sample as → Sample bs
stepARouter sfId    sa         = sa
stepARouter fstProj (sa₁ , _)  = sa₁
stepARouter sndProj (_ , sa₂)  = sa₂

---------------------------------------------------------

data SF : SVDesc → SVDesc → Dec → Set where
  cprim      : ∀ {as bs} → (Sample as → Node as bs cau  × Sample bs)           → SF as bs cau
  dprim      : ∀ {as bs} → (Sample as → Node as bs dec) → Sample bs            → SF as bs dec
  arouter    : ∀ {as bs} → AtomicRouter as bs                                  → SF as bs cau
  seq        : ∀ {d₁ d₂ as bs cs} → SF as bs d₁ → SF bs cs d₂                  → SF as cs (d₁ ∨ d₂)
  fan        : ∀ {d₁ d₂ as bs cs} → SF as bs d₁ → SF as cs d₂                  → SF as (bs , cs) (d₁ ∧ d₂)
  rswitcher  : ∀ {d₁ d₂ as bs A} → SF as (bs , E A) d₁ → (A → SF as (bs , E A) d₂) → SF as bs (d₁ ∧ d₂)
  freezer    : ∀ {d as bs} → SF as bs d                                        → SF as (bs , C (SF as bs d)) d
  looper     : ∀ {d as bs cs} → SF (as , cs) bs d → SF bs cs dec               → SF as bs d
  weakener   : ∀ {d as bs} → SF as bs d                                        → SF as bs cau

data SF' : SVDesc → SVDesc → Dec → Set where
  prim       : ∀ {d as bs} → Node as bs d                                        → SF' as bs d
  arouter    : ∀ {as bs} → AtomicRouter as bs                                    → SF' as bs cau
  seq        : ∀ {d₁ d₂ as bs cs} → SF' as bs d₁ → SF' bs cs d₂                  → SF' as cs (d₁ ∨ d₂)
  fan        : ∀ {d₁ d₂ as bs cs} → SF' as bs d₁ → SF' as cs d₂                  → SF' as (bs , cs) (d₁ ∧ d₂)
  rswitcher  : ∀ {d₁ d₂ as bs A} → SF' as (bs , E A) d₁ → (A → SF as (bs , E A) d₂)  → SF' as bs (d₁ ∧ d₂)
  freezer    : ∀ {d as bs} → SF' as bs d                                         → SF' as (bs , C (SF as bs d)) d
  looper     : ∀ {d as bs cs} → SF' (as , cs) bs d → SF' bs cs dec               → SF' as bs d
  weakener   : ∀ {d as bs} → SF' as bs d                                         → SF' as bs cau

---------------------------------------------------------

weakenSwitch : ∀ {as bs} → (d₁ d₂ : Dec) → SF' as bs (d₂ ∧ d₂) → SF' as bs (d₁ ∧ d₂)
weakenSwitch cau _   = weakener
weakenSwitch dec cau = id
weakenSwitch dec dec = id

---------------------------------------------------------

mutual

  step₀ : ∀ {d as bs} → SF as bs d → Sample as → SF' as bs d × Sample bs
  step₀ (cprim f) sa = first prim (f sa)
  step₀ (dprim f sb) sa = (prim (f sa) , sb)
  step₀ (arouter r) sa = (arouter r , stepARouter r sa)
  step₀ (seq sf₁ sf₂) sa with step₀ sf₁ sa
  ...  | (sf₁' , sb) with step₀ sf₂ sb
  ...     | (sf₂' , sc) = (seq sf₁' sf₂' , sc)
  step₀ (fan sf₁ sf₂) sa with step₀ sf₁ sa | step₀ sf₂ sa
  ... | (sf₁' , sb) | (sf₂' , sc) = (fan sf₁' sf₂' , (sb , sc))
  step₀ (rswitcher {d₁} {d₂} sf f) sa with step₀ sf sa
  ...  | (sf' , (sb , nothing)) = (rswitcher sf' f , sb)
  ...  | (_   , (_  , just e)) with step₀ (f e) sa
  ...     | (sf' , (sb , _)) = (weakenSwitch d₁ d₂ (rswitcher sf' f) , sb)
  step₀ (freezer sf) sa with step₀ sf sa
  ... | (sf' , sb) = (freezer sf' , (sb , sf))
  step₀ (looper sff sfb) sa with dstep₀ sfb
  ...  | (g , sc) with step₀ sff (sa , sc)
  ...     | (sff' , sb) = (looper sff' (g sb) , sb)
  step₀ (weakener sf) sa = first weakener (step₀ sf sa)

  dstep₀ : ∀ {as bs} → SF as bs dec → (Sample as → SF' as bs dec) × Sample bs
  dstep₀ sf = dstepAux₀ sf refl

  dstepAux₀ : ∀ {d as bs} → SF as bs d → d ≡ dec → (Sample as → SF' as bs dec) × Sample bs

  dstepAux₀ (cprim f) ()

  dstepAux₀ (dprim f sb) refl = (prim ∘ f , sb)

  dstepAux₀ (arouter r) ()

  dstepAux₀ (seq {dec} sf₁ sf₂) refl with dstep₀ sf₁
  ...  | (g , sb) with step₀ sf₂ sb
  ...     | (sf₂' , sc) = ((λ sa → seq (g sa) sf₂') , sc)
  dstepAux₀ (seq {cau} {.dec} {as} {bs} {cs} sf₁ sf₂) refl with dstep₀ sf₂
  ... | (g , sc) = (aux , sc)
                   where  aux : Sample as → SF' as cs dec
                          aux sa with step₀ sf₁ sa
                          ... | (sf₁' , sb) = seq sf₁' (g sb)

  dstepAux₀ (fan {cau} sf₁ sf₂) ()
  dstepAux₀ (fan {dec} sf₁ sf₂) refl with dstep₀ sf₁ | dstep₀ sf₂
  ... | (g₁ , sb) | (g₂ , sc) = ((λ sa → fan (g₁ sa) (g₂ sa)) , (sb , sc))

  dstepAux₀ (rswitcher {cau} sf f) ()
  dstepAux₀ (rswitcher {dec} sf f) refl with dstep₀ sf
  ...  | (g , (sb , nothing)) = ((λ sa → rswitcher (g sa) f) , sb)
  ...  | (_ , (_ , just e)) with dstep₀ (f e)
  ...     |  (g , (sb , _)) = ((λ sa → rswitcher (g sa) f) , sb)

  dstepAux₀ (freezer sf) refl with dstep₀ sf
  ... | (g , sb) = (freezer ∘ g , (sb , sf))

  dstepAux₀ (looper sff sfb) refl with dstep₀ sff
  ...  | (g , sb) with step₀ sfb sb
  ...     | (sfb' , sc) = ((λ sa → looper (g (sa , sc)) sfb') , sb)

  dstepAux₀ (weakener sf) ()

---------------------------------------------------------

freezeSF : ∀ {d as bs} → Δt → SF' as bs d → SF as bs d
freezeSF δ (arouter r)      = arouter r
freezeSF δ (seq sf₁ sf₂)    = seq (freezeSF δ sf₁) (freezeSF δ sf₂)
freezeSF δ (fan sf₁ sf₂)    = fan (freezeSF δ sf₁) (freezeSF δ sf₂)
freezeSF δ (rswitcher sf f) = rswitcher (freezeSF δ sf) f
freezeSF δ (freezer sf)     = freezer (freezeSF δ sf)
freezeSF δ (looper sff sfb) = looper (freezeSF δ sff) (freezeSF δ sfb)
freezeSF δ (weakener sf)    = weakener (freezeSF δ sf)
freezeSF {cau} δ (prim n)   = cprim (stepNode δ n)
freezeSF {dec} δ (prim n)   = uncurry dprim (dstepNode δ n)

---------------------------------------------------------

mutual

  step' : ∀ {d as bs} → Δt → SF' as bs d → Sample as → SF' as bs d × Sample bs
  step' δ (prim n) sa = first prim (stepNode δ n sa)
  step' δ (arouter r) sa = (arouter r , stepARouter r sa)
  step' δ (seq sf₁ sf₂) sa with step' δ sf₁ sa
  ... | (sf₁' , sb) with step' δ sf₂ sb
  ...    | (sf₂' , sc) = (seq sf₁' sf₂' , sc)
  step' δ (fan sf₁ sf₂) sa with step' δ sf₁ sa | step' δ sf₂ sa
  ... | (sf₁' , sb) | (sf₂' , sc) = (fan sf₁' sf₂' , (sb , sc))
  step' δ (rswitcher {d₁} {d₂} sf f) sa with step' δ sf sa
  ... | (sf' , (sb , nothing)) = (rswitcher sf' f , sb)
  ... | (_   , (_  , just e)) with step₀ (f e) sa
  ...    | (sf' , (sb , _)) = (weakenSwitch d₁ d₂ (rswitcher sf' f) , sb)
  step' δ (freezer sf) sa with step' δ sf sa
  ... | (sf' , sb) = (freezer sf' , (sb , freezeSF δ sf))
  step' δ (looper sff sfb) sa with dstep' δ sfb
  ... | (g , sc) with step' δ sff (sa , sc)
  ...    | (sff' , sb) = (looper sff' (g sb) , sb)
  step' δ (weakener sf) sa = first weakener (step' δ sf sa)


  dstep' : ∀ {as bs} → Δt → SF' as bs dec → (Sample as → SF' as bs dec) × Sample bs
  dstep' δ sf = dstepAux' δ sf refl

  dstepAux' : ∀ {d as bs} → Δt → SF' as bs d → d ≡ dec → (Sample as → SF' as bs dec) × Sample bs

  dstepAux' δ (prim n) refl = (first ∘ result) prim (dstepNode δ n)

  dstepAux' δ (arouter r) ()

  dstepAux' δ (seq {dec} sf₁ sf₂) refl with dstep' δ sf₁
  ... | (g , sb) with step' δ sf₂ sb
  ...    | (sf₂' , sc) = ((λ sa → seq (g sa) sf₂') , sc)
  dstepAux' δ (seq {cau} {._} {as} {_} {cs} sf₁ sf₂) refl with dstep' δ sf₂
  ... | (g , sc) = (aux , sc)
                   where aux : Sample as → SF' as cs dec
                         aux sa with step' δ sf₁ sa
                         ... | (sf₁' , sb) = seq sf₁' (g sb)

  dstepAux' δ (fan {cau} sf₁ sf₂) ()
  dstepAux' δ (fan {dec} sf₁ sf₂) refl with dstep' δ sf₁ | dstep' δ sf₂
  ... | (g₁ , sb) | (g₂ , sc) = ((λ sa → fan (g₁ sa) (g₂ sa)) , (sb , sc))

  dstepAux' δ (rswitcher {cau} sf f) ()
  dstepAux' δ (rswitcher {dec} {cau} sf f) ()
  dstepAux' δ (rswitcher {dec} {dec} sf f) refl with dstep' δ sf
  ... | (g , (sb , nothing)) = ((λ sa → rswitcher (g sa) f) , sb)
  ... | (_ , (_ , just e)) with dstep₀ (f e)
  ...    | (g , (sb , _)) = ((λ sa → rswitcher (g sa) f) , sb)

  dstepAux' δ (freezer sf) refl with dstep' δ sf
  ... | (g , sb) = (freezer ∘ g , (sb , freezeSF δ sf))

  dstepAux' δ (looper sff sfb) refl with dstep' δ sff
  ... | (g , sb) with step' δ sfb sb
  ...    | (sfb' , sc) = ((λ sa → looper (g (sa , sc)) sfb') , sb)

  dstepAux' δ (weakener sf) ()

---------------------------------------------------------
