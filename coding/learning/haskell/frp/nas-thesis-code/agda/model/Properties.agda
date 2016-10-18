{-# OPTIONS --type-in-type #-}

module Properties where

open import NeilPrelude
open import RealTime
open import Logic
open import Utilities
open import TimeDeltaList
open import SigVecs

import TemporalLogic renaming (_B_ to _`Back-To`_ ; _S_ to _`Since`_)
open TemporalLogic Time _<ℜ₀_ <ℜ₀-trans <ℜ₀-trich public

import TemporalFunction
open TemporalFunction Time _<ℜ₀_ <ℜ₀-trans <ℜ₀-trich public

-------------------------------------------------------------------------------------------------

-- Side Condition: All change prefixes should be coherent

Coherent : {A : Set} → ChangePrefix A → Set
Coherent cp = {t₁ t₂ : Time} → (t₁ ≤ t₂) → (cp t₁ ≡ takeIncl t₁ (cp t₂))

Coherence : Set
Coherence = {A : Set} → (cp : ChangePrefix A) → Coherent cp

-- Stability was the old name for Coherence

Stable : {A : Set} → ChangePrefix A → Set
Stable = Coherent

Stability : Set
Stability = Coherence

-------------------------------------------------------------------------------------------------

EqContent : {as : SVDesc} → SigVec as → SigVec as → TPred
EqContent {as} s₁ s₂ = content as s₁ ≐ content as s₂

EqAt : {as : SVDesc} → SigVec as → SigVec as → TPred
EqAt {as} = EqContent {as}

EqChange : {as : SVDesc} → SigVec as → SigVec as → TPred
EqChange {as} = EqContent {as}

EqSample : {as : SVDesc} → SigVec as → SigVec as → TPred
EqSample {as} s₁ s₂ = sample {as} s₁ ≐ sample {as} s₂

EqRep : {as : SVDesc} → SigVec as → SigVec as → TPred
EqRep {as} s₁ s₂ = rep {as} s₁ ≐ rep {as} s₂

-------------------------------------------------------------------------------------------------

SampleCausal : {as bs : SVDesc} → SF as bs → Set
SampleCausal {as} {bs} sf = ∀ s₁ s₂ → Always (Hʳ (EqSample {as} s₁ s₂) ⇒ EqSample {bs} (sf s₁) (sf s₂))

RepCausal : {as bs : SVDesc} → SF as bs → Set
RepCausal {as} {bs} sf = ∀ s₁ s₂ → Always (Hʳ (EqRep {as} s₁ s₂) ⇒ EqRep {bs} (sf s₁) (sf s₂))

-------------------------------------------------------------------------------------------------

Causal : {as bs : SVDesc} → SF as bs → Set
Causal {as} {bs} sf = ∀ s₁ s₂ → Always (Hʳ (EqRep {as} s₁ s₂) ⇒ EqRep {bs} (sf s₁) (sf s₂))

Decoupled : {as bs : SVDesc} → SF as bs → Set
Decoupled {as} {bs} sf = ∀ s₁ s₂ → Always (H (EqRep {as} s₁ s₂) ⇒ EqRep {bs} (sf s₁) (sf s₂))

Stateless : {as bs : SVDesc} → SF as bs → Set
Stateless {as} {bs} sf = ∀ s₁ s₂ t₁ t₂ → (sample {as} s₁ t₁ ≡ sample {as} s₂ t₂) → (sample {bs} (sf s₁) t₁ ≡ sample {bs} (sf s₂) t₂)

-------------------------------------------------------------------------------------------------

Earlier : Δt → TPred → TPred
Earlier δ φ t with compareGeq t (δ >0)
... | geq p  = φ (ℜ₀⁺₀-minus t δ p)
... | less p = True

StrictlyDecAux : {as bs : SVDesc} → SF as bs → Δt → Set
StrictlyDecAux {as} {bs} sf δ = ∀ s₁ s₂ → Always (Earlier δ (Hʳ (EqRep {as} s₁ s₂)) ⇒ EqRep {bs} (sf s₁) (sf s₂))

StrictlyDec : {as bs : SVDesc} → SF as bs → Set
StrictlyDec {as} {bs} sf = Σ Δt (StrictlyDecAux {as} {bs} sf)

-------------------------------------------------------------------------------------------------

DiscreteSV : SVDesc → Set
DiscreteSV (C _) = False
DiscreteSV (E _) = True
DiscreteSV (S _) = True
DiscreteSV (as , bs) = DiscreteSV as × DiscreteSV bs

UniqueFixPoint : {as : SVDesc} → SF as as → Set
UniqueFixPoint {as} sf = StrictlyDec {as} {as} sf ⊎ (Decoupled {as} {as} sf × DiscreteSV as)

-------------------------------------------------------------------------------------------------

-- Change Properties

UnchangingCP : {A : Set} → ChangePrefix A → TPred
UnchangingCP cp t = IsNothing (lookupCP cp t)

UnchangingE : {A : Set} → SigVec (E A) → TPred
UnchangingE (ma , _)   O  =  IsNothing ma
UnchangingE (_  , cp)  t  =  UnchangingCP cp t

UnchangingS : {A : Set} → SigVec (S A) → TPred
UnchangingS _         O   =  False
UnchangingS (_ , cp)  t   =  UnchangingCP cp t

UnchangingC : {A : Set} → SigVec (C A) → TPred
UnchangingC s t₁ = P (λ t₀ → ConstantOver s [ t₀ , t₁ ]) t₁

Unchanging : {as : SVDesc} → SigVec as → TPred
Unchanging {C _} s = UnchangingC s
Unchanging {S _} s = UnchangingS s
Unchanging {E _} s = UnchangingE s
Unchanging {as , bs} (s₁ , s₂) = Unchanging {as} s₁ ∧ Unchanging {bs} s₂

AlwaysUnchanging : {as : SVDesc} → SigVec as → Set
AlwaysUnchanging {as} s = Always (Unchanging {as} s)


-- UnchangingOverLO : {as : SVDesc} → SigVec as → (t₁ t₂ : Time) → Set
-- UnchangingOverLO {C _}      s          t₁ t₂ = ConstantOver s [ t₁ , t₂ ]
-- UnchangingOverLO {E _}      (_ , cp)   t₁ t₂ = t₁ < t₂ → cp t₁ ≡ cp t₂
-- UnchangingOverLO {S _}      (_ , cp)   t₁ t₂ = t₁ < t₂ → cp t₁ ≡ cp t₂
-- UnchangingOverLO {as , bs}  (s₁ , s₂)  t₁ t₂ = UnchangingOverLO {as} s₁ t₁ t₂ × UnchangingOverLO {bs} s₂ t₁ t₂

-- UnchangingOver : {as : SVDesc} → SigVec as → Interval → Set
-- UnchangingOver {as} s ⟨ t₁ , t₂ ⟩ = H (UnchangingOverLO {as} s t₁) t₂
-- UnchangingOver {as} s ⟨ t₁ , t₂ ] = UnchangingOverLO {as} s t₁ t₂
-- UnchangingOver {as} s [ t₁ , t₂ ⟩ = Unchanging {as} s t₁ × UnchangingOver {as} s ⟨ t₁ , t₂ ⟩
-- UnchangingOver {as} s [ t₁ , t₂ ] = Unchanging {as} s t₁ × UnchangingOver {as} s ⟨ t₁ , t₂ ]

UnchangingOver : {as : SVDesc} → SigVec as → (t₁ t₂ : Time) → Set
UnchangingOver {C _}      s          t₁ t₂ = ConstantOver s [ t₁ , t₂ ]
UnchangingOver {E _}      (_ , cp)   t₁ t₂ = t₁ < t₂ → cp t₁ ≡ cp t₂
UnchangingOver {S _}      (_ , cp)   t₁ t₂ = t₁ < t₂ → cp t₁ ≡ cp t₂
UnchangingOver {as , bs}  (s₁ , s₂)  t₁ t₂ = UnchangingOver {as} s₁ t₁ t₂ × UnchangingOver {bs} s₂ t₁ t₂

UnchangingOverʳ : {as : SVDesc} → SigVec as → (t₁ t₂ : Time) → Set
UnchangingOverʳ {as} s t₁ t₂ = Unchanging {as} s t₁ × UnchangingOver {as} s t₁ t₂

-- This definition could be misused unless the t₁ ≤ t₂ constraint is added
-- However, UnchangingOverʳ is only used in contexts where t₁ ≤ t₂, so it is omitted to simplify the proofs
-- UnchangingOverʳ : {as : SVDesc} → SigVec as → (t₁ t₂ : Time) → Set
-- UnchangingOverʳ {as} s t₁ t₂ = t₁ ≤ t₂ → Unchanging {as} s t₁ × UnchangingOver {as} s t₁ t₂

UnchangingBetween : {as : SVDesc} → SigVec as → (t₁ t₂ : Time) → Set
UnchangingBetween {as} = UnchangingOver {as}

ChangelessSV : {as : SVDesc} → SigVec as → TPred
ChangelessSV {as} s t = G (UnchangingOver {as} s t) t

ChangelessSVʳ : {as : SVDesc} → SigVec as → TPred
ChangelessSVʳ {as} s = Unchanging {as} s ∧ ChangelessSV {as} s

-------------------------------------------------------------------------------------------------

Changeless : {as bs : SVDesc} → SF as bs → SigVec as → TPred
Changeless {as} {bs} sf s t = ∀ s' → (Hʳ (EqRep {as} s s') ⇒ ChangelessSV {bs} (sf s')) t

Changelessʳ : {as bs : SVDesc} → SF as bs → SigVec as → TPred
Changelessʳ {as} {bs} sf s t = ∀ s' → (H (EqRep {as} s s') ⇒ ChangelessSVʳ {bs} (sf s')) t

ChangeDep : {as bs : SVDesc} → SF as bs → SigVec as → TPred
ChangeDep {as} {bs} sf s t  =  ∀ s' → (Hʳ (EqRep {as} s s') ⇒ G (UnchangingOver {as} s' t ⇒ UnchangingOver {bs} (sf s') t)) t
                                --- (t₂ : Time) → t₂ > t₁ → UnchangingOver {as} s' t₁ t₂ → UnchangingOver {bs} (sf s') t₁ t₂

ChangeDepʳ : {as bs : SVDesc} → SF as bs → SigVec as → TPred
ChangeDepʳ {as} {bs} sf s t₁ =  ∀ s' → H (EqRep {as} s s') t₁ → (t₂ : Time) → t₂ ≥ t₁
                                → UnchangingOverʳ {as} s' t₁ t₂      
                                → UnchangingOverʳ {bs} (sf s') t₁ t₂ 

-- This definition is used in the thesis, but the above (equivalent) definition is easier to work with (TO DO: Prove it)
-- ChangeDepʳ : {as bs : SVDesc} → SF as bs → SigVec as → TPred
-- ChangeDepʳ {as} {bs} sf s t =  ∀ s' → (H (EqRep {as} s s') ⇒ Gʳ (UnchangingOverʳ {as} s' t ⇒ UnchangingOverʳ {bs} (sf s') t)) t


ChangePrp : {as bs : SVDesc} → SF as bs → SigVec as → TPred
ChangePrp {as} {bs} sf s = Gʳ (ChangeDep {as} {bs} sf s) ∧ G (ChangeDepʳ {as} {bs} sf s)

ChangePrpʳ : {as bs : SVDesc} → SF as bs → SigVec as → TPred
ChangePrpʳ {as} {bs} sf s = ChangePrp {as} {bs} sf s ∧ ChangeDepʳ {as} {bs} sf s

ChangeExecʳ : {as bs : SVDesc} → SF as bs → SigVec as → TPred
ChangeExecʳ {as} {bs} sf s t = ∀ s' → H (EqRep {as} s s') t → ∀ t' → t' ≥ t → UnchangingOverʳ {as} s' t t' → UnchangingOverʳ {bs} (sf s') t t'
                               × Gʳ (EqRep {bs} (cut {bs} t t' (sf s')) (sf (cut {as} t t' s'))) t
  where 
        -- cuts out the interval [t₁,t₂⟩
        cut : {xs : SVDesc} → Time → Time → SigVec xs → SigVec xs
        cut {xs} t₁ t₂ sv = splice {xs} sv (advance {xs} t₂ sv) t₁

-------------------------------------------------------------------------------------------------

Source : {as bs : SVDesc} → SF as bs → SigVec as → TPred
Source {as} {bs} sf s t = ∀ s' → Hʳ (EqRep {as} s s') t → Always (EqRep {bs} (sf s) (sf s'))

Sourceʳ : {as bs : SVDesc} → SF as bs → SigVec as → TPred
Sourceʳ {as} {bs} sf s t = ∀ s' → H (EqRep {as} s s') t → Always (EqRep {bs} (sf s) (sf s'))

-- Due to causality, these two sets of definitions are equivalent

-- Source' : {as bs : SVDesc} → SF as bs → SigVec as → TPred
-- Source' {as} {bs} sf s t = ∀ s' → (Hʳ (EqRep {as} s s') ⇒ G (EqRep {bs} (sf s) (sf s'))) t

-- Sourceʳ' : {as bs : SVDesc} → SF as bs → SigVec as → TPred
-- Sourceʳ' {as} {bs} sf s t = ∀ s' → (H (EqRep {as} s s') ⇒ Gʳ (EqRep {bs} (sf s) (sf s'))) t

-------------------------------------------------------------------------------------------------

NoOccs : {A : Set} → SigVec (E A) → TPred
NoOccs s t = fstOcc s t ≡ nothing

FstOcc : {A : Set} → Time × A → SigVec (E A) → TPred
FstOcc e s t = fstOcc s t ≡ just e

NotSwitched : {as bs : SVDesc} → {A : Set} → SF as (bs , E A) → SigVec as → TPred
NotSwitched sf s = NoOccs (snd (sf s))

Switched : {as bs : SVDesc} → {A : Set} → Time × A → SF as (bs , E A) → SigVec as → TPred
Switched e sf s = FstOcc e (snd (sf s))


-- NoOccs : {A : Set} → SigVec (E A) → TPred
-- NoOccs s t = IsNothing (fstOcc s t)

-- Occs : {A : Set} → SigVec (E A) → TPred
-- Occs s t = IsJust (fstOcc s t)

-- NotSwitched : {as bs : SVDesc} → {A : Set} → SF as (bs , E A) → SigVec as → TPred
-- NotSwitched sf s = NoOccs (snd (sf s))

-- Switched : {as bs : SVDesc} → {A : Set} → SF as (bs , E A) → SigVec as → TPred
-- Switched sf s = Occs (snd (sf s))

-------------------------------------------------------------------------------------------------

-- gt5 : SigVec (C Bool)
-- gt5 t = {!t > 5!}

-------------------------------------------------------------------------------------------------

SFRepExt : Set
SFRepExt = {as bs : SVDesc} → {sf₁ sf₂ : SF as bs} → (∀ s → Always (EqRep {bs} (sf₁ s) (sf₂ s))) → sf₁ ≡ sf₂

SFSampleExt : Set
SFSampleExt = {as bs : SVDesc} → {sf₁ sf₂ : SF as bs} → (∀ s → Always (EqSample {bs} (sf₁ s) (sf₂ s))) → sf₁ ≡ sf₂

-------------------------------------------------------------------------------------------------
