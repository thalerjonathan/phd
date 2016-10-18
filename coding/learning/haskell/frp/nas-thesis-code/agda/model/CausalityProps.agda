{-# OPTIONS --type-in-type #-}

module CausalityProps where

open import NeilPrelude
open import RealTime
open import SigVecs
open import Utilities
open import Properties
open import NaryFRP
open import SigLemmas
open import SFLemmas

-----------------------------------------------------------------------------------

Decoupled→Causal : {as bs : SVDesc} → (sf : SF as bs) → Decoupled {as} {bs} sf → Causal {as} {bs} sf
Decoupled→Causal sf dec s₁ s₂ t (_ , Heq) = dec s₁ s₂ t Heq

StrictlyDec→Decoupled : {as bs : SVDesc} → (sf : SF as bs) → StrictlyDec {as} {bs} sf → Decoupled {as} {bs} sf
StrictlyDec→Decoupled {as} sf (δ , dec) s₁ s₂ t Heq = dec s₁ s₂ t SD→Daux
  where
        SD→Daux : Earlier δ (Hʳ (EqRep {as} s₁ s₂)) t
        SD→Daux with compareGeq t (δ >0)
        ... | geq p = Heq (ℜ₀⁺₀-minus t δ p) (lem-ℜ₀-minus-<-decreasing' p) , reduce-range-< (lem-ℜ₀-minus-<-decreasing' p) Heq
        ... | less p = _

StrictlyDec→Causal : {as bs : SVDesc} → (sf : SF as bs) → StrictlyDec {as} {bs} sf → Causal {as} {bs} sf
StrictlyDec→Causal {as} {bs} sf = Decoupled→Causal {as} {bs} sf ∘ StrictlyDec→Decoupled {as} {bs} sf

Stateless→SampleCausal : {as bs : SVDesc} → (sf : SF as bs) → Stateless {as} {bs} sf → SampleCausal {as} {bs} sf
Stateless→SampleCausal sf stl s₁ s₂ t (eq , _) = stl s₁ s₂ t t eq

----------------------------------------------------------------------------------

-- Preservation of Properties by (>>>)

-- Causal sf₁ × Causal sf₂ → Causal (sf₁ >>> sf₂)
Causal1Causal2→CausalSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {as} {bs} sf₁ → Causal {bs} {cs} sf₂ → Causal {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂)
Causal1Causal2→CausalSeq {as} {bs} sf₁ sf₂ cau₁ cau₂ s₁ s₂ t Heq = cau₂ (sf₁ s₁) (sf₁ s₂) t (lem-Hʳ⇒Hʳ (cau₁ s₁ s₂) t Heq)

-- Stateless sf₁ × Stateless sf₂ → Stateless (sf₁ >>> sf₂)
Stateless1Stateless2→StatelessSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Stateless {as} {bs} sf₁ → Stateless {bs} {cs} sf₂ → Stateless {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂)
Stateless1Stateless2→StatelessSeq sf₁ sf₂ stl₁ stl₂ s₁ s₂ t₁ t₂ = stl₂ (sf₁ s₁) (sf₁ s₂) t₁ t₂ ∘ stl₁ s₁ s₂ t₁ t₂

-- Decoupled sf₁ × Causal sf₂ → Decoupled (sf₁ >>> sf₂)
Decoupled1Causal2→DecoupledSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) 
                                     → Decoupled {as} {bs} sf₁ → Causal {bs} {cs} sf₂ → Decoupled {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂)
Decoupled1Causal2→DecoupledSeq {as} {bs} sf₁ sf₂ dec₁ cau₂ s₁ s₂ t Heq = cau₂ (sf₁ s₁) (sf₁ s₂) t (lem-H⇒Hʳ (dec₁ s₁ s₂) t Heq)

-- Causal sf₁ × Decoupled sf₂ → Decoupled (sf₁ >>> sf₂)
Causal1Decoupled2→DecoupledSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) 
                                     → Causal {as} {bs} sf₁ → Decoupled {bs} {cs} sf₂ → Decoupled {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂)
Causal1Decoupled2→DecoupledSeq {as} {bs} sf₁ sf₂ cau₁ dec₂ s₁ s₂ t Heq = dec₂ (sf₁ s₁) (sf₁ s₂) t (lem-Hʳ⇒H (cau₁ s₁ s₂) t Heq)

-- Decoupled sf₁ × Decoupled sf₂ → Decoupled (sf₁ >>> sf₂)
Decoupled1Decoupled2→DecoupledSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Decoupled {as} {bs} sf₁ → Decoupled {bs} {cs} sf₂ → Decoupled {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂)
Decoupled1Decoupled2→DecoupledSeq {as} {bs} {cs} sf₁ sf₂ dec₁ dec₂ = Decoupled1Causal2→DecoupledSeq {as} {bs} {cs} sf₁ sf₂ dec₁ (Decoupled→Causal {bs} {cs} sf₂ dec₂) 

-- StrictlyDec sf₁ × Causal sf₂ → StrictlyDec (sf₁ >>> sf₂)
StrictlyDec1Causal2→StrictlyDecSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) 
                                     → StrictlyDec {as} {bs} sf₁ → Causal {bs} {cs} sf₂ → StrictlyDec {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂)
StrictlyDec1Causal2→StrictlyDecSeq {as} {bs} {cs} sf₁ sf₂ (δ , dec) cau = δ , SD1C2→SDSeq
  where
        SD1C2→SDSeq : StrictlyDecAux {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) δ
        SD1C2→SDSeq s₁ s₂ t Heq = cau (sf₁ s₁) (sf₁ s₂) t (lem-SD-Hʳ {as} {bs} sf₁ s₁ s₂ δ (dec s₁ s₂) t Heq)

-- Causal sf₁ × StrictlyDec sf₂ → StrictlyDec (sf₁ >>> sf₂)
Causal1StrictlyDec2→StrictlyDecSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) 
                                     → Causal {as} {bs} sf₁ → StrictlyDec {bs} {cs} sf₂ → StrictlyDec {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂)
Causal1StrictlyDec2→StrictlyDecSeq {as} {bs} {cs} sf₁ sf₂ cau (δ , dec) = δ , C1SD2→SDSeq
  where
        C1SD2→SDSeq : StrictlyDecAux {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) δ
        C1SD2→SDSeq s₁ s₂ t Heq with dec (sf₁ s₁) (sf₁ s₂) t
        C1SD2→SDSeq s₁ s₂ t Heq | dec' with compareGeq t (δ >0)
        C1SD2→SDSeq s₁ s₂ t Heq | dec' | less p  = dec' _
        C1SD2→SDSeq s₁ s₂ t Heq | dec' | geq q   = dec' (lem-Hʳ⇒Hʳ (cau s₁ s₂) (ℜ₀⁺₀-minus t δ q) Heq)

-- StrictlyDec sf₁ × StrictlyDec sf₂ → StrictlyDec (sf₁ >>> sf₂)
StrictlyDec1StrictlyDec2→StrictlyDecSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → StrictlyDec {as} {bs} sf₁ → StrictlyDec {bs} {cs} sf₂ → StrictlyDec {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂)
StrictlyDec1StrictlyDec2→StrictlyDecSeq {as} {bs} {cs} sf₁ sf₂ dec₁ dec₂ = StrictlyDec1Causal2→StrictlyDecSeq {as} {bs} {cs} sf₁ sf₂ dec₁ (StrictlyDec→Causal {bs} {cs} sf₂ dec₂) 

-----------------------------------------------------------------------------------

-- Preservation of Properties by (&&&)

-- Causal sf₁ × Causal sf₂ → Causal (sf₁ &&& sf₂)
Causal1Causal2→CausalFan : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF as cs) → Causal {as} {bs} sf₁ → Causal {as} {cs} sf₂ → Causal {as} {bs , cs} (_&&&_ {as} {bs} {cs} sf₁ sf₂)
Causal1Causal2→CausalFan {as} {bs} sf₁ sf₂ = ×-cong5

-- Stateless sf₁ × Stateless sf₂ → Stateless (sf₁ &&& sf₂)
Stateless1Stateless2→StatelessFan : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF as cs) → Stateless {as} {bs} sf₁ → Stateless {as} {cs} sf₂ → Stateless {as} {bs , cs} (_&&&_ {as} {bs} {cs} sf₁ sf₂)
Stateless1Stateless2→StatelessFan {as} {bs} sf₁ sf₂ = ×-cong6

-- Decoupled sf₁ × Decoupled sf₂ → Decoupled (sf₁ &&& sf₂)
Decoupled1Decoupled2→DecoupledFan : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF as cs) → Decoupled {as} {bs} sf₁ → Decoupled {as} {cs} sf₂ → Decoupled {as} {bs , cs} (_&&&_ {as} {bs} {cs} sf₁ sf₂)
Decoupled1Decoupled2→DecoupledFan {as} {bs} sf₁ sf₂ = ×-cong5

-- StrictlyDec sf₁ × StrictlyDec sf₂ → StrictlyDec (sf₁ &&& sf₂)
StrictlyDec1StrictlyDec2→StrictlyDecFan : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF as cs) → StrictlyDec {as} {bs} sf₁ → StrictlyDec {as} {cs} sf₂ → StrictlyDec {as} {bs , cs} (_&&&_ {as} {bs} {cs} sf₁ sf₂)
StrictlyDec1StrictlyDec2→StrictlyDecFan {as} {bs} {cs} sf₁ sf₂ (δ₁ , dec₁) (δ₂ , dec₂) with compareGeqℜ⁺ δ₁ δ₂
... | less p = δ₁ , SDaux1 p
  where
      SDaux1 : δ₁ <ℜ⁺ δ₂ → StrictlyDecAux {as} {bs , cs} (sf₁ & sf₂) δ₁
      SDaux1 p s₁ s₂ t with dec₁ s₁ s₂ t | dec₂ s₁ s₂ t
      SDaux1 p s₁ s₂ t | dec₁' | dec₂' with compareGeq t (δ₁ >0) | compareGeq t (δ₂ >0)
      ... | less q | less r = λ _ → ×-cong (dec₁' _) (dec₂' _)
      ... | less q | geq r  = absurd (<≤ℜ₀-asym (<ℜ₀-trans q p) r)
      ... | geq q  | less r = λ Heq → ×-cong (dec₁' Heq) (dec₂' _)
      ... | geq q  | geq r  = λ Heq → ×-cong (dec₁' Heq) (dec₂' (lem-HeqRep-minus {as} s₁ s₂ (inl p) q r Heq))

... | geq p  = δ₂ , SDaux2 p
  where
      SDaux2 : δ₁ ≥ℜ⁺ δ₂ → StrictlyDecAux {as} {bs , cs} (sf₁ & sf₂) δ₂
      SDaux2 p s₁ s₂ t with dec₁ s₁ s₂ t | dec₂ s₁ s₂ t
      SDaux2 p s₁ s₂ t | dec₁' | dec₂' with compareGeq t (δ₁ >0) | compareGeq t (δ₂ >0)
      ... | less q | less r = λ _ → ×-cong (dec₁' _) (dec₂' _)
      ... | less q | geq r  = λ Heq → ×-cong (dec₁' _) (dec₂' Heq)
      ... | geq q  | less r = absurd (<≤ℜ₀-asym (<≤ℜ₀-trans r (>0-cong-≤ p)) q)
      ... | geq q  | geq r  = λ Heq → ×-cong (dec₁' (lem-HeqRep-minus {as} s₁ s₂ p r q Heq)) (dec₂' Heq)

-----------------------------------------------------------------------------------

-- Preservation of Properties by (freeze)

-- Causal sf → Causal (freeze sf)
Causal→CausalFreeze : {as bs : SVDesc} → Extensionality → (sf : SF as bs) → Causal {as} {bs} sf → Causal {as} {bs , C (SF as bs)} (freeze {as} {bs} sf)
Causal→CausalFreeze {as} {bs} ext sf cau s₁ s₂ t (eq , Heq) = ×-cong (cau s₁ s₂ t (eq , Heq)) (ext (λ s → lem-Heq-splice-advance {as} {bs} ext sf s₁ s₂ s t Heq))

-- Decoupled sf → Decoupled (freeze sf)
Decoupled→DecoupledFreeze : {as bs : SVDesc} → Extensionality → (sf : SF as bs) → Decoupled {as} {bs} sf → Decoupled {as} {bs , C (SF as bs)} (freeze {as} {bs} sf)
Decoupled→DecoupledFreeze {as} {bs} ext sf dec s₁ s₂ t Heq = ×-cong (dec s₁ s₂ t Heq) (ext (λ s → lem-Heq-splice-advance {as} {bs} ext sf s₁ s₂ s t Heq))

-- Stateless sf → Stateless (freeze sf)
Stateless→StatelessFreeze : {as bs : SVDesc} → SFSampleExt → Stability → (sf : SF as bs) → Stateless {as} {bs} sf → Stateless {as} {bs , C (SF as bs)} (freeze {as} {bs} sf)
Stateless→StatelessFreeze {as} {bs} ext stab sf stl s₁ s₂ t₁ t₂ eq = ×-cong (stl s₁ s₂ t₁ t₂ eq) (ext {as} {bs} (λ s t → lem-sample-advance {as} {bs} stab s₁ s₂ s t₁ t₂ t sf stl eq))

-- Causal sf → (∀ s t → Causal (frozenSample sf s t))
Causal→CausalFrozen : {as bs : SVDesc} → Stability → (sf : SF as bs) → Causal {as} {bs} sf → ∀ s t → Causal {as} {bs} (frozenSample {as} {bs} sf s t)
Causal→CausalFrozen {as} {bs} stab sf cau s t s₁ s₂ = lem-cau-rep-advance-splice {as} {bs} stab sf cau s₁ s₂ s t

-- Decoupled sf → (∀ s t → Decoupled (frozenSample sf s t))
Decoupled→DecoupledFrozen : {as bs : SVDesc} → Stability → (sf : SF as bs) → Decoupled {as} {bs} sf → ∀ s t → Decoupled {as} {bs} (frozenSample {as} {bs} sf s t)
Decoupled→DecoupledFrozen {as} {bs} stab sf dec s t s₁ s₂ = lem-dec-rep-advance-splice {as} {bs} stab sf dec s₁ s₂ s t

-- StrictlyDec sf → (∀ s t → StrictlyDec (frozenSample sf s t))
postulate StrictlyDec→StrictlyDecFrozen : {as bs : SVDesc} → Stability → (sf : SF as bs) → StrictlyDec {as} {bs} sf → (∀ s t → StrictlyDec {as} {bs} (frozenSample {as} {bs} sf s t))

-- Stateless sf → (∀ s t → Stateless (frozenSample sf s t))
Stateless→StatelessFrozen : {as bs : SVDesc} → Stability → (sf : SF as bs) → Stateless {as} {bs} sf → ∀ s t → Stateless {as} {bs} (frozenSample {as} {bs} sf s t)
Stateless→StatelessFrozen {as} {bs} stab sf stl s t s₁ s₂ = lem-stl-sample-advance-splice {as} {bs} stab sf stl s₁ s₂ s t

-----------------------------------------------------------------------------------

-- Preservation of Properties by (switch)

-- Causal sf × Causal (∀ e → f e) → Causal (switch sf f)
postulate Causal1Causal2→CausalSwitch : {as bs : SVDesc} → {A : Set} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → Causal {as} {bs , E A} sf → (∀ e → Causal {as} {bs} (f e)) → Causal {as} {bs} (switch {as} {bs} sf f)

-- Decoupled sf × Decoupled (∀ e → f e) → Decoupled (switch sf f)
postulate Decoupled1Decoupled2→DecoupledSwitch : {as bs : SVDesc} → {A : Set} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → Decoupled {as} {bs , E A} sf → (∀ e → Decoupled {as} {bs} (f e)) → Decoupled {as} {bs} (switch {as} {bs} sf f)

-----------------------------------------------------------------------------------
