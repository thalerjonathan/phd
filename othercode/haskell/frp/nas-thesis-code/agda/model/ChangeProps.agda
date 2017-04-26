{-# OPTIONS --type-in-type #-}

module ChangeProps where

open import NeilPrelude
open import RealTime
open import Logic
open import SigVecs
open import Properties
open import Utilities
open import NaryFRP
open import SigLemmas
open import SFLemmas

-----------------------------------------------------------------------------------

-- Changeless sf s   ⇒  G (Changeless sf s)
-- Changelessʳ sf s  ⇒  G (Changelessʳ sf s)
-- ChangePrp sf s    ⇒  G (ChangePrp sf s)
-- ChangePrpʳ sf s   ⇒  G (ChangePrpʳ sf s)
-- Source sf s       ⇒  G (Source sf s)
-- Sourceʳ sf s      ⇒  G (Sourceʳ sf s)

-- Changelessʳ sf s  ⇒  Changeless sf s
-- Changelessʳ sf s  ⇒  ChangePrpʳ sf s
-- Changelessʳ sf s  ⇒  ChangeExecʳ sf s
-- Changelessʳ sf s  ⇒  Sourceʳ sf s
-- Changeless sf s   ⇒  ChangePrp sf s
-- Changeless sf s   ⇒  Source sf s
-- ChangePrpʳ sf s   ⇒  ChangeDepʳ sf s
-- ChangePrpʳ sf s   ⇒  ChangePrp sf s
-- ChangePrp sf s    ⇒  ChangeDep sf s
-- ChangeExecʳ sf s  ⇒  ChangeDepʳ sf s
-- Sourceʳ sf s      ⇒  Source sf s

-- ChangeDep sf ∧ Source sf  ⇒ ChangelessSF sf

-----------------------------------------------------------------------------------

Changeless⇒GChangeless : {as bs : SVDesc} → Stability → (sf : SF as bs) → (s : SigVec as) → Always (Changeless {as} {bs} sf s ⇒ G (Changeless {as} {bs} sf s))
Changeless⇒GChangeless {_} {bs} stab sf s t₀ ch t₁ lt₁ s' Heq t₂ lt₂ = lem-UB-reduceR {bs} stab (sf s') (inl lt₁) (inl lt₂) (ch s' (lem-H⇒HHʳ t₁ (snd Heq) t₀ lt₁) t₂ (<ℜ₀-trans lt₁ lt₂)) 

Changelessʳ⇒GChangelessʳ : {as bs : SVDesc} → Stability → (sf : SF as bs) → (s : SigVec as) → Always (Changelessʳ {as} {bs} sf s ⇒ G (Changelessʳ {as} {bs} sf s))
Changelessʳ⇒GChangelessʳ {as} {bs} stab sf s t₀ ch t₁ lt₁ s' Heq with ch s' (reduce-range-< lt₁ Heq)
... | _ , Gunb = lem-UB→U {bs} stab (sf s') (lt₁ , inr refl) (Gunb t₁ lt₁) , λ t₂ lt₂ → lem-UB-reduceR {bs} stab (sf s') (inl lt₁) (inl lt₂) (Gunb t₂ (<ℜ₀-trans lt₁ lt₂))

ChangePrp⇒GChangePrp : {as bs : SVDesc} → Stability → (sf : SF as bs) → (s : SigVec as) → Always (ChangePrp {as} {bs} sf s ⇒ G (ChangePrp {as} {bs} sf s))
ChangePrp⇒GChangePrp stab sf s t₀ ((chd , Gchd) , Gchdr) t₁ lt₁ = (Gchd t₁ lt₁ , λ t₂ lt₂ → Gchd t₂ (<ℜ₀-trans lt₁ lt₂)) , λ t₂ lt₂ → Gchdr t₂ (<ℜ₀-trans lt₁ lt₂)

ChangePrpʳ⇒GChangePrpʳ : {as bs : SVDesc} → Stability → (sf : SF as bs) → (s : SigVec as) → Always (ChangePrpʳ {as} {bs} sf s ⇒ G (ChangePrpʳ {as} {bs} sf s))
ChangePrpʳ⇒GChangePrpʳ stab sf s t₀ (((chd , Gchd) , Gchdr) , chdr) t₁ lt₁ =
   ((Gchd t₁ lt₁ , λ t₂ lt₂ → Gchd t₂ (<ℜ₀-trans lt₁ lt₂)) , λ t₂ lt₂ → Gchdr t₂ (<ℜ₀-trans lt₁ lt₂)) , Gchdr t₁ lt₁

Source⇒GSource : {as bs : SVDesc} → Stability → (sf : SF as bs) → (s : SigVec as) → Always (Source {as} {bs} sf s ⇒ G (Source {as} {bs} sf s))
Source⇒GSource stab sf s t₀ src t₁ lt₁ s' Heq = src s' (lem-H⇒HHʳ t₁ (snd Heq) t₀ lt₁)

Sourceʳ⇒GSourceʳ : {as bs : SVDesc} → Stability → (sf : SF as bs) → (s : SigVec as) → Always (Sourceʳ {as} {bs} sf s ⇒ G (Sourceʳ {as} {bs} sf s))
Sourceʳ⇒GSourceʳ stab sf s t₀ src t₁ lt₁ s' Heq = src s' (lem-H⇒HH t₁ Heq t₀ lt₁)

-----------------------------------------------------------------------------------

Changelessʳ⇒Changeless : {as bs : SVDesc} → (sf : SF as bs) → (s : SigVec as) → Always (Changelessʳ {as} {bs} sf s ⇒ Changeless {as} {bs} sf s)
Changelessʳ⇒Changeless {_} {bs} sf s t ch s' (eq , Heq)  = snd (ch s' Heq) 

ChangePrpʳ⇒ChangePrp : {as bs : SVDesc} → (sf : SF as bs) → (s : SigVec as) → Always (ChangePrpʳ {as} {bs} sf s ⇒ ChangePrp {as} {bs} sf s)
ChangePrpʳ⇒ChangePrp sf s t (chp , chd) = chp

ChangePrp⇒ChangeDep : {as bs : SVDesc} → (sf : SF as bs) → (s : SigVec as) → Always (ChangePrp {as} {bs} sf s ⇒ ChangeDep {as} {bs} sf s)
ChangePrp⇒ChangeDep sf s t ((chd , Gchd) , chdr) = chd

ChangePrpʳ⇒ChangeDepʳ : {as bs : SVDesc} → (sf : SF as bs) → (s : SigVec as) → Always (ChangePrpʳ {as} {bs} sf s ⇒ ChangeDepʳ {as} {bs} sf s)
ChangePrpʳ⇒ChangeDepʳ sf s t (chp , chd) = chd

Sourceʳ⇒Source : {as bs : SVDesc} → (sf : SF as bs) → (s : SigVec as) → Always (Sourceʳ {as} {bs} sf s ⇒ Source {as} {bs} sf s)
Sourceʳ⇒Source sf s t₁ src s' (eq , Heq) = src s' Heq

Changeless⇒Source : {as bs : SVDesc} → (sf : SF as bs) → Causal {as} {bs} sf → (s : SigVec as) → Always (Changeless {as} {bs} sf s ⇒ Source {as} {bs} sf s)
Changeless⇒Source {as} {bs} sf cau s t ch s' Heq = lem-HʳEqRep→C→AlwaysEqRep {as} {bs} sf cau s s' t Heq (ch s (refl , λ _ _ → refl)) (ch s' Heq)

Changelessʳ⇒Sourceʳ : {as bs : SVDesc} → Stability → (sf : SF as bs) → Causal {as} {bs} sf → (s : SigVec as) → Always (Changelessʳ {as} {bs} sf s ⇒ Sourceʳ {as} {bs} sf s)
Changelessʳ⇒Sourceʳ {as} {bs} stab sf cau s t ch s' Heq = lem-HEqRep→Cʳ→AlwaysEqRep {as} {bs} stab sf cau s s' t Heq (ch s (λ _ _ → refl)) (ch s' Heq)

Changeless⇒ChangePrp : {as bs : SVDesc} → Stability → (sf : SF as bs) → (s : SigVec as) → Always (Changeless {as} {bs} sf s ⇒ ChangePrp {as} {bs} sf s)
Changeless⇒ChangePrp {_} {bs} stab sf s t₀ ch =
  ((λ s' Heq t₂ lt _ → ch s' Heq t₂ lt) ,
   (λ t₁ lt₁ s' Heq t₂ lt₂ _ → lem-UB-reduceR {bs} stab (sf s') (inl lt₁) (inl lt₂) (ch s' (lem-H⇒HHʳ t₁ (snd Heq) t₀ lt₁) t₂ (<ℜ₀-trans lt₁ lt₂)))) ,
   (λ t₁ lt₁ s' Heq t₂ lt₂ unch → lem-UB→U {bs} stab (sf s') (lt₁ , lt₂) (ch s' (lem-H⇒HHʳ t₁ Heq t₀ lt₁) t₂ (<≤ℜ₀-trans lt₁ lt₂)) ,
                                  lem-UB-reduceR {bs} stab (sf s') (inl lt₁) lt₂ (ch s' (lem-H⇒HHʳ t₁ Heq t₀ lt₁) t₂ (<≤ℜ₀-trans lt₁ lt₂)))

Changelessʳ⇒ChangePrpʳ : {as bs : SVDesc} → Stability → (sf : SF as bs) → (s : SigVec as) → Always (Changelessʳ {as} {bs} sf s ⇒ ChangePrpʳ {as} {bs} sf s)
Changelessʳ⇒ChangePrpʳ {as} {bs} stab sf s t₀ ch = Changeless⇒ChangePrp {as} {bs} stab sf s t₀ (Changelessʳ⇒Changeless {as} {bs} sf s t₀ ch) ,
                                                   λ s' Heq t₂ lt _ → fst (ch s' Heq) , case (snd (ch s' Heq) t₂) (λ eq → subst (cong (UnchangingBetween {bs} (sf s') t₀) eq) (lem-UB {bs} (sf s') (inr refl))) lt

ChangeExecʳ⇒ChangeDepʳ : {as bs : SVDesc} → (sf : SF as bs) → (s : SigVec as) → Always (ChangeExecʳ {as} {bs} sf s ⇒ ChangeDepʳ {as} {bs} sf s)
ChangeExecʳ⇒ChangeDepʳ sf s t ch s' Heq t' p un  = fst (ch s' Heq t' p un)

-- Changelessʳ⇒ChangeExecʳ : {as bs : SVDesc} → Stability → (sf : SF as bs) → (s : SigVec as) → Always (Changelessʳ {as} {bs} sf s ⇒ ChangeExecʳ {as} {bs} sf s)
-- Changelessʳ⇒ChangeExecʳ {as} {bs} stab sf s t₀ ch = {!!}

ChangeDep∧Source⇒Changeless : {as bs : SVDesc} → Stability → (sf : SF as bs) → (s : SigVec as) → Always (ChangeDep {as} {bs} sf s ⇒ Source {as} {bs} sf s ⇒ Changeless {as} {bs} sf s)
ChangeDep∧Source⇒Changeless {as} {bs} = lem-CD⇒Src⇒C {as} {bs}

-- This doesn't hold!
-- ChangeDepʳ sf ∧ Sourceʳ sf ⇒ ChangelessSFʳ sf
-- consider nowS:

-- nowS : SF (S Unit) (E Unit)
-- nowS = const (just unit , const [])

-----------------------------------------------------------------------------------

-- Changeless sf₁ s ∧ ChangeDep sf₂ (sf₁ s) ⇒ Changeless (sf₁ >>> sf₂) s
Changeless1∧ChangeDep2⇒Changeless : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {as} {bs} sf₁ → (s : SigVec as) → Always (Changeless {as} {bs} sf₁ s ⇒ ChangeDep {bs} {cs} sf₂ (sf₁ s) ⇒ Changeless {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
Changeless1∧ChangeDep2⇒Changeless sf₁ sf₂ cau s t₁ ch chd s' Heq t₂ lt = chd (sf₁ s') (lem-Hʳ⇒Hʳ (cau s s') t₁ Heq) t₂ lt (ch s' Heq t₂ lt)

-- Changelessʳ sf₁ s ∧ ChangeDepʳ sf₂ (sf₁ s) ⇒ Changelessʳ (sf₁ >>> sf₂) s
Changelessʳ1∧ChangeDepʳ2⇒Changelessʳ : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {as} {bs} sf₁ → (s : SigVec as) → Always (Changelessʳ {as} {bs} sf₁ s ⇒ ChangeDepʳ {bs} {cs} sf₂ (sf₁ s) ⇒ Changelessʳ {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
Changelessʳ1∧ChangeDepʳ2⇒Changelessʳ {as} {bs} {cs} sf₁ sf₂ cau s t₁ ch chd s' Heq with ch s' Heq | lem-Hʳ⇒H (cau s s') t₁ Heq
... | un , Gunb | Heqb = fst (chd (sf₁ s') Heqb t₁ (inr refl) (un , lem-UB {bs} (sf₁ s') (inr refl))) , λ t₂ lt → snd (chd (sf₁ s') Heqb t₂ (inl lt) (un , Gunb t₂ lt))

----------------------------------------------------------------------------------

-- Preservation of Properties by (>>>)

-- Source sf₁ s ∧ Source sf₂ (sf₁ s) ⇒ Source (sf₁ >>> sf₂) s
Source1Source2⇒SourceSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → (s : SigVec as) → Always (Source {as} {bs} sf₁ s ⇒ Source {bs} {cs} sf₂ (sf₁ s) ⇒ Source {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
Source1Source2⇒SourceSeq sf₁ sf₂ s t src₁ src₂ s' Heq = src₂ (sf₁ s') (lem-Always⇒Hʳ (src₁ s' Heq) t)

-- Causal sf₁ → Always (Source sf₂ (sf₁ s) ⇒ Source (sf₁ >>> sf₂) s)
Source1Causal2⇒SourceSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {bs} {cs} sf₂ → (s : SigVec as) → Always (Source {as} {bs} sf₁ s ⇒ Source {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
Source1Causal2⇒SourceSeq sf₁ sf₂ cau s t src s' Heq = lem-Hʳ⇒Always (cau (sf₁ s) (sf₁ s')) (src s' Heq)

-- Causal sf₂ → Always (Source sf₁ s ⇒ Source (sf₁ >>> sf₂) s)
Causal1Source2⇒SourceSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {as} {bs} sf₁ → (s : SigVec as) → Always (Source {bs} {cs} sf₂ (sf₁ s) ⇒ Source {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
Causal1Source2⇒SourceSeq sf₁ sf₂ cau s t src s' Heq = src (sf₁ s') (lem-Hʳ⇒Hʳ (cau s s') t Heq)

-- Sourceʳ sf₁ s ∧ Sourceʳ sf₂ (sf₁ s) ⇒ Sourceʳ (sf₁ >>> sf₂) s
Sourceʳ1Sourceʳ2⇒SourceʳSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → (s : SigVec as) → Always (Sourceʳ {as} {bs} sf₁ s ⇒ Sourceʳ {bs} {cs} sf₂ (sf₁ s) ⇒ Sourceʳ {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
Sourceʳ1Sourceʳ2⇒SourceʳSeq sf₁ sf₂ s t src₁ src₂ s' Heq = src₂ (sf₁ s') (lem-Always⇒H (src₁ s' Heq) t)

-- Causal sf₁ → Always (Sourceʳ sf₂ (sf₁ s) ⇒ Sourceʳ (sf₁ >>> sf₂) s)
Sourceʳ1Causal2⇒SourceʳSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {bs} {cs} sf₂ → (s : SigVec as) → Always (Sourceʳ {as} {bs} sf₁ s ⇒ Sourceʳ {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
Sourceʳ1Causal2⇒SourceʳSeq sf₁ sf₂ cau s t src s' Heq = lem-Hʳ⇒Always (cau (sf₁ s) (sf₁ s')) (src s' Heq)

-- Causal sf₂ → Always (Sourceʳ sf₁ s ⇒ Sourceʳ (sf₁ >>> sf₂) s)
Causal1Sourceʳ2⇒SourceʳSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {as} {bs} sf₁ → (s : SigVec as) → Always (Sourceʳ {bs} {cs} sf₂ (sf₁ s) ⇒ Sourceʳ {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
Causal1Sourceʳ2⇒SourceʳSeq sf₁ sf₂ cau s t src s' Heq = src (sf₁ s') (lem-Hʳ⇒H (cau s s') t Heq)

-- Changeless sf₂ (sf₁ s) ⇒ Changeless (sf₁ >>> sf₂) s
Changeless2⇒ChangelessSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {as} {bs} sf₁ → (s : SigVec as) → Always (Changeless {bs} {cs} sf₂ (sf₁ s) ⇒ Changeless {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
Changeless2⇒ChangelessSeq sf₁ sf₂ cau s t p s' Heq = p (sf₁ s') (lem-Hʳ⇒Hʳ (cau s s') t Heq)

-- Changelessʳ sf₂ (sf₁ s) ⇒ Changelessʳ (sf₁ >>> sf₂) s
Changelessʳ2⇒ChangelessʳSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {as} {bs} sf₁ → (s : SigVec as) → Always (Changelessʳ {bs} {cs} sf₂ (sf₁ s) ⇒ Changelessʳ {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
Changelessʳ2⇒ChangelessʳSeq sf₁ sf₂ cau s t p s' Heq = p (sf₁ s') (lem-Hʳ⇒H (cau s s') t Heq)

-- ChangeDep sf₁ s ∧ ChangeDep sf₂ (sf₁ s) ⇒ ChangeDep (sf₁ >>> sf₂) s
ChangeDep1ChangeDep2⇒ChangeDepSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {as} {bs} sf₁ → (s : SigVec as) → Always (ChangeDep {as} {bs} sf₁ s ⇒ ChangeDep {bs} {cs} sf₂ (sf₁ s) ⇒ ChangeDep {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
ChangeDep1ChangeDep2⇒ChangeDepSeq sf₁ sf₂ cau s t₁ chd₁ chd₂ s' Heq t₂ lt un = chd₂ (sf₁ s') (lem-Hʳ⇒Hʳ (cau s s') t₁ Heq) t₂ lt (chd₁ s' Heq t₂ lt un)

-- ChangeDepʳ sf₁ s ∧ ChangeDepʳ sf₂ (sf₁ s) ⇒ ChangeDepʳ (sf₁ >>> sf₂) s
ChangeDepʳ1ChangeDepʳ2⇒ChangeDepʳSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {as} {bs} sf₁ → (s : SigVec as) → Always (ChangeDepʳ {as} {bs} sf₁ s ⇒ ChangeDepʳ {bs} {cs} sf₂ (sf₁ s) ⇒ ChangeDepʳ {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
ChangeDepʳ1ChangeDepʳ2⇒ChangeDepʳSeq sf₁ sf₂ cau s t₁ chd₁ chd₂ s' Heq t₂ lt un = chd₂ (sf₁ s') (lem-Hʳ⇒H (cau s s') t₁ Heq) t₂ lt (chd₁ s' Heq t₂ lt un)

-- ChangePrp sf₁ s ∧ ChangePrp sf₂ (sf₁ s) ⇒ ChangePrp (sf₁ >>> sf₂) s
ChangePrp1ChangePrp2⇒ChangePrpSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {as} {bs} sf₁ → (s : SigVec as) → Always (ChangePrp {as} {bs} sf₁ s ⇒ ChangePrp {bs} {cs} sf₂ (sf₁ s) ⇒ ChangePrp {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
ChangePrp1ChangePrp2⇒ChangePrpSeq {as} {bs} {cs} sf₁ sf₂ cau s t₁ ((chd₁ , Gchd₁) , chdr₁) ((chd₂ , Gchd₂) , chdr₂) =
   (ChangeDep1ChangeDep2⇒ChangeDepSeq {as} {bs} {cs} sf₁ sf₂ cau s t₁ chd₁ chd₂ ,
   λ t₂ lt s' Heq t₃ lt' unb → Gchd₂ t₂ lt (sf₁ s') (lem-Hʳ⇒Hʳ (cau s s') t₂ Heq) t₃ lt' (Gchd₁ t₂ lt s' Heq t₃ lt' unb)) ,
   (λ t₂ lt s' Heq t₃ lt' unch → chdr₂ t₂ lt (sf₁ s') (lem-Hʳ⇒H (cau s s') t₂ Heq) t₃ lt' (chdr₁ t₂ lt s' Heq t₃ lt' unch))

-- ChangePrpʳ sf₁ s ∧ ChangePrpʳ sf₂ (sf₁ s) ⇒ ChangePrpʳ (sf₁ >>> sf₂) s
ChangePrpʳ1ChangePrpʳ2⇒ChangePrpʳSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {as} {bs} sf₁ → (s : SigVec as) → Always (ChangePrpʳ {as} {bs} sf₁ s ⇒ ChangePrpʳ {bs} {cs} sf₂ (sf₁ s) ⇒ ChangePrpʳ {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
ChangePrpʳ1ChangePrpʳ2⇒ChangePrpʳSeq {as} {bs} {cs} sf₁ sf₂ cau s t₁ (chp₁ , chd₁) (chp₂ , chd₂) =
  ChangePrp1ChangePrp2⇒ChangePrpSeq {as} {bs} {cs} sf₁ sf₂ cau s t₁ chp₁ chp₂ ,
  ChangeDepʳ1ChangeDepʳ2⇒ChangeDepʳSeq {as} {bs} {cs} sf₁ sf₂ cau s t₁ chd₁ chd₂

-- ChangeExecʳ sf₁ s ∧ ChangeExecʳ sf₂ (sf₁ s) ⇒ ChangeExecʳ (sf₁ >>> sf₂) s
-- ChangeExecʳ1ChangeExecʳ2⇒ChangeExecʳSeq : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF bs cs) → Causal {as} {bs} sf₁ → (s : SigVec as) → Always (ChangeExecʳ {as} {bs} sf₁ s ⇒ ChangeExecʳ {bs} {cs} sf₂ (sf₁ s) ⇒ ChangeExecʳ {as} {cs} (_>>>_ {as} {bs} {cs} sf₁ sf₂) s)
-- ChangeExecʳ1ChangeExecʳ2⇒ChangeExecʳSeq sf₁ sf₂ cau s t₁ che₁ che₂ s' Heq t₂ lt un with che₁ s' Heq t₂ lt un
-- ... | un₁ , (eq₁ , Heq₁) with che₂ (sf₁ s') (lem-Hʳ⇒H (cau s s') t₁ Heq) t₂ lt un₁ 
-- ...    | un₂ , (eq₂ , Heq₂) = (un₂ , {!!})

-----------------------------------------------------------------------------------

-- Preservation of Properties by (&&&)

-- Source sf₁ s ∧ Source sf₂ s ⇒ Source (sf₁ &&& sf₂) s
Source1Source2⇒SourceFan : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF as cs) → (s : SigVec as) → Always (Source {as} {bs} sf₁ s ⇒ Source {as} {cs} sf₂ s ⇒ Source {as} {bs , cs} (_&&&_ {as} {bs} {cs} sf₁ sf₂) s)
Source1Source2⇒SourceFan {as} {bs} sf₁ sf₂ s t = ×-cong4

-- Sourceʳ sf₁ s ∧ Sourceʳ sf₂ s ⇒ Sourceʳ (sf₁ &&& sf₂) s
Sourceʳ1Sourceʳ2⇒SourceʳFan : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF as cs) → (s : SigVec as) → Always (Sourceʳ {as} {bs} sf₁ s ⇒ Sourceʳ {as} {cs} sf₂ s ⇒ Sourceʳ {as} {bs , cs} (_&&&_ {as} {bs} {cs} sf₁ sf₂) s)
Sourceʳ1Sourceʳ2⇒SourceʳFan sf₁ sf₂ s t = ×-cong4

-- Changeless sf₁ s ∧ Changeless sf₂ s ⇒ Changeless (sf₁ &&& sf₂) s
Changeless1Changeless2⇒ChangelessFan : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF as cs) → (s : SigVec as) → Always (Changeless {as} {bs} sf₁ s ⇒ Changeless {as} {cs} sf₂ s ⇒ Changeless {as} {bs , cs} (_&&&_ {as} {bs} {cs} sf₁ sf₂) s)
Changeless1Changeless2⇒ChangelessFan sf₁ sf₂ s t ch₁ ch₂ = ch₁ &₄ ch₂

-- Changelessʳ sf₁ s ∧ Changelessʳ sf₂ s ⇒ Changelessʳ (sf₁ &&& sf₂) s
Changelessʳ1Changelessʳ2⇒ChangelessʳFan : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF as cs) → (s : SigVec as) → Always (Changelessʳ {as} {bs} sf₁ s ⇒ Changelessʳ {as} {cs} sf₂ s ⇒ Changelessʳ {as} {bs , cs} (_&&&_ {as} {bs} {cs} sf₁ sf₂) s)
Changelessʳ1Changelessʳ2⇒ChangelessʳFan sf₁ sf₂ s t ch₁ ch₂ s' Heq with ch₁ s' Heq | ch₂ s' Heq
... | un₁ , Gun₁ | un₂ , Gun₂ = (un₁ , un₂) , (Gun₁ &₂ Gun₂)

-- ChangeDep sf₁ s ∧ ChangeDep sf₂ s ⇒ ChangeDep (sf₁ &&& sf₂) s
ChangeDep1ChangeDep2⇒ChangeDepFan : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF as cs) → (s : SigVec as) → Always (ChangeDep {as} {bs} sf₁ s ⇒ ChangeDep {as} {cs} sf₂ s ⇒ ChangeDep {as} {bs , cs} (_&&&_ {as} {bs} {cs} sf₁ sf₂) s)
ChangeDep1ChangeDep2⇒ChangeDepFan sf₁ sf₂ s t₁ chd₁ chd₂ = chd₁ &₅ chd₂

-- ChangeDepʳ sf₁ s ∧ ChangeDepʳ sf₂ s ⇒ ChangeDepʳ (sf₁ &&& sf₂) s
ChangeDepʳ1ChangeDepʳ2⇒ChangeDepʳFan : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF as cs) → (s : SigVec as) → Always (ChangeDepʳ {as} {bs} sf₁ s ⇒ ChangeDepʳ {as} {cs} sf₂ s ⇒ ChangeDepʳ {as} {bs , cs} (_&&&_ {as} {bs} {cs} sf₁ sf₂) s)
ChangeDepʳ1ChangeDepʳ2⇒ChangeDepʳFan sf₁ sf₂ s t₁ chd₁ chd₂ s' Heq t₂ lt unch with chd₁ s' Heq t₂ lt unch | chd₂ s' Heq t₂ lt unch
... | un₁ , unch₁ | un₂ , unch₂ = (un₁ , un₂) , unch₁ , unch₂

-- ChangePrp sf₁ s ∧ ChangePrp sf₂ s ⇒ ChangePrp (sf₁ &&& sf₂) s
ChangePrp1ChangePrp2⇒ChangePrpFan : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF as cs) → (s : SigVec as) → Always (ChangePrp {as} {bs} sf₁ s ⇒ ChangePrp {as} {cs} sf₂ s ⇒ ChangePrp {as} {bs , cs} (_&&&_ {as} {bs} {cs} sf₁ sf₂) s)
ChangePrp1ChangePrp2⇒ChangePrpFan {as} {bs} {cs} sf₁ sf₂ s t ((chd₁ , Gchd₁) , chdr₁) ((chd₂ , Gchd₂) , chdr₂) =
  (chd₁ &₅ chd₂ ,
  Gchd₁ &₇ Gchd₂) ,
  λ t' lt → ChangeDepʳ1ChangeDepʳ2⇒ChangeDepʳFan {as} {bs} {cs} sf₁ sf₂ s t' (chdr₁ t' lt) (chdr₂ t' lt)

-- ChangePrpʳ sf₁ s ∧ ChangePrpʳ sf₂ s ⇒ ChangePrpʳ (sf₁ &&& sf₂) s
ChangePrpʳ1ChangePrpʳ2⇒ChangePrpʳFan : {as bs cs : SVDesc} → (sf₁ : SF as bs) → (sf₂ : SF as cs) → (s : SigVec as) → Always (ChangePrpʳ {as} {bs} sf₁ s ⇒ ChangePrpʳ {as} {cs} sf₂ s ⇒ ChangePrpʳ {as} {bs , cs} (_&&&_ {as} {bs} {cs} sf₁ sf₂) s)
ChangePrpʳ1ChangePrpʳ2⇒ChangePrpʳFan {as} {bs} {cs} sf₁ sf₂ s t (chp₁ , chd₁) (chp₂ , chd₂) =
  ChangePrp1ChangePrp2⇒ChangePrpFan {as} {bs} {cs} sf₁ sf₂ s t chp₁ chp₂ ,
  ChangeDepʳ1ChangeDepʳ2⇒ChangeDepʳFan {as} {bs} {cs} sf₁ sf₂ s t chd₁ chd₂

-----------------------------------------------------------------------------------

-- Preservation of Properties by (freeze)

-- These still need proving!!!!

-- -- Changelessʳ sf s ⇒ Changelessʳ (freeze sf) s
postulate Changelessʳ⇒ChangelessʳFreeze : {as bs : SVDesc} → SFRepExt → (sf : SF as bs) → (s : SigVec as) → Always (Changelessʳ {as} {bs} sf s ⇒ Changelessʳ {as} {bs , C (SF as bs)} (freeze {as} {bs} sf) s)
-- Changelessʳ⇒ChangelessʳFreeze {as} {bs} ext sf s₁ t₀ ch = {!!} -- ch s₂ Heq t₃ lt , λ t₁ t₂ p q → ext {as} {bs} (λ s t → {!!}) -- ×-cong (src s₂ Heq t₂) (ext {as} {bs} (λ s t → lem-rep-advance-eq {bs} {!!} t t₂ (sf (splice s₁ s t₂)) (sf (splice s₂ s t₂)) {!!} ))

-- -- Source sf s ⇒ Source (freeze sf) s
postulate Source⇒SourceFreeze : {as bs : SVDesc} → SFRepExt → (sf : SF as bs) → (s : SigVec as) → Always (Source {as} {bs} sf s ⇒ Source {as} {bs , C (SF as bs)} (freeze {as} {bs} sf) s)
-- Source⇒SourceFreeze {as} {bs} ext sf s₁ t₁ src s₂ Heq t₂ = ×-cong (src s₂ Heq t₂) (ext {as} {bs} (λ s t → lem-rep-advance-eq {bs} {!!} t t₂ (sf (splice s₁ s t₂)) (sf (splice s₂ s t₂)) {!!} ))

-- -- Sourceʳ sf s ⇒ Sourceʳ (freeze sf) s
postulate Sourceʳ⇒SourceʳFreeze : {as bs : SVDesc} → SFRepExt → (sf : SF as bs) → (s : SigVec as) → Always (Sourceʳ {as} {bs} sf s ⇒ Sourceʳ {as} {bs , C (SF as bs)} (freeze {as} {bs} sf) s)
-- Sourceʳ⇒SourceʳFreeze {as} {bs} ext sf s₁ t₁ src s₂ Heq t₂ = ×-cong (src s₂ Heq t₂) (ext {as} {bs} (λ s t → {!!})) -- ×-cong (src s' Heq t₂) (ext (λ s₂ → {!!}))

-- -- Changelessʳ sf s ⇒ (λ t → (∀ s' → Changeless (frozenSample sf s t) s' O))
postulate Changelessʳ→ChangelessFrozen : {as bs : SVDesc} → (sf : SF as bs) → (s : SigVec as) → Always (Changelessʳ {as} {bs} sf s ⇒ (λ t → ∀ s' → Changeless {as} {bs} (frozenSample {as} {bs} sf s t) s' O))
-- Changelessʳ→ChangelessFrozen sf s t ch = {!!}

-- -- ChangePrpʳ sf s ⇒ (λ t → (∀ s' → ChangePrp (frozenSample sf s t) s' O))
postulate ChangePrpʳ→ChangePrpFrozen : {as bs : SVDesc} → (sf : SF as bs) → (s : SigVec as) → Always (ChangePrpʳ {as} {bs} sf s ⇒ (λ t → ∀ s' → ChangePrp {as} {bs} (frozenSample {as} {bs} sf s t) s' O))
-- ChangePrpʳ→ChangePrpFrozen sf s t ch = {!!}

-- -- Sourceʳ sf s ⇒ (λ t → (∀ s' → Sourceʳ (frozenSample sf s t) s' O))
postulate Sourceʳ→SourceʳFrozen : {as bs : SVDesc} → (sf : SF as bs) → (s : SigVec as) → Always (Sourceʳ {as} {bs} sf s ⇒ (λ t → ∀ s' → Sourceʳ {as} {bs} (frozenSample {as} {bs} sf s t) s' O))
-- Sourceʳ→SourceʳFrozen sf s t ch s₁ s₂ Heq t' = {!!}

-----------------------------------------------------------------------------------

-- Preservation of Properties by (switch)

-- NotSwitched sf s ∧ Changeless sf s ⇒ Changeless (switch sf f) s
postulate ChangelessNotSwitched⇒ChangelessSwitch : {as bs : SVDesc} → {A : Set} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (NotSwitched {as} {bs} sf s ⇒ Changeless {as} {bs , E A} sf s ⇒ Changeless {as} {bs} (switch {as} {bs} sf f) s)

-- NotSwitched sf s ∧ Changelessʳ sf s ⇒ Changelessʳ (switch sf f) s
postulate ChangelessʳNotSwitched⇒ChangelessʳSwitch : {as bs : SVDesc} → {A : Set} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (NotSwitched {as} {bs} sf s ⇒ Changelessʳ {as} {bs , E A} sf s ⇒ Changelessʳ {as} {bs} (switch {as} {bs} sf f) s)

-- NotSwitched sf s ∧ ChangeDep sf s ⇒ ChangeDep (switch sf f) s
postulate ChangeDepNotSwitched⇒ChangeDepSwitch : {as bs : SVDesc} → {A : Set} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (NotSwitched {as} {bs} sf s ⇒ ChangeDep {as} {bs , E A} sf s ⇒ ChangeDep {as} {bs} (switch {as} {bs} sf f) s)

-- NotSwitched sf s ∧ ChangeDepʳ sf s ⇒ ChangeDepʳ (switch sf f) s
postulate ChangeDepʳNotSwitched⇒ChangeDepʳSwitch : {as bs : SVDesc} → {A : Set} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (NotSwitched {as} {bs} sf s ⇒ ChangeDepʳ {as} {bs , E A} sf s ⇒ ChangeDepʳ {as} {bs} (switch {as} {bs} sf f) s)

-- NotSwitched sf s ∧ ChangePrp sf s ⇒ ChangeDep (switch sf f) s
ChangePrpNotSwitched⇒ChangeDepSwitch : {as bs : SVDesc} → {A : Set} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (NotSwitched {as} {bs} sf s ⇒ ChangePrp {as} {bs , E A} sf s ⇒ ChangeDep {as} {bs} (switch {as} {bs} sf f) s)
ChangePrpNotSwitched⇒ChangeDepSwitch {as} {bs} {A} sf f s t nsw chp = ChangeDepNotSwitched⇒ChangeDepSwitch {as} {bs} sf f s t nsw (ChangePrp⇒ChangeDep {as} {bs , E A} sf s t chp)

-- NotSwitched sf s ∧ ChangePrpʳ sf s ⇒ ChangeDepʳ (switch sf f) s
ChangePrpʳNotSwitched⇒ChangeDepʳSwitch : {as bs : SVDesc} → {A : Set} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (NotSwitched {as} {bs} sf s ⇒ ChangePrpʳ {as} {bs , E A} sf s ⇒ ChangeDepʳ {as} {bs} (switch {as} {bs} sf f) s)
ChangePrpʳNotSwitched⇒ChangeDepʳSwitch {as} {bs} {A} sf f s t nsw chp = ChangeDepʳNotSwitched⇒ChangeDepʳSwitch {as} {bs} sf f s t nsw (ChangePrpʳ⇒ChangeDepʳ {as} {bs , E A} sf s t chp)


private postulate _-_ : SampleTime → EventTime → SampleTime

-- Switched (te , a) sf s ∧ (λ t → Changeless (f a) (advance te s) (t - te)) ⇒ Changeless (switch sf f) s
postulate SwitchedChangeless⇒ChangelessSwitch : {as bs : SVDesc} → {A : Set} → {a : A} → {te : Time} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (Switched {as} {bs} (te , a) sf s ⇒ (λ t → Changeless {as} {bs} (f a) (advance {as} te s) (t - te)) ⇒ Changeless {as} {bs} (switch {as} {bs} sf f) s)

-- Switched (te , a) sf s ∧ (λ t → Changelessʳ (f a) (advance te s) (t - te)) ⇒ Changelessʳ (switch sf f) s
postulate SwitchedChangelessʳ⇒ChangelessʳSwitch : {as bs : SVDesc} → {A : Set} → {a : A} → {te : Time} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (Switched {as} {bs} (te , a) sf s ⇒ (λ t → Changelessʳ {as} {bs} (f a) (advance {as} te s) (t - te)) ⇒ Changelessʳ {as} {bs} (switch {as} {bs} sf f) s)

-- Switched (te , a) sf s ∧ (λ t → ChangeDep (f a) (advance te s) (t - te)) ⇒ ChangeDep (switch sf f) s
postulate SwitchedChangeDep⇒ChangeDepSwitch : {as bs : SVDesc} → {A : Set} → {a : A} → {te : Time} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (Switched {as} {bs} (te , a) sf s ⇒ (λ t → ChangeDep {as} {bs} (f a) (advance {as} te s) (t - te)) ⇒ ChangeDep {as} {bs} (switch {as} {bs} sf f) s)

-- Switched (te , a) sf s ∧ (λ t → ChangeDepʳ (f a) (advance te s) (t - te)) ⇒ ChangeDepʳ (switch sf f) s
postulate SwitchedChangeDepʳ⇒ChangeDepʳSwitch : {as bs : SVDesc} → {A : Set} → {a : A} → {te : Time} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (Switched {as} {bs} (te , a) sf s ⇒ (λ t → ChangeDepʳ {as} {bs} (f a) (advance {as} te s) (t - te)) ⇒ ChangeDepʳ {as} {bs} (switch {as} {bs} sf f) s)

-- Switched (te , a) sf s ∧ (λ t → ChangePrp (f a) (advance te s) (t - te)) ⇒ ChangePrp (switch sf f) s
postulate SwitchedChangePrp⇒ChangePrpSwitch : {as bs : SVDesc} → {A : Set} → {a : A} → {te : Time} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (Switched {as} {bs} (te , a) sf s ⇒ (λ t → ChangePrp {as} {bs} (f a) (advance {as} te s) (t - te)) ⇒ ChangePrp {as} {bs} (switch {as} {bs} sf f) s)

-- P (Switched (te , a) sf s) ∧ (λ t → ChangePrpʳ (f a) (advance te s) (t - te)) ⇒ ChangePrpʳ (switch sf f) s
postulate SwitchedChangePrpʳ⇒ChangePrpʳSwitch : {as bs : SVDesc} → {A : Set} → {a : A} → {te : Time} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (Switched {as} {bs} (te , a) sf s ⇒ (λ t → ChangePrpʳ {as} {bs} (f a) (advance {as} te s) (t - te)) ⇒ ChangePrpʳ {as} {bs} (switch {as} {bs} sf f) s)

-- Switched (te , a) sf s ∧ (λ t → Source (f a) (advance te s) (t - te)) ⇒ Source (switch sf f) s
postulate SwitchedSource⇒SourceSwitch : {as bs : SVDesc} → {A : Set} → {a : A} → {te : Time} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (Switched {as} {bs} (te , a) sf s ⇒ (λ t → Source {as} {bs} (f a) (advance {as} te s) (t - te)) ⇒ Source {as} {bs} (switch {as} {bs} sf f) s)

-- Switched (te , a) sf s ∧ (λ t → Sourceʳ (f a) (advance te s) (t - te)) ⇒ Sourceʳ (switch sf f) s
postulate SwitchedSourceʳ⇒SourceʳSwitch : {as bs : SVDesc} → {A : Set} → {a : A} → {te : Time} → (sf : SF as (bs , E A)) → (f : A → SF as bs) → (s : SigVec as) → Always (Switched {as} {bs} (te , a) sf s ⇒ (λ t → Sourceʳ {as} {bs} (f a) (advance {as} te s) (t - te)) ⇒ Sourceʳ {as} {bs} (switch {as} {bs} sf f) s)

-----------------------------------------------------------------------------------
