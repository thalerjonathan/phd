{-# OPTIONS --type-in-type #-}

module SFLemmas where

open import NeilPrelude
open import RealTime
open import SigVecs
open import Utilities
open import Properties
open import CPLemmas
open import SigLemmas

------------------------------------------------------------------------------

lem-SD-H : {as bs : SVDesc} → (sf : SF as bs) → (s₁ s₂ : SigVec as) → (δ : Δt) → Always (Earlier δ (Hʳ (EqRep {as} s₁ s₂)) ⇒ EqRep {bs} (sf s₁) (sf s₂))
                                                                               → Always (Earlier δ (Hʳ (EqRep {as} s₁ s₂)) ⇒ H (EqRep {bs} (sf s₁) (sf s₂)))
lem-SD-H {as} {bs} sf s₁ s₂ δ dec t₁ Heq t₀ lt with dec t₀
... | dec' with compareGeq t₀ (δ >0)
...    | less p = dec' _
...    | geq  p with compareGeq t₁ (δ >0)
...         | less q = absurd (<ℜ₀-asym lt (<≤ℜ₀-trans q p))
...         | geq  q = dec' (lem-HeqRep-minus2 {as} s₁ s₂ lt p q Heq)

lem-SD-Hʳ : {as bs : SVDesc} → (sf : SF as bs) → (s₁ s₂ : SigVec as) → (δ : Δt) → Always (Earlier δ (Hʳ (EqRep {as} s₁ s₂)) ⇒ EqRep {bs} (sf s₁) (sf s₂))
                                                                                → Always (Earlier δ (Hʳ (EqRep {as} s₁ s₂)) ⇒ Hʳ (EqRep {bs} (sf s₁) (sf s₂)))
lem-SD-Hʳ {as} {bs} sf s₁ s₂ δ dec t Heq = dec t Heq , lem-SD-H {as} {bs} sf s₁ s₂ δ dec t Heq

------------------------------------------------------------------------------

ChangeDep⇒ChangelessSVʳ⇒ChangelessSVʳ : {as bs : SVDesc} → (sf : SF as bs) → (s : SigVec as) → Always (ChangeDepʳ {as} {bs} sf s ⇒ ChangelessSVʳ {as} s ⇒ ChangelessSVʳ {bs} (sf s))
ChangeDep⇒ChangelessSVʳ⇒ChangelessSVʳ {as} {bs} sf s t₁ chd (un , Gun) = fst (chd s (λ _ _ → refl) t₁ (inr refl) (un , lem-UB {as} s (inr refl))) , λ t₂ lt → snd (chd s (λ _ _ → refl) t₂ (inl lt) (un , Gun t₂ lt))

ChangeDep⇒ChangelessSV⇒ChangelessSV : {as bs : SVDesc} → (sf : SF as bs) → (s : SigVec as) → Always (ChangeDep {as} {bs} sf s ⇒ ChangelessSV {as} s ⇒ ChangelessSV {bs} (sf s))
ChangeDep⇒ChangelessSV⇒ChangelessSV {as} {bs} sf s t₁ chd ch t₂ lt = chd s (refl , λ _ _ → refl) t₂ lt (ch t₂ lt)

------------------------------------------------------------------------------

Changelessʳ→AlwaysEqRep : {as bs : SVDesc} → Stability → (sf : SF as bs) → ((s : SigVec as) → Always (Changelessʳ {as} {bs} sf s)) → (s₁ s₂ : SigVec as) → Always (EqRep {bs} (sf s₁) (sf s₂))
Changelessʳ→AlwaysEqRep {as} {bs} stab sf ch s₁ s₂ = lem-ChangelessSVʳ-AlwaysEqRep {bs} stab (sf s₁) (sf s₂) (ch s₁ O s₁ (λ _ _ → refl)) (ch s₂ O s₂ (λ _ _ → refl))

Changelessʳ→AlwaysEqSample : {as bs : SVDesc} → Stability → (sf : SF as bs) → ((s : SigVec as) →  Always (Changelessʳ {as} {bs} sf s)) → (s₁ s₂ : SigVec as) → Always (EqSample {bs} (sf s₁) (sf s₂))
Changelessʳ→AlwaysEqSample {as} {bs} stab sf ch s₁ s₂ t = lem-EqRep→EqSample {bs} (sf s₁) (sf s₂) t (Changelessʳ→AlwaysEqRep {as} {bs} stab sf ch s₁ s₂ t)

------------------------------------------------------------------------------

lem-Cau-HʳEqSample⇒C⇒GeqSample : {as bs : SVDesc} → Stability → (sf : SF as bs) → SampleCausal {as} {bs} sf → (s₁ s₂ : SigVec as) → Always (Hʳ (EqSample {as} s₁ s₂) ⇒ ChangelessSV {bs} (sf s₁) ⇒ ChangelessSV {bs} (sf s₂) ⇒ G (EqSample {bs} (sf s₁) (sf s₂)))
lem-Cau-HʳEqSample⇒C⇒GeqSample {as} {bs} stab sf cau s₁ s₂ t Heq = lem-EqSample⇒ChangelessSV⇒GEqSample {bs} stab (sf s₁) (sf s₂) t (cau s₁ s₂ t Heq)

lem-Cau-HEqSample⇒Cʳ⇒GʳeqSample : {as bs : SVDesc} → Stability → (sf : SF as bs) → SampleCausal {as} {bs} sf → (s₁ s₂ : SigVec as) → Always (H (EqSample {as} s₁ s₂) ⇒ ChangelessSVʳ {bs} (sf s₁) ⇒ ChangelessSVʳ {bs} (sf s₂) ⇒ Gʳ (EqSample {bs} (sf s₁) (sf s₂)))
lem-Cau-HEqSample⇒Cʳ⇒GʳeqSample {as} {bs} stab sf cau s₁ s₂ t Heq = lem-HEqSample⇒ChangelessSVʳ⇒GʳEqSample {bs} stab (sf s₁) (sf s₂) t (lem-Hʳ⇒H (cau s₁ s₂) t Heq)

------------------------------------------------------------------------------

lem-HʳEqRep→C→AlwaysEqRep : {as bs : SVDesc} → (sf : SF as bs) → RepCausal {as} {bs} sf → (s₁ s₂ : SigVec as) → (t : Time)
               → Hʳ (EqRep {as} s₁ s₂) t → ChangelessSV {bs} (sf s₁) t → ChangelessSV {bs} (sf s₂) t → Always (EqRep {bs} (sf s₁) (sf s₂))
lem-HʳEqRep→C→AlwaysEqRep {as} {bs} sf cau s₁ s₂ t Heq = lem-EqHʳ⇒C⇒Eq {bs} (sf s₁) (sf s₂) t (lem-Hʳ⇒Hʳ (cau s₁ s₂) t Heq) 

lem-HEqRep→Cʳ→AlwaysEqRep : {as bs : SVDesc} → Stability → (sf : SF as bs) → RepCausal {as} {bs} sf → (s₁ s₂ : SigVec as) → (t : Time)
               → H (EqRep {as} s₁ s₂) t → ChangelessSVʳ {bs} (sf s₁) t → ChangelessSVʳ {bs} (sf s₂) t → Always (EqRep {bs} (sf s₁) (sf s₂))
lem-HEqRep→Cʳ→AlwaysEqRep {as} {bs} stab sf cau s₁ s₂ t Heq = lem-EqH⇒Cʳ⇒Eq {bs} stab (sf s₁) (sf s₂) t (lem-Hʳ⇒H (cau s₁ s₂) t Heq)

------------------------------------------------------------------------------

lem-CD⇒Src⇒C : {as bs : SVDesc} → Stability → (sf : SF as bs) → (s : SigVec as) → Always (ChangeDep {as} {bs} sf s ⇒ Source {as} {bs} sf s ⇒ Changeless {as} {bs} sf s)
lem-CD⇒Src⇒C {as} {bs} stab sf s₁ t₁ chd src s₂ Heq₂ t₂ lt with changelessBeyond {as} stab s₁ t₁
... | s₃ , unb , Heq₃ = lem-AlwaysEqRep⇒C⇒C {bs} (sf s₃) (sf s₂) (λ t' → trans (sym (src s₃ Heq₃ t')) (src s₂ Heq₂ t')) t₁ (λ t₃ lt₃ → chd s₃ Heq₃ t₃ lt₃ (unb t₃ lt₃)) t₂ lt

------------------------------------------------------------------------------

lem-sample-advance : {as bs : SVDesc} → Stability → (s₁ s₂ s : SigVec as) → (t₁ t₂ t : Time) → (sf : SF as bs) → Stateless {as} {bs} sf → sample {as} s₁ t₁ ≡ sample {as} s₂ t₂ → sample {bs} (advance {bs} t₁ (sf (splice {as} s₁ s t₁))) t ≡ sample {bs} (advance {bs} t₂ (sf (splice {as} s₂ s t₂))) t
lem-sample-advance {as} {bs} stab s₁ s₂ s t₁ t₂ t sf stl eq = trans2 (lem-sample-advance-plus {bs} stab t₁ t (sf (splice {as} s₁ s t₁)))
                                                                     (stl (splice {as} s₁ s t₁) (splice {as} s₂ s t₂) (t ₀+₀ t₁) (t ₀+₀ t₂) (lem-sample-splice-eq {as} stab t₁ t₂ t s₁ s₂ s eq))
                                                                     (sym (lem-sample-advance-plus {bs} stab t₂ t (sf (splice {as} s₂ s t₂))))

------------------------------------------------------------------------------

lem-Heq-splice-advance : {as bs : SVDesc} → Extensionality → (sf : SF as bs) → (s₁ s₂ s : SigVec as) → (t : Time) → H (EqRep {as} s₁ s₂) t → advance {bs} t (sf (splice {as} s₁ s t)) ≡ advance {bs} t (sf (splice {as} s₂ s t))
lem-Heq-splice-advance {as} {bs} ext sf s₁ s₂ s t Heq = cong (advance {bs} t ∘ sf) (lem-Heq-splice {as} ext s₁ s₂ s t Heq)

lem-Heq-rep-splice-advance : {as bs : SVDesc} → Extensionality → (sf : SF as bs) → (s₁ s₂ s : SigVec as) → (t : Time) → H (EqRep {as} s₁ s₂) t
                             → Always (EqRep {bs} (advance {bs} t (sf (splice {as} s₁ s t))) (advance {bs} t (sf (splice {as} s₂ s t))))
lem-Heq-rep-splice-advance {as} {bs} ext sf s₁ s₂ s t Heq t' rewrite lem-Heq-splice-advance {as} {bs} ext sf s₁ s₂ s t Heq = refl

------------------------------------------------------------------------------

lem-cau-rep-advance-splice : {as bs : SVDesc} → Stability → (sf : SF as bs) → Causal {as} {bs} sf → (s₁ s₂ s : SigVec as) → (t : Time) → Always (Hʳ (EqRep {as} s₁ s₂) ⇒ EqRep {bs} (advance {bs} t (sf (splice {as} s s₁ t))) (advance {bs} t (sf (splice {as} s s₂ t))))
lem-cau-rep-advance-splice {as} {bs} stab sf cau s₁ s₂ s tx t (eq , Heq) =
  lem-rep-advance-eq {bs} stab t tx (sf (splice {as} s s₁ tx)) (sf (splice {as} s s₂ tx)) (cau (splice {as} s s₁ tx) (splice {as} s s₂ tx) (t ₀+₀ tx) (lem-EqRep-splice {as} s₁ s₂ s tx t eq , lem-HEqRep-splice {as} s₁ s₂ s tx t Heq))

lem-dec-rep-advance-splice : {as bs : SVDesc} → Stability → (sf : SF as bs) → Decoupled {as} {bs} sf → (s₁ s₂ s : SigVec as) → (t : Time) → Always (H (EqRep {as} s₁ s₂) ⇒ EqRep {bs} (advance {bs} t (sf (splice {as} s s₁ t))) (advance {bs} t (sf (splice {as} s s₂ t))))
lem-dec-rep-advance-splice {as} {bs} stab sf dec s₁ s₂ s tx t Heq =
  lem-rep-advance-eq {bs} stab t tx (sf (splice {as} s s₁ tx)) (sf (splice {as} s s₂ tx)) (dec (splice {as} s s₁ tx) (splice {as} s s₂ tx) (t ₀+₀ tx) (lem-HEqRep-splice {as} s₁ s₂ s tx t Heq))

postulate lem-stl-sample-advance-splice : {as bs : SVDesc} → Stability → (sf : SF as bs) → Stateless {as} {bs} sf → (s₁ s₂ s : SigVec as) → (t t₁ t₂ : Time) → sample {as} s₁ t₁ ≡ sample {as} s₂ t₂ → sample {bs} (advance {bs} t (sf (splice {as} s s₁ t))) t₁ ≡ sample {bs} (advance {bs} t (sf (splice {as} s s₂ t))) t₂
-- lem-stl-sample-advance-splice {as} {bs} stab sf stl s₁ s₂ s t t₁ t₂ eq = {!lem-sample-advance-eq {bs} ?!}

------------------------------------------------------------------------------

SFSampleExt→SFRepExt : SFSampleExt → SFRepExt
SFSampleExt→SFRepExt ext {as} {bs} {sf₁} {sf₂} eq = ext {as} {bs} (λ s t → lem-EqRep→EqSample {bs} (sf₁ s) (sf₂ s) t (eq s t))

------------------------------------------------------------------------------
