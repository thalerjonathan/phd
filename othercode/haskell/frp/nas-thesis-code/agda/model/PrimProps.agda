{-# OPTIONS --type-in-type #-}

module PrimProps where

open import NeilPrelude
open import RealTime
open import Logic
open import List
open import TimeDeltaList
open import TimeDeltaListProps
open import SigVecs
open import Properties
open import CPLemmas
open import SigLemmas
open import SFLemmas
open import NaryFRP

-----------------------------------------------------------------------------------

-- NOTE: These proofs are unfinished, and several primitives have yet to be dealt with

-----------------------------------------------------------------------------------

ChangelessSF : {as bs : SVDesc} → (sf : SF as bs) → Set
ChangelessSF {as} {bs} sf = (s : SigVec as) → Always (Changeless {as} {bs} sf s)

ChangelessSFʳ : {as bs : SVDesc} → (sf : SF as bs) → Set
ChangelessSFʳ {as} {bs} sf = (s : SigVec as) → Always (Changelessʳ {as} {bs} sf s)

ChangeDepSF : {as bs : SVDesc} → (sf : SF as bs) → Set
ChangeDepSF {as} {bs} sf = (s : SigVec as) → Always (ChangeDep {as} {bs} sf s)

ChangeDepSFʳ : {as bs : SVDesc} → (sf : SF as bs) → Set
ChangeDepSFʳ {as} {bs} sf = (s : SigVec as) → Always (ChangeDepʳ {as} {bs} sf s)

ChangePrpSF : {as bs : SVDesc} → (sf : SF as bs) → Set
ChangePrpSF {as} {bs} sf = (s : SigVec as) → Always (ChangePrp {as} {bs} sf s)

ChangePrpSFʳ : {as bs : SVDesc} → (sf : SF as bs) → Set
ChangePrpSFʳ {as} {bs} sf = (s : SigVec as) → Always (ChangePrpʳ {as} {bs} sf s)

SourceSF : {as bs : SVDesc} → (sf : SF as bs) → Set
SourceSF {as} {bs} sf = (s : SigVec as) → Always (Source {as} {bs} sf s)

SourceSFʳ : {as bs : SVDesc} → (sf : SF as bs) → Set
SourceSFʳ {as} {bs} sf = (s : SigVec as) → Always (Sourceʳ {as} {bs} sf s)

ChangeExecSFʳ : {as bs : SVDesc} → (sf : SF as bs) → Set
ChangeExecSFʳ {as} {bs} sf = (s : SigVec as) → Always (ChangeExecʳ {as} {bs} sf s)

-----------------------------------------------------------------------------------

lem-CD→CDʳ→CP : {as bs : SVDesc} → (sf : SF as bs) → ChangeDepSF {as} {bs} sf → ChangeDepSFʳ {as} {bs} sf → ChangePrpSF {as} {bs} sf
lem-CD→CDʳ→CP sf chd chdr s = lem-Always⇒Gʳ (chd s) & lem-Always⇒G (chdr s)

lem-CD→CDʳ→CPʳ : {as bs : SVDesc} → (sf : SF as bs) → ChangeDepSF {as} {bs} sf → ChangeDepSFʳ {as} {bs} sf → ChangePrpSFʳ {as} {bs} sf
lem-CD→CDʳ→CPʳ {as} {bs} sf chd chdr s t = lem-CD→CDʳ→CP {as} {bs} sf chd chdr s t , chdr s t

lem-Cʳ→SL : {as bs : SVDesc} → Stability → (sf : SF as bs) → ChangelessSFʳ {as} {bs} sf → Stateless {as} {bs} sf
lem-Cʳ→SL {as} {bs} stab sf ch s₁ s₂ t₁ t₂ _ = lem-ChangelessSVʳ-EqSample {bs} stab (sf s₁) (sf s₂) t₁ t₂ (ch s₁ O s₁ (λ _ ())) (ch s₂ O s₂ (λ _ → λ ()))

lem-Cʳ→Dec : {as bs : SVDesc} → Stability → (sf : SF as bs) → ChangelessSFʳ {as} {bs} sf → Decoupled {as} {bs} sf
lem-Cʳ→Dec {as} {bs} stab sf ch s₁ s₂ t _ = Changelessʳ→AlwaysEqRep {as} {bs} stab sf ch s₁ s₂ t

lem-Srcʳ→Dec : {as bs : SVDesc} → (sf : SF as bs) → SourceSFʳ {as} {bs} sf → Decoupled {as} {bs} sf
lem-Srcʳ→Dec {as} {bs} sf src s₁ s₂ t _ = src s₁ O s₂ (λ _ ()) t

lem-Srcʳ→SD : {as bs : SVDesc} → (sf : SF as bs) → SourceSFʳ {as} {bs} sf → StrictlyDec {as} {bs} sf
lem-Srcʳ→SD {as} {bs} sf src = ı⁺ , lem-Srcʳ→SDaux
  where
        lem-Srcʳ→SDaux : StrictlyDecAux {as} {bs} sf ı⁺
        lem-Srcʳ→SDaux s₁ s₂ t with compareGeq t ı₀
        ... | geq p   = λ Heq → src s₁ O s₂ (λ _ ()) t
        ... | less p  = λ _ → src s₁ O s₂ (λ _ ()) t

-----------------------------------------------------------------------------------

-- PROPERTIES

-- Causal - identity, sfFst , sfSnd , never , now , constant , notYet , filterE, hold , edge, fromS , dfromS, liftC , liftE, delayC, delayE, delayS
-- Stateless - identity , sfFst , sfSnd , never , filterE, fromS, liftC, liftE
-- Decoupled - never , now , constant , dfromS, delayC, delayE, delayS
-- StrictlyDec - never , now , constant , delayC, delayE, delayS
-- Source - never , now , constant
-- Sourceʳ - never , now , constant
-- ChangelessSF - never , now , constant
-- ChangelessSFʳ - never
-- ChangePrp - identity , sfFst , sfSnd , never , now , constant , notYet , filterE, hold , edge, fromS, liftC, liftE
-- ChangePrpʳ - identity , sfFst , sfSnd , never , notYet , filterE, edge, fromS, liftC, liftE
-- ChangeDep - identity , sfFst , sfSnd , never , now , constant , notYet , filterE, hold , edge, fromS, liftC, liftE
-- ChangeDepʳ - identity , sfFst , sfSnd , never , notYet , filterE, edge, fromS, liftC, liftE
-- ChangeExecʳ - identity , sfFst , sfSnd , never , notYet, filterE, edge, fromS, liftC, liftE

-- Proofs that follow from transitivity are omitted (see ChangeProps and above) 

-----------------------------------------------------------------------------------

-- identity

Causal-identity : {as : SVDesc} → Causal {as} {as} (identity {as})
Causal-identity _ _ _ (eq , _) = eq

Stateless-identity : {as : SVDesc} → Stateless {as} {as} (identity {as})
Stateless-identity _ _ _ _ = id

ChangeDep-identity : {as : SVDesc} → ChangeDepSF {as} {as} (identity {as})
ChangeDep-identity _ _ _ _ _ _ = id

ChangeDepʳ-identity : {as : SVDesc} → ChangeDepSFʳ {as} {as} (identity {as})
ChangeDepʳ-identity _ _ _ _ _ _ = id

ChangeExecʳ-identity : {as : SVDesc} → ChangeExecSFʳ {as} {as} (identity {as})
ChangeExecʳ-identity _ _ _ _ _ _ un = un , (refl , λ _ _ → refl)

-----------------------------------------------------------------------------------

-- sfFst

Causal-sfFst : {as bs : SVDesc} → Causal {as , bs} {as} (sfFst {as} {bs})
Causal-sfFst (sa₁ , sb₁) (sa₂ , sb₂) t (eq , _) = fst (×-inj eq)

Stateless-sfFst : {as bs : SVDesc} → Stateless {as , bs} {as} (sfFst {as} {bs})
Stateless-sfFst (sa₁ , sb₁) (sa₂ , sb₂) t₁ t₂ = fst ∘ ×-inj

ChangeDep-sfFst : {as bs : SVDesc} → ChangeDepSF {as , bs} {as} (sfFst {as} {bs})
ChangeDep-sfFst (sa , sb) _ (sa' , sb') _ _ _ = fst

ChangeDepʳ-sfFst : {as bs : SVDesc} → ChangeDepSFʳ {as , bs} {as} (sfFst {as} {bs})
ChangeDepʳ-sfFst (sa , sb) _ (sa' , sb') _ _ _ = fst ∘ ××-swap

ChangeExecʳ-sfFst : {as bs : SVDesc} → ChangeExecSFʳ {as , bs} {as} (sfFst {as} {bs})
ChangeExecʳ-sfFst (sa , sb) _ (sa' , sb') _ _ _ (un , unch) = (fst un , fst unch) , (refl , λ _ _ → refl)

-----------------------------------------------------------------------------------

-- sfSnd

Causal-sfSnd : {as bs : SVDesc} → Causal {as , bs} {bs} (sfSnd {as} {bs})
Causal-sfSnd (sa₁ , sb₁) (sa₂ , sb₂) t (eq , _) = snd (×-inj eq)

Stateless-sfSnd : {as bs : SVDesc} → Stateless {as , bs} {bs} (sfSnd {as} {bs})
Stateless-sfSnd (sa₁ , sb₁) (sa₂ , sb₂) t₁ t₂ = snd ∘ ×-inj

ChangeDep-sfSnd : {as bs : SVDesc} → ChangeDepSF {as , bs} {bs} (sfSnd {as} {bs})
ChangeDep-sfSnd (sa , sb) _ (sa' , sb') _ _ _ = snd

ChangeDepʳ-sfSnd : {as bs : SVDesc} → ChangeDepSFʳ {as , bs} {bs} (sfSnd {as} {bs})
ChangeDepʳ-sfSnd (sa , sb) _ (sa' , sb') _ _ _ = snd ∘ ××-swap

ChangeExecʳ-sfSnd : {as bs : SVDesc} → ChangeExecSFʳ {as , bs} {bs} (sfSnd {as} {bs})
ChangeExecʳ-sfSnd (sa , sb) _ (sa' , sb') _ _ _ (un , unch) = (snd un , snd unch) , (refl , λ _ _ → refl)

-----------------------------------------------------------------------------------

-- never

ChangelessSFʳ-never : {as : SVDesc} → {A : Set} → (s : SigVec as) → Always (Changelessʳ {as} {E A} (never {as}) s)
ChangelessSFʳ-never s O      s' _ = id , λ _ _ _ → refl
ChangelessSFʳ-never s (t >0) s' _ = id , λ _ _ _ → refl

-----------------------------------------------------------------------------------

-- now

Sourceʳ-now : {as : SVDesc} → SourceSFʳ {as} {E _} (now {as})
Sourceʳ-now _ _ _ _ _ = refl

Changeless-now : {as : SVDesc} → ChangelessSF {as} {E _} (now {as})
Changeless-now _ _ _ _ _ _ _ = refl

-----------------------------------------------------------------------------------

-- constantS

Sourceʳ-constantS : {as : SVDesc} → {A : Set} → {a : A} → SourceSFʳ {as} {S A} (constantS {as} a)
Sourceʳ-constantS _ _ _ _ _ = refl

Changeless-constantS : {as : SVDesc} → {A : Set} → {a : A} → ChangelessSF {as} {S A} (constantS {as} a)
Changeless-constantS _ _ _ _ _ _ _ = refl

-----------------------------------------------------------------------------------

-- notYet

Causal-notYet : {A : Set} → Causal {E A} {E A} notYet
Causal-notYet (ma₁ , cp₁) (ma₂ , cp₂) t (eq , Heq) = ×-congL (snd (×-inj eq))

ChangeDep-notYet : {A : Set} → ChangeDepSF {E A} {E A} notYet
ChangeDep-notYet (ma , cp) t (ma' , cp') _ _ _ = id

ChangeDepʳ-notYet : {A : Set} → ChangeDepSFʳ {E A} {E A} notYet
ChangeDepʳ-notYet (ma , cp) O (ma' , cp') Heq t₂ lt (_ , eq) = id , eq
ChangeDepʳ-notYet (ma , cp) (t₁ >0) (ma' , cp') Heq t₂ lt eq = eq

-- This is proeable, just painful
-- ChangeExecʳ-notYet : {A : Set} → Stability → ChangeExecSFʳ {E A} {E A} notYet
-- ChangeExecʳ-notYet stab (ma , cp) O (just a , cp') Heq t₂ lt (spell , _) = magic spell
-- ChangeExecʳ-notYet stab (ma , cp) O (nothing , cp') _ O lt (_ , eq) = (id , eq) , refl , λ _ _ → refl
-- ChangeExecʳ-notYet stab (ma , cp) O (nothing , cp') _ (t₂ >0) lt (_ , eq) rewrite (sym (eq _)) | (lemCP-O-empty cp' (stab cp')) = (id , λ _ → refl) , refl , λ _ _ → refl
-- ChangeExecʳ-notYet stab (ma , cp) (t >0) (ma' , cp') Heq O lt (spell , eq) = (spell , eq) , {!!} , {!!}
-- ChangeExecʳ-notYet stab (ma , cp) (t >0) (ma' , cp') Heq (t₂ >0) (inl y) (spell , eq) = {!!}
-- ChangeExecʳ-notYet stab (ma , cp) (.t₂ >0) (ma' , cp') Heq (t₂ >0) (inr refl) (spell , eq) = {!!} , {!!} , {!!}

-----------------------------------------------------------------------------------

-- filterE

Causal-filterE : {A : Set} → {p : A → Bool} → Stability → Causal {E A} {E A} (filterE p)
Causal-filterE stab (ma₁ , cp₁) (ma₂ , cp₂) t (eq , Heq) with ×-inj eq
... | eqa , eqcp rewrite eqa | eqcp = refl

Stateless-filterE : {A : Set} → {p : A → Bool} → Stateless {E A} {E A} (filterE p)
Stateless-filterE (.ma₂ , cp₁) (ma₂ , cp₂) O O refl = refl
Stateless-filterE (._ , cp₁) (ma₂ , cp₂) O (t₂ >0) refl = sym (lemTDL-lookup-filter-filter (cp₂ (t₂ >0)) _ t₂)
Stateless-filterE (ma₁ , cp₁) (._ , cp₂) (t₁ >0) O refl = lemTDL-lookup-filter-filter (cp₁ (t₁ >0)) _ t₁
Stateless-filterE (ma₁ , cp₁) (ma₂ , cp₂) (t₁ >0) (t₂ >0) eq = lemTDL-lookup-filter (cp₁ (t₁ >0)) (cp₂ (t₂ >0)) _ t₁ t₂ eq

ChangeDep-filterE : {A : Set} → {p : A → Bool} → ChangeDepSF {E A} {E A} (filterE p)
ChangeDep-filterE (ma , cp) t₀ (ma' , cp') Heq t₁ lt₁ unb rewrite unb lt₁ = λ _ → refl

ChangeDepʳ-filterE : {A : Set} → {p : A → Bool} → ChangeDepSFʳ {E A} {E A} (filterE p)
ChangeDepʳ-filterE (ma , cp) O       (ma' , cp') Heq t₁ (inl lt) (un , unch) rewrite unch lt | lem-isNothing un = id , λ _ → refl
ChangeDepʳ-filterE (ma , cp) (t₀ >0) (ma' , cp') Heq t₁ (inl lt) (un , unch) rewrite unch lt = lemTDL-isNothing-lookup-filter (cp' t₁) _ t₀ un , λ _ → refl
ChangeDepʳ-filterE (ma , cp) O       (ma' , cp') Heq ._ (inr refl) (un , unch) rewrite lem-isNothing un = id , λ _ → refl
ChangeDepʳ-filterE (ma , cp) (t₀ >0) (ma' , cp') Heq ._ (inr refl) (un , unch) = lemTDL-isNothing-lookup-filter (cp' (t₀ >0)) _ t₀ un , λ _ → refl

-- ChangeExecʳ-filterE : {A : Set} → {p : A → Bool} → ChangeExecSFʳ {E A} {E A} (filterE p)

-----------------------------------------------------------------------------------

-- hold

Causal-hold : {A : Set} → {a : A} → Causal {E A} {S A} (hold a)
Causal-hold (a₁ , cp₁) (a₂ , cp₂) t (eq , Heq) with ×-inj eq
... | eqa , eqcp rewrite eqa | eqcp = refl

ChangePrp-hold : {A : Set} → {a : A} → ChangePrpSF {E A} {S A} (hold a)
ChangePrp-hold {A} {a} s t = (ChangeDep-hold t , GChangeDep-hold t) , GChangeDepʳ-hold t
  where

        ChangeDep-hold : Always (ChangeDep {E A} {S A} (hold a) s)
        ChangeDep-hold t₀ (ma , cp) Heq t₁ lt₁ = id

        GChangeDep-hold : Always (G (ChangeDep {E A} {S A} (hold a) s))
        GChangeDep-hold t₀ t₁ lt₁ (ma , cp) Heq t₂ lt₂ = id

        GChangeDepʳ-hold : Always (G (ChangeDepʳ {E A} {S A} (hold a) s))
        GChangeDepʳ-hold t₀ O () (ma , cp) Heq t₂ lt₂
        GChangeDepʳ-hold t₀ (t₁ >0) lt₁ (ma , cp) Heq t₂ lt₂ = id

-----------------------------------------------------------------------------------

-- edge

Causal-edge : Stability → Causal {S Bool} {E Unit} edge
Causal-edge stab (a₁ , cp₁) (a₂ , cp₂) t (eq , Heq) with ×-inj eq
... | eqa , eqcp rewrite eqa | eqcp = ×-congL refl

ChangeDep-edge : ChangeDepSF {S Bool} {E Unit} edge
ChangeDep-edge (a , cp) t₁ (a' , cp') Heq t₂ lt eq rewrite eq lt = λ _ → refl

-- ChangeDepʳ-edge : ChangeDepSFʳ {S Bool} {E Unit} edge
-- ChangeDepʳ-edge (a , cp) O       (a' , cp') Heq t₂ (inl lt) (un , eq) rewrite eq lt = id ,  λ _ → refl
-- ChangeDepʳ-edge (a , cp) (t₁ >0) (a' , cp') Heq t₂ (inl lt) (un , eq) rewrite eq lt = {!!} ,  λ _ → refl
-- ChangeDepʳ-edge (a , cp) .O      (a' , cp') Heq O (inr refl) (un , eq) = id , λ _ → refl
-- ChangeDepʳ-edge (a , cp) ._      (a' , cp') Heq (t₂ >0) (inr refl) (un , eq) = {!!} , λ _ → refl

-- ChangeExecʳ-edge : ChangeExecSFʳ {S Bool} {E Unit} edge

-----------------------------------------------------------------------------------

-- fromS

Causal-fromS : {A : Set} → Causal {S A} {C A} fromS
Causal-fromS s₁ s₂ t (eq , Heq) = lem-EqRep→EqSample {S _} s₁ s₂ t eq

Stateless-fromS : {A : Set} → Stateless {S A} {C A} fromS
Stateless-fromS s₁ s₂ t₁ t₂ = id

-- ChangeDep-fromS : {A : Set} → ChangeDepSF {S A} {C A} fromS
-- ChangeDep-fromS {A} s t₀ s' Heq t₃ lt unb t₁ t₂ (lt₁ , lt₂) (lt₃ , lt₄) = {!!}

-- ChangeDepʳ-fromS : {A : Set} → ChangeDepSFʳ {S A} {C A} fromS
-- ChangeDepʳ-fromS {A} s t₀ s' Heq t₃ lt (un , unb) = {!!} , {!!}

-- ChangeExecʳ-fromS : {A : Set} → ChangeExecSFʳ {S A} {C A} fromS

-----------------------------------------------------------------------------------

-- dfromS

-- Decoupled-dfromS : {A : Set} → {a : A} → Decoupled {S A} {C A} (dfromS a)
-- Decoupled-dfromS (a₁ , cp₁) (a₂ , cp₂) O Heq = refl
-- Decoupled-dfromS (a₁ , cp₁) (a₂ , cp₂) (t₁ >0) Heq with Heq O _
-- Decoupled-dfromS (._ , cp₁) (a₂ , cp₂) (t₁ >0) Heq | refl = {!seems okay!}

-----------------------------------------------------------------------------------

-- liftC

Causal-liftC : {A B : Set} → {f : A → B} → Causal {C A} {C B} (liftC f)
Causal-liftC s₁ s₂ t (eq , Heq) rewrite eq = refl

Stateless-liftC : {A B : Set} → {f : A → B} → Stateless {C A} {C B} (liftC f)
Stateless-liftC s₁ s₂ t₁ t₂ eq rewrite eq = refl

ChangeDep-liftC : {A B : Set} → {f : A → B} → ChangeDepSF {C A} {C B} (liftC f)
ChangeDep-liftC s t₀ s' (eq , Heq) t₃ lt ch t₁ t₂ p q rewrite ch t₁ t₂ p q = refl

ChangeDepʳ-liftC : {A B : Set} → {f : A → B} → ChangeDepSFʳ {C A} {C B} (liftC f)
ChangeDepʳ-liftC {_} {_} {f} s t₁ s' Heq t₄ lt₄ ((t₀ , lt₀ , ch) , Gch) = (t₀ , lt₀ , λ t₂ t₃ p q → cong f (ch t₂ t₃ p q)) , λ t₂ t₃ p q → cong f (Gch t₂ t₃ p q)

-- ChangeExecʳ-liftC : {A B : Set} → {f : A → B} → ChangeExecSFʳ {C A} {C B} (liftC f)
-- ChangeExecʳ-liftC {A} {B} {f} s t s' Heq t' p un = ChangeDepʳ-liftC {A} {B} {f} s t s' Heq t' p un , {!!} , {!!}

-----------------------------------------------------------------------------------

-- liftE

Causal-liftE : {A B : Set} → {f : A → B} → Causal {E A} {E B} (liftE f)
Causal-liftE (ma₁ , cp₁) (ma₂ , cp₂) t (eq , Heq) with ×-inj eq
... | eqma , eqcp rewrite eqma | eqcp = refl

-- Stateless-liftE : {A B : Set} → {f : A → B} → Stateless {E A} {E B} (liftE f)
-- Stateless-liftE (.ma₂ , cp₁) (ma₂ , cp₂) O O refl = refl
-- Stateless-liftE (ma₁ , cp₁) (ma₂ , cp₂) O (t₂ >0) eq = {!!}
-- Stateless-liftE (ma₁ , cp₁) (ma₂ , cp₂) (t₁ >0) O eq = {!!}
-- Stateless-liftE (ma₁ , cp₁) (ma₂ , cp₂) (t₁ >0) (t₂ >0) eq = {!!}

ChangeDep-liftE : {A B : Set} → {f : A → B} → ChangeDepSF {E A} {E B} (liftE f)
ChangeDep-liftE (ma , cp) t₀ (ma' , cp') (eq , Heq) t₂ lt eq₂ rewrite eq₂ lt = λ _ → refl

ChangeDepʳ-liftE : {A B : Set} → {f : A → B} → ChangeDepSFʳ {E A} {E B} (liftE f)
ChangeDepʳ-liftE (ma , cp) O (ma' , cp') Heq t₄ (inl lt) (un , eq) rewrite lem-isNothing un | eq lt = id , λ _ → refl
ChangeDepʳ-liftE {_} {_} {f} (ma , cp) (t₀ >0) (ma' , cp') Heq t₄ (inl lt) (un , eq) rewrite eq lt = lemTDL-isNothing-lookup-map (cp' t₄) f t₀ un , λ _ → refl
ChangeDepʳ-liftE (ma , cp) O (ma' , cp') Heq .O (inr refl) (un , eq) rewrite lem-isNothing un = id , λ _ → refl
ChangeDepʳ-liftE {_} {_} {f} (ma , cp) (t₀ >0) (ma' , cp') Heq ._ (inr refl) (un , eq) = lemTDL-isNothing-lookup-map (cp' (t₀ >0)) f t₀ un , λ _ → refl

-- ChangeExecʳ-liftE : {A B : Set} → {f : A → B} → ChangeExecSFʳ {E A} {E B} (liftE f)

-----------------------------------------------------------------------------------

-- delayC

Decoupled-delayC : {A : Set} → {d : Time⁺} → {f : Time → A} → Decoupled {C A} {C A} (delayC d f)
Decoupled-delayC {A} {d} s₁ s₂ t Heq with compareGeq t (d >0)
Decoupled-delayC {A} {d} s₁ s₂ t Heq | less p = refl
Decoupled-delayC {A} {d} s₁ s₂ t Heq | geq (inl p) = Heq (ℜ₀⁺⁺-minus t d p >0) (lem-ℜ₀-minus-<-decreasing p)
Decoupled-delayC {A} {d} s₁ s₂ ._ Heq | geq (inr refl) = Heq O _

StrictlyDec-delayC : {A : Set} → {d : Time⁺} → {f : Time → A} → StrictlyDec {C A} {C A} (delayC d f)
StrictlyDec-delayC {A} {d} {f} = (d , SD-delayC-aux)
  where
        SD-delayC-aux : StrictlyDecAux {C A} {C A} (delayC d f) d
        SD-delayC-aux s₁ s₂ t Heq with compareGeq t (d >0)
        ... | less p = refl
        ... | geq  p = fst Heq

-----------------------------------------------------------------------------------

-- delayE

Decoupled-delayE : {A : Set} → {d : Time⁺} → Decoupled {E A} {E A} (delayE d)
Decoupled-delayE {A} {d} (ma₁ , cp₁) (ma₂ , cp₂) t Heq with compareGeq t (d >0)
Decoupled-delayE {A} {d} (ma₁ , cp₁) (ma₂ , cp₂) t Heq | less p = refl
Decoupled-delayE {A} {d} (ma₁ , cp₁) (ma₂ , cp₂) t Heq | geq p with ×-inj (Heq (ℜ₀⁺₀-minus t d p) (lem-ℜ₀-minus-<-decreasing' p))
... | eq , eqcp rewrite eq | eqcp = refl

StrictlyDec-delayE : {A : Set} → {d : Time⁺} → StrictlyDec {E A} {E A} (delayE d)
StrictlyDec-delayE {A} {d} = (d , SD-delayE-aux)
  where
        SD-delayE-aux : StrictlyDecAux {E A} {E A} (delayE d) d
        SD-delayE-aux (ma₁ , cp₁) (ma₂ , cp₂) t Heq with compareGeq t (d >0)
        SD-delayE-aux (ma₁ , cp₁) (ma₂ , cp₂) t Heq | less p = refl
        SD-delayE-aux (ma₁ , cp₁) (ma₂ , cp₂) t Heq | geq p with ×-inj (fst Heq)
        ... | eq , eqcp rewrite eq | eqcp = refl

-----------------------------------------------------------------------------------

-- delayS

Decoupled-delayS : {A : Set} → {d : Time⁺} → {a : A} → Decoupled {S A} {S A} (delayS d a)
Decoupled-delayS {A} {d} (a₁ , cp₁) (a₂ , cp₂) t Heq with compareGeq t (d >0)
Decoupled-delayS {A} {d} (a₁ , cp₁) (a₂ , cp₂) t Heq | less p = refl
Decoupled-delayS {A} {d} (a₁ , cp₁) (a₂ , cp₂) t Heq | geq p with ×-inj (Heq (ℜ₀⁺₀-minus t d p) (lem-ℜ₀-minus-<-decreasing' p))
... | eq , eqcp rewrite eq | eqcp = refl

StrictlyDec-delayS : {A : Set} → {d : Time⁺} → {a : A} → StrictlyDec {S A} {S A} (delayS d a)
StrictlyDec-delayS {A} {d} {a} = (d , SD-delayS-aux)
  where
        SD-delayS-aux : StrictlyDecAux {S A} {S A} (delayS d a) d
        SD-delayS-aux (a₁ , cp₁) (a₂ , cp₂) t Heq with compareGeq t (d >0)
        SD-delayS-aux (a₁ , cp₁) (a₂ , cp₂) t Heq | less p = refl
        SD-delayS-aux (a₁ , cp₁) (a₂ , cp₂) t Heq | geq p with ×-inj (fst Heq)
        ... | eq , eqcp rewrite eq | eqcp = refl

-----------------------------------------------------------------------------------
