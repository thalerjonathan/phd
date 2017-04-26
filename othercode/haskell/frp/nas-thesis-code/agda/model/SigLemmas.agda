{-# OPTIONS --type-in-type #-}

module SigLemmas where

open import NeilPrelude
open import List hiding ([_])
open import TimeDeltaList
open import RealTime
open import SigVecs
open import Utilities
open import Properties
open import Logic
open import Maybe
open import CPLemmas
open import TimeDeltaListProps

------------------------------------------------------------------------------

ChangelessSVʳ⇒ChangelessSV : {as : SVDesc} → (s : SigVec as) → Always (ChangelessSVʳ {as} s ⇒ ChangelessSV {as} s)
ChangelessSVʳ⇒ChangelessSV s t₁ p t₂ lt = snd p t₂ lt

ChangelessSVʳ⇒Unchanging : {as : SVDesc} → (s : SigVec as) → Always (ChangelessSVʳ {as} s ⇒ Unchanging {as} s)
ChangelessSVʳ⇒Unchanging s t₁ p = fst p

------------------------------------------------------------------------------

lemS-HeqAt⇒HeqSample : {A : Set} → Stability → (s₁ s₂ : SigVec (S A)) → Always (H (EqAt {S A} s₁ s₂) ⇒ H (EqSample {S A} s₁ s₂))
lemS-HeqAt⇒HeqSample stab (a₁ , cp₁) (a₂ , cp₂) t₁ Heq t₀ lt with lemCP-Hchange⇒eq cp₁ cp₂ (stab cp₁) (stab cp₂) t₁ Heq t₀ lt | Heq O (≤<ℜ₀-trans ≤ℜ₀-min lt)
... | eq | eq0 rewrite eq with reverse (cp₂ t₀)
...   | [] = just-inj eq0
...   | (δ , a) ∷ δas = refl

lemS-HʳeqAt⇒HeqSample : {A : Set} → Stability → (s₁ s₂ : SigVec (S A)) → Always (Hʳ (EqAt {S A} s₁ s₂) ⇒ EqSample {S A} s₁ s₂)
lemS-HʳeqAt⇒HeqSample stab (a₁ , cp₁) (._ , cp₂) O (refl , _) with lemCP-O-empty cp₁ (stab cp₁) | lemCP-O-empty cp₂ (stab cp₂)
... | eq₁ | eq₂ rewrite eq₁ | eq₂ = refl
lemS-HʳeqAt⇒HeqSample stab (a₁ , cp₁) (a₂ , cp₂) (t >0) Heq with lemCP-Hʳchange⇒eq cp₁ cp₂ (stab cp₁) (stab cp₂) (t >0) Heq | snd Heq O _
... | eq | eq0 rewrite eq with reverse (cp₂ (t >0))
...    | [] = just-inj eq0
...    | (δ , a) ∷ δas = refl

lem-HeqAt⇒HeqSample : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (H (EqAt {as} s₁ s₂) ⇒ H (EqSample {as} s₁ s₂))
lem-HeqAt⇒HeqSample {C _} stab s₁ s₂ t Heq = Heq
lem-HeqAt⇒HeqSample {E _} stab s₁ s₂ t Heq = Heq
lem-HeqAt⇒HeqSample {S _} stab s₁ s₂ t Heq = lemS-HeqAt⇒HeqSample stab s₁ s₂ t Heq
lem-HeqAt⇒HeqSample {as , bs} stab (sa₁ , sb₁) (sa₂ , sb₂) t Heq with ×-inj3' Heq
... | Heqa , Heqb = ×-cong3 (lem-HeqAt⇒HeqSample {as} stab sa₁ sa₂ t Heqa) (lem-HeqAt⇒HeqSample {bs} stab sb₁ sb₂ t Heqb)

lem-HʳeqAt⇒eqSample : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (Hʳ (EqAt {as} s₁ s₂) ⇒ EqSample {as} s₁ s₂)
lem-HʳeqAt⇒eqSample {C _} stab s₁ s₂ t Heq = fst Heq
lem-HʳeqAt⇒eqSample {E _} stab s₁ s₂ t Heq = fst Heq
lem-HʳeqAt⇒eqSample {S _} stab s₁ s₂ t Heq = lemS-HʳeqAt⇒HeqSample stab s₁ s₂ t Heq
lem-HʳeqAt⇒eqSample {as , bs} stab (sa₁ , sb₁) (sa₂ , sb₂) t (eq , Heq) with ×-inj eq | ×-inj3' Heq
... | eqa , eqb | Heqa , Heqb  = ×-cong (lem-HʳeqAt⇒eqSample {as} stab sa₁ sa₂ t (eqa , Heqa)) (lem-HʳeqAt⇒eqSample {bs} stab sb₁ sb₂ t (eqb , Heqb))

lem-HʳeqAt⇒HʳeqSample : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (Hʳ (EqAt {as} s₁ s₂) ⇒ Hʳ (EqSample {as} s₁ s₂))
lem-HʳeqAt⇒HʳeqSample {as} stab s₁ s₂ t (eq , Heq) = lem-HʳeqAt⇒eqSample {as} stab s₁ s₂ t (eq , Heq) , lem-HeqAt⇒HeqSample {as} stab s₁ s₂ t Heq

lem-AlwaysEqAt→AlwaysEqSample : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (EqAt {as} s₁ s₂) → Always (EqSample {as} s₁ s₂)
lem-AlwaysEqAt→AlwaysEqSample {as} stab s₁ s₂ eqA t = lem-HʳeqAt⇒eqSample {as} stab s₁ s₂ t (eqA t , λ t' _ → eqA t')

------------------------------------------------------------------------------

-- Creating changeless signal vectors

takeChangesInclC : {A : Set} → SigVec (C A) → (t : Time) → SigVec (C A)
takeChangesInclC s t t' with compareLeq t' t
... | leq p  = s t'
... | more p = s t

takeChangesInclE : {A : Set} → SigVec (E A) → (t : Time) → SigVec (E A)
takeChangesInclE (ma , cp) t = ma , (λ t' → takeIncl t (cp t'))

takeChangesInclS : {A : Set} → SigVec (S A) → (t : Time) → SigVec (S A)
takeChangesInclS (a , cp) t = a , (λ t' → takeIncl t (cp t'))

takeChangesIncl : {as : SVDesc} → SigVec as → (t : Time) → SigVec as
takeChangesIncl {C _} s t = takeChangesInclC s t
takeChangesIncl {E _} s t = takeChangesInclE s t
takeChangesIncl {S _} s t = takeChangesInclS s t
takeChangesIncl {as , bs} (sa , sb) t = takeChangesIncl {as} sa t , takeChangesIncl {bs} sb t

lem-ChangelessSVTakeInclC : {A : Set} → (s : SigVec (C A)) → (t : Time) → ChangelessSV {C A} (takeChangesInclC  s t) t
lem-ChangelessSVTakeInclC s t₀ t₃ lt t₁ t₂ (p , q) (r , u) with compareLeq t₁ t₀ | compareLeq t₂ t₀
... | leq lt₂  | leq lt₃  = cong s (trans (≤ℜ₀-antisym lt₂ p) (≤ℜ₀-antisym r lt₃)) 
... | leq lt₂  | more lt₃ = cong s (≤ℜ₀-antisym lt₂ p)
... | more lt₂ | leq lt₃  = cong s (≤ℜ₀-antisym r lt₃)
... | more lt₂ | more lt₃ = refl

lem-ChangelessSVTakeIncl : {as : SVDesc} → Stability → (s : SigVec as) → (t : Time) → ChangelessSV {as} (takeChangesIncl {as} s t) t
lem-ChangelessSVTakeIncl {C _} stab s t = lem-ChangelessSVTakeInclC s t
lem-ChangelessSVTakeIncl {E _} stab (a , cp) t = λ t₂ lt₁ lt₂ → lemCP-stable-eq cp (stab cp) (inl lt₁)
lem-ChangelessSVTakeIncl {S _} stab (a , cp) t = λ t₂ lt₁ lt₂ → lemCP-stable-eq cp (stab cp) (inl lt₁)
lem-ChangelessSVTakeIncl {as , bs} stab (sa , sb) t = lem-ChangelessSVTakeIncl {as} stab sa t &₂
                                                    lem-ChangelessSVTakeIncl {bs} stab sb t

---------------------------------------

-- Probably redundant now

-- lem-takeInclChangeHEqAtC : {A : Set} → (s : SigVec (C A)) → (t : Time) → H (EqAt {C A} s (takeChangesInclC s t)) t
-- lem-takeInclChangeHEqAtC s t₁ t₀ lt with compareLeq t₀ t₁
-- ... | leq p = refl 
-- ... | more p = absurd (<ℜ₀-asym p lt)

-- lem-takeInclChangeHEqAtE : {A : Set} → (s : SigVec (E A)) → (t : Time) → H (EqAt {E A} s (takeChangesInclE s t)) t
-- lem-takeInclChangeHEqAtE (ma , cp) t₁ O lt = refl
-- lem-takeInclChangeHEqAtE (ma , cp) t₁ (t₀ >0) lt = lemCP-lookupTaken (inl lt) cp

-- lem-takeInclChangeHEqAtS : {A : Set} → (s : SigVec (S A)) → (t : Time) → H (EqAt {S A} s (takeChangesInclS s t)) t
-- lem-takeInclChangeHEqAtS (a , cp) t₁ O lt = refl
-- lem-takeInclChangeHEqAtS (ma , cp) t₁ (t₀ >0) lt = lemCP-lookupTaken (inl lt) cp

-- lem-takeInclChangeHEqAt : {as : SVDesc} → (s : SigVec as) → (t : Time) → H (EqAt {as} s (takeChangesIncl {as} s t)) t
-- lem-takeInclChangeHEqAt {C _} s t = lem-takeInclChangeHEqAtC s t
-- lem-takeInclChangeHEqAt {E _} s t = lem-takeInclChangeHEqAtE s t
-- lem-takeInclChangeHEqAt {S _} s t = lem-takeInclChangeHEqAtS s t
-- lem-takeInclChangeHEqAt {as , bs} (sa , sb) t = ×-cong3 (lem-takeInclChangeHEqAt {as} sa t) (lem-takeInclChangeHEqAt {bs} sb t)

-- lem-takeInclChangeEqAtC : {A : Set} → (s : SigVec (C A)) → (t : Time) → EqAt {C A} s (takeChangesInclC s t) t
-- lem-takeInclChangeEqAtC s t with compareLeq t t
-- ... | leq p = refl
-- ... | more p = refl

-- lem-takeInclChangeEqAtE : {A : Set} → (s : SigVec (E A)) → (t : Time) → EqAt {E A} s (takeChangesInclE s t) t
-- lem-takeInclChangeEqAtE (ma , cp) O = refl
-- lem-takeInclChangeEqAtE (ma , cp) (t >0) = lemCP-lookupTaken {_} {t >0} (inr refl) cp

-- lem-takeInclChangeEqAtS : {A : Set} → (s : SigVec (S A)) → (t : Time) → EqAt {S A} s (takeChangesInclS s t) t
-- lem-takeInclChangeEqAtS (a , cp) O = refl
-- lem-takeInclChangeEqAtS (a , cp) (t >0) = lemCP-lookupTaken {_} {t >0} (inr refl) cp

-- lem-takeInclChangeEqAt : {as : SVDesc} → (s : SigVec as) → (t : Time) → EqAt {as} s (takeChangesIncl {as} s t) t
-- lem-takeInclChangeEqAt {C _} s t = lem-takeInclChangeEqAtC s t
-- lem-takeInclChangeEqAt {E _} s t = lem-takeInclChangeEqAtE s t
-- lem-takeInclChangeEqAt {S _} s t = lem-takeInclChangeEqAtS s t
-- lem-takeInclChangeEqAt {as , bs} (sa , sb) t = ×-cong (lem-takeInclChangeEqAt {as} sa t) (lem-takeInclChangeEqAt {bs} sb t)

-- lem-takeInclChangeHʳEqAt : {as : SVDesc} → (s : SigVec as) → (t : Time) → Hʳ (EqAt {as} s (takeChangesIncl {as} s t)) t
-- lem-takeInclChangeHʳEqAt {as} = lem-takeInclChangeEqAt {as} &₂ lem-takeInclChangeHEqAt {as}

---------------------------------------

lemC-takeInclChangeHEqRep : {A : Set} → (s : SigVec (C A)) → (t : Time) → H (EqRep {C A} s (takeChangesInclC s t)) t
lemC-takeInclChangeHEqRep s t₁ t₀ lt with compareLeq t₀ t₁
... | leq p = refl 
... | more p = absurd (<ℜ₀-asym p lt)

lemE-takeInclChangeHEqRep : {A : Set} → Stability → (s : SigVec (E A)) → (t : Time) → H (EqRep {E A} s (takeChangesInclE s t)) t
lemE-takeInclChangeHEqRep stab (ma , cp) t₁ t₀ lt = ×-congL (sym (lemCP-take-Beyond cp (stab cp) (inl lt)))

lemS-takeInclChangeHEqRep : {A : Set} → Stability → (s : SigVec (S A)) → (t : Time) → H (EqRep {S A} s (takeChangesInclS s t)) t
lemS-takeInclChangeHEqRep stab (a , cp) t₁ t₀ lt = ×-congL (sym (lemCP-take-Beyond cp (stab cp) (inl lt)))

lem-takeInclChangeHEqRep : {as : SVDesc} → Stability → (s : SigVec as) → (t : Time) → H (EqRep {as} s (takeChangesIncl {as} s t)) t
lem-takeInclChangeHEqRep {C _} stab s t = lemC-takeInclChangeHEqRep s t
lem-takeInclChangeHEqRep {E _} stab s t = lemE-takeInclChangeHEqRep stab s t
lem-takeInclChangeHEqRep {S _} stab s t = lemS-takeInclChangeHEqRep stab s t
lem-takeInclChangeHEqRep {as , bs} stab (sa , sb) t = ×-cong3 (lem-takeInclChangeHEqRep {as} stab sa t) (lem-takeInclChangeHEqRep {bs} stab sb t)

lemC-takeInclChangeEqRep : {A : Set} → (s : SigVec (C A)) → (t : Time) → EqRep {C A} s (takeChangesInclC s t) t
lemC-takeInclChangeEqRep s t with compareLeq t t
... | leq p = refl
... | more p = refl

lemE-takeInclChangeEqRep : {A : Set} → Stability → (s : SigVec (E A)) → (t : Time) → EqRep {E A} s (takeChangesInclE s t) t
lemE-takeInclChangeEqRep stab (ma , cp) t = ×-congL (stab cp (inr refl))

lemS-takeInclChangeEqRep : {A : Set} → Stability → (s : SigVec (S A)) → (t : Time) → EqRep {S A} s (takeChangesInclS s t) t
lemS-takeInclChangeEqRep stab (a , cp) t = ×-congL (stab cp (inr refl))

lem-takeInclChangeEqRep : {as : SVDesc} → Stability → (s : SigVec as) → (t : Time) → EqRep {as} s (takeChangesIncl {as} s t) t
lem-takeInclChangeEqRep {C _} stab s t = lemC-takeInclChangeEqRep s t
lem-takeInclChangeEqRep {E _} stab s t = lemE-takeInclChangeEqRep stab  s t
lem-takeInclChangeEqRep {S _} stab s t = lemS-takeInclChangeEqRep stab  s t
lem-takeInclChangeEqRep {as , bs} stab (sa , sb) t = ×-cong (lem-takeInclChangeEqRep {as} stab sa t) (lem-takeInclChangeEqRep {bs} stab sb t)

lem-takeInclChangeHʳEqRep : {as : SVDesc} → Stability → (s : SigVec as) → (t : Time) → Hʳ (EqRep {as} s (takeChangesIncl {as} s t)) t
lem-takeInclChangeHʳEqRep {as} stab = lem-takeInclChangeEqRep {as} stab &₂ lem-takeInclChangeHEqRep {as} stab

---------------------------------------

changelessBeyond : {as : SVDesc} → Stability → (s : SigVec as) → (t : Time) → Σ (SigVec as) (λ s' → (ChangelessSV {as} s' t × Hʳ (EqRep {as} s s') t))
changelessBeyond {as} stab s t = takeChangesIncl {as} s t , lem-ChangelessSVTakeIncl {as} stab s t , lem-takeInclChangeHʳEqRep {as} stab s t

------------------------------------------------------------------------------

-- Doesn't work for C Signals !!!

-- takeChangesExclC : {A : Set} → SigVec (C A) → (t : Time) → SigVec (C A)
-- takeChangesExclC s t t' with compareGeq t' t
-- ... | less p = s t'
-- ... | geq  p = s ?

------------------------------------------------------------------------------

lemE-rep-eq-stable : {A : Set} → {t₀ t₁ : Time} → Stability → (s₁ s₂ : SigVec (E A)) → t₀ ≤ t₁ → EqRep {E A} s₁ s₂ t₁ → EqRep {E A} s₁ s₂ t₀
lemE-rep-eq-stable stab (ma₁ , cp₁) (ma₂ , cp₂) lt eq with ×-inj eq
... | eqma , eqcp = ×-cong eqma (lemCP-eq-stable cp₁ cp₂ (stab cp₁) (stab cp₂) lt eqcp)

lemS-rep-eq-stable : {A : Set} → {t₀ t₁ : Time} → Stability → (s₁ s₂ : SigVec (S A)) → t₀ ≤ t₁ → EqRep {S A} s₁ s₂ t₁ → EqRep {S A} s₁ s₂ t₀
lemS-rep-eq-stable stab (a₁ , cp₁) (a₂ , cp₂) lt eq with ×-inj eq
... | eqa , eqcp = ×-cong eqa (lemCP-eq-stable cp₁ cp₂ (stab cp₁) (stab cp₂) lt eqcp)

------------------------------------------------------------------------------

lemE-rep-at : {A : Set} → (s₁ s₂ : SigVec (E A)) → Always (EqRep {E A} s₁ s₂ ⇒ EqAt {E A} s₁ s₂)
lemE-rep-at (ma₁ , cp₁) (ma₂ , cp₂) t eq with ×-inj eq
lemE-rep-at (.ma₂ , cp₁) (ma₂ , cp₂) O eq | refl , eqcp = refl
lemE-rep-at (.ma₂ , cp₁) (ma₂ , cp₂) (t >0) eq | refl , eqcp = cong2R lookupTDL⁺ eqcp

lemS-rep-at : {A : Set} → (s₁ s₂ : SigVec (S A)) → Always (EqRep {S A} s₁ s₂ ⇒ EqAt {S A} s₁ s₂)
lemS-rep-at (a₁ , cp₁) (a₂ , cp₂) t eq with ×-inj eq
lemS-rep-at (.a₂ , cp₁) (a₂ , cp₂) O eq | refl , eqcp = refl
lemS-rep-at (.a₂ , cp₁) (a₂ , cp₂) (t >0) eq | refl , eqcp = cong2R lookupTDL⁺ eqcp

lem-rep-at : {as : SVDesc} → (s₁ s₂ : SigVec as) → Always (EqRep {as} s₁ s₂ ⇒ EqAt {as} s₁ s₂)
lem-rep-at {C _} s₁ s₂ t eq = eq
lem-rep-at {E _} s₁ s₂ t eq = lemE-rep-at s₁ s₂ t eq
lem-rep-at {S _} s₁ s₂ t eq = lemS-rep-at s₁ s₂ t eq
lem-rep-at {as , bs} (sa₁ , sb₁) (sa₂ , sb₂) t eq with ×-inj eq
... | eqa , eqb = ×-cong (lem-rep-at {as} sa₁ sa₂ t eqa) (lem-rep-at {bs} sb₁ sb₂ t eqb)

lem-rep-Hat : {as : SVDesc} → (s₁ s₂ : SigVec as) → Always (H (EqRep {as} s₁ s₂) ⇒ H (EqAt {as} s₁ s₂))
lem-rep-Hat {as} s₁ s₂ t eq t' lt = lem-rep-at {as} s₁ s₂ t' (eq t' lt)

lem-rep-Hʳat : {as : SVDesc} → (s₁ s₂ : SigVec as) → Always (Hʳ (EqRep {as} s₁ s₂) ⇒ Hʳ (EqAt {as} s₁ s₂))
lem-rep-Hʳat {as} s₁ s₂ t = lem-rep-at {as} s₁ s₂ t ∥ lem-rep-Hat {as} s₁ s₂ t


------------------------------------------------------------------------------

-- FALSE!!!
-- lemE-EqRep→EqSample : {A : Set} → (s₁ s₂ : SigVec (E A)) → (t₁ t₂ : Time) → rep {E A} s₁ t₁ ≡ rep {E A} s₂ t₂ → sample (E A) s₁ t₁ ≡ sample (E A) s₂ t₂
-- lemE-EqRep→EqSample (ma₁ , cp₁) (ma₂ , cp₂) t₁ t₂ eq with ×-inj eq
-- lemE-EqRep→EqSample (ma₁ , cp₁) (.ma₁ , cp₂) t₁ t₂ eq | refl , eqcp = lemCP-occ-eq cp₁ cp₂ t₁ t₂ eqcp

-- lemS-EqRep→EqSample : {A : Set} → (s₁ s₂ : SigVec (S A)) → (t₁ t₂ : Time) → rep {S A} s₁ t₁ ≡ rep {S A} s₂ t₂ → sample (S A) s₁ t₁ ≡ sample (S A) s₂ t₂
-- lemS-EqRep→EqSample (a₁ , cp₁) (a₂ , cp₂) t₁ t₂ eq with ×-inj eq
-- lemS-EqRep→EqSample (a₁ , cp₁) (.a₁ , cp₂) t₁ t₂ eq | refl , eqcp rewrite eqcp with reverse (cp₂ t₂)
-- ... | [] = refl
-- ... | (_ , a) ∷ _ = refl 

-- lem-EqRep→EqSample : {as : SVDesc} → (s₁ s₂ : SigVec as) → (t₁ t₂ : Time) → rep {as} s₁ t₁ ≡ rep {as} s₂ t₂ → sample as s₁ t₁ ≡ sample as s₂ t₂
-- lem-EqRep→EqSample {C _} s₁ s₂ t₁ t₂ eq = eq
-- lem-EqRep→EqSample {E _} s₁ s₂ t₁ t₂ eq = lemE-EqRep→EqSample s₁ s₂ t₁ t₂ eq
-- lem-EqRep→EqSample {S _} s₁ s₂ t₁ t₂ eq = lemS-EqRep→EqSample s₁ s₂ t₁ t₂ eq
-- lem-EqRep→EqSample {as , bs} (sa₁ , sb₁) (sa₂ , sb₂) t₁ t₂ eq with ×-inj eq
-- ... | eqa , eqb = ×-cong (lem-EqRep→EqSample {as} sa₁ sa₂ t₁ t₂ eqa)
--                          (lem-EqRep→EqSample {bs} sb₁ sb₂ t₁ t₂ eqb)

lemE-EqRep→EqSample : {A : Set} → (s₁ s₂ : SigVec (E A)) → Always (EqRep {E A} s₁ s₂ ⇒ EqSample {E A} s₁ s₂)
lemE-EqRep→EqSample (ma₁ , cp₁) (ma₂ , cp₂) t eq with ×-inj eq
lemE-EqRep→EqSample (ma₁ , cp₁) (.ma₁ , cp₂) O eq | refl , eqcp = refl
lemE-EqRep→EqSample (ma₁ , cp₁) (.ma₁ , cp₂) (t >0) eq | refl , eqcp rewrite eqcp = refl

lemS-EqRep→EqSample : {A : Set} → (s₁ s₂ : SigVec (S A)) → Always (EqRep {S A} s₁ s₂ ⇒ EqSample {S A} s₁ s₂)
lemS-EqRep→EqSample (a₁ , cp₁) (a₂ , cp₂) t eq with ×-inj eq
lemS-EqRep→EqSample (a₁ , cp₁) (.a₁ , cp₂) t eq | refl , eqcp rewrite eqcp with reverse (cp₂ t)
... | [] = refl
... | ((_ , a) ∷ _) = refl

lem-EqRep→EqSample : {as : SVDesc} → (s₁ s₂ : SigVec as) → Always (EqRep {as} s₁ s₂ ⇒ EqSample {as} s₁ s₂)
lem-EqRep→EqSample {C _} s₁ s₂ t eq = eq
lem-EqRep→EqSample {E _} s₁ s₂ t eq = lemE-EqRep→EqSample s₁ s₂ t eq 
lem-EqRep→EqSample {S _} s₁ s₂ t eq = lemS-EqRep→EqSample s₁ s₂ t eq
lem-EqRep→EqSample {as , bs} (sa₁ , sb₁) (sa₂ , sb₂) t eq with ×-inj eq
... | eqa , eqb = ×-cong (lem-EqRep→EqSample {as} sa₁ sa₂ t eqa) (lem-EqRep→EqSample {bs} sb₁ sb₂ t eqb)

------------------------------------------------------------------------------

lem-UB-rep' : {as : SVDesc} → {t₀ t₂ : Time} → t₀ < t₂ → (s : SigVec as) → UnchangingBetween {as} s t₀ t₂ → rep {as} s t₀ ≡ rep {as} s t₂
lem-UB-rep' {C _} lt s un = un _ _ (inr refl , inl lt) (inl lt , inr refl)
lem-UB-rep' {E _} lt (ma , cp) un = ×-congL (un lt)
lem-UB-rep' {S _} lt (a , cp)  un = ×-congL (un lt)
lem-UB-rep' {as , bs} lt (sa , sb) (una , unb) = ×-cong (lem-UB-rep' {as} lt sa una) (lem-UB-rep' {bs} lt sb unb)

lem-UB-rep : {as : SVDesc} → {t₀ t₂ : Time} → t₀ ≤ t₂ → (s : SigVec as) → UnchangingBetween {as} s t₀ t₂ → rep {as} s t₀ ≡ rep {as} s t₂
lem-UB-rep (inr refl) _ _ = refl
lem-UB-rep {as} (inl lt) s un = lem-UB-rep' {as} lt s un

-- False!!!
-- lem-UB-sample' : {as : SVDesc} → {t₀ t₂ : Time} → t₀ < t₂ → (s : SigVec as) → UnchangingBetween {as} s t₀ t₂ → sample as s t₀ ≡ sample as s t₂
-- lem-UB-sample' {C _} lt s ub = ub _ _ (inr refl , inl lt) (inl lt , inr refl)
-- lem-UB-sample' {E _} lt (ma , cp) ub = {!!}
-- lem-UB-sample' {S _} {_} {t₂} lt (a , cp) ub rewrite (ub lt) with reverse (cp t₂)
-- ... | [] = refl
-- ... | (_ , a') ∷ _ = refl
-- lem-UB-sample' {as , bs} lt (sa , sb) (una , unb) = ×-cong (lem-UB-sample' {as} lt sa una) (lem-UB-sample' {bs} lt sb unb)

-- lem-UB-sample : {as : SVDesc} → {t₀ t₂ : Time} → t₀ ≤ t₂ → (s : SigVec as) → UnchangingBetween {as} s t₀ t₂ → sample as s t₀ ≡ sample as s t₂
-- lem-UB-sample {as} (inl lt) s ub = lem-UB-sample' {as} lt s ub
-- lem-UB-sample (inr refl) s ub = refl

------------------------------------------------------------------------------

lemS-HʳEqAt⇒Eq : {A : Set} →  Stability → (s₁ s₂ : SigVec (S A)) → Always (Hʳ (EqAt {S A} s₁ s₂) ⇒ EqRep {S A} s₁ s₂)
lemS-HʳEqAt⇒Eq stab (.ma₂ , cp₁) (ma₂ , cp₂) O (refl , Heq) = ×-congL (lemCP-O-equal cp₁ cp₂ (stab cp₁) (stab cp₂))
lemS-HʳEqAt⇒Eq stab (ma₁ , cp₁) (ma₂ , cp₂) (t >0) (eq , Heq) with Heq O _
lemS-HʳEqAt⇒Eq stab (.ma₂ , cp₁) (ma₂ , cp₂) (t >0) (eq , Heq) | refl = ×-congL (lemCP-Hʳchange⇒eq cp₁ cp₂ (stab cp₁) (stab cp₂) (t >0) (eq , Heq))

lemE-HʳEqAt⇒Eq : {A : Set} → Stability → (s₁ s₂ : SigVec (E A)) → Always (Hʳ (EqAt {E A} s₁ s₂) ⇒ EqRep {E A} s₁ s₂)
lemE-HʳEqAt⇒Eq stab (.ma₂ , cp₁) (ma₂ , cp₂) O (refl , Heq) = ×-congL (lemCP-O-equal cp₁ cp₂ (stab cp₁) (stab cp₂))
lemE-HʳEqAt⇒Eq stab (ma₁ , cp₁) (ma₂ , cp₂) (t >0) (eq , Heq) with Heq O _
lemE-HʳEqAt⇒Eq stab (.ma₂ , cp₁) (ma₂ , cp₂) (t >0) (eq , Heq) | refl = ×-congL (lemCP-Hʳocc⇒eq cp₁ cp₂ (stab cp₁) (stab cp₂) (t >0) (eq , Heq))

lem-HʳEqAt⇒Eq : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (Hʳ (EqAt {as} s₁ s₂) ⇒ EqRep {as} s₁ s₂)
lem-HʳEqAt⇒Eq {C _} stab s₁ s₂ t (eq , _) = eq
lem-HʳEqAt⇒Eq {E _} stab s₁ s₂ t eq = lemE-HʳEqAt⇒Eq stab s₁ s₂ t eq
lem-HʳEqAt⇒Eq {S _} stab s₁ s₂ t eq = lemS-HʳEqAt⇒Eq stab s₁ s₂ t eq
lem-HʳEqAt⇒Eq {as , bs} stab (sa₁ , sb₁) (sa₂ , sb₂) t (eq , Heq) with ×-inj eq | ×-inj3' Heq
... | eqa , eqb | Heqa , Heqb = ×-cong (lem-HʳEqAt⇒Eq {as} stab sa₁ sa₂ t (eqa , Heqa)) (lem-HʳEqAt⇒Eq {bs} stab sb₁ sb₂ t (eqb , Heqb))

lem-HEqAt⇒EqH : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (H (EqAt {as} s₁ s₂) ⇒ H (EqRep {as} s₁ s₂))
lem-HEqAt⇒EqH {as} stab s₁ s₂ t Heq t' lt = lem-HʳEqAt⇒Eq {as} stab s₁ s₂ t' (Heq t' lt , λ t'' lt' → Heq t'' (<ℜ₀-trans lt' lt))

lem-HʳEqAt⇒EqHʳ : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (Hʳ (EqAt {as} s₁ s₂) ⇒ Hʳ (EqRep {as} s₁ s₂))
lem-HʳEqAt⇒EqHʳ {as} stab s₁ s₂ t (eq , Heq) = lem-HʳEqAt⇒Eq {as} stab s₁ s₂ t (eq , Heq) , lem-HEqAt⇒EqH {as} stab s₁ s₂ t Heq

------------------------------------------------------------------------------

lem-AlwaysEqAt→AlwaysEqRep : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (EqAt {as} s₁ s₂) → Always (EqRep {as} s₁ s₂)
lem-AlwaysEqAt→AlwaysEqRep {as} stab s₁ s₂ eq t = lem-HʳEqAt⇒Eq {as} stab s₁ s₂ t (eq t , λ t' _ → eq t')

lem-AlwaysEqAt→GʳEqRep : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (EqAt {as} s₁ s₂) → Always (Gʳ (EqRep {as} s₁ s₂))
lem-AlwaysEqAt→GʳEqRep {as} stab s₁ s₂ eq t = lem-AlwaysEqAt→AlwaysEqRep {as} stab s₁ s₂ eq t , λ t' _ → lem-AlwaysEqAt→AlwaysEqRep {as} stab s₁ s₂ eq t'

------------------------------------------------------------------------------

lemC-UB-reduce : {A : Set} → {t₀ t₁ t₂ t₃ : Time} → (s : SigVec (C A)) → t₁ ∈ [ t₀ , t₃ ] → t₂ ∈ [ t₀ , t₃ ] → UnchangingBetween {C A} s t₀ t₃ → UnchangingBetween {C A} s t₁ t₂
lemC-UB-reduce s (p₁ , p₂) (q₁ , q₂) un tx ty (r₁ , r₂) (u₁ , u₂) = un tx ty (≤ℜ₀-trans p₁ r₁ , ≤ℜ₀-trans r₂ q₂) (≤ℜ₀-trans p₁ u₁ , ≤ℜ₀-trans u₂ q₂)

lem-UB-reduce : {as : SVDesc} → {t₀ t₁ t₂ t₃ : Time} → Stability → (s : SigVec as) → t₁ ∈ [ t₀ , t₃ ] → t₂ ∈ [ t₀ , t₃ ] → UnchangingBetween {as} s t₀ t₃ → UnchangingBetween {as} s t₁ t₂
lem-UB-reduce {C _} stab s p q = lemC-UB-reduce s p q 
lem-UB-reduce {E _} stab (ma , cp) (p₁ , p₂) (q₁ , q₂) = λ un lt → lemCP-stable2 cp (stab cp) (p₁ , p₂) (q₁ , q₂) (lem-eq-strict cp un (≤ℜ₀-trans q₁ q₂))
lem-UB-reduce {S _} stab (a , cp) (p₁ , p₂) (q₁ , q₂) = λ un lt → lemCP-stable2 cp (stab cp) (p₁ , p₂) (q₁ , q₂) (lem-eq-strict cp un (≤ℜ₀-trans q₁ q₂))
lem-UB-reduce {as , bs} stab (sa , sb) p q = lem-UB-reduce {as} stab sa p q ∥ lem-UB-reduce {bs} stab sb p q

lem-UB-reduceR : {as : SVDesc} → {t₀ t₁ t₂ : Time} → Stability → (s : SigVec as) → t₀ ≤ t₁ → t₁ ≤ t₂ → UnchangingBetween {as} s t₀ t₂ → UnchangingBetween {as} s t₁ t₂
lem-UB-reduceR {as} stab s lt₁ lt₂ = lem-UB-reduce {as} stab s (lt₁ , lt₂) (≤ℜ₀-trans lt₁ lt₂ , inr refl)

------------------------------------------------------------------------------

lemC-UB→U-R : {A : Set} → (t₀ t₂ : Time) → (s : SigVec (C A)) → t₀ < t₂ → UnchangingBetween {C A} s t₀ t₂ → Unchanging {C A} s t₂
lemC-UB→U-R t₀ t₃ s lt unch = t₀ , lt , lemC-UB→U-R-aux
  where
     lemC-UB→U-R-aux : (t₁ t₂ : Time) → t₁ ∈ [ t₀ , t₃ ] → t₂ ∈ [ t₀ , t₃ ] → s t₁ ≡ s t₂
     lemC-UB→U-R-aux t₁ t₂ (p , q) (r , u) = unch t₁ t₂ (p , q) (r , u)

lemE-EqRep→U : {A : Set} → {t₀ : Time} → Stability → (t₂ : Time) → (s : SigVec (E A)) → t₀ < t₂ → rep {E A} s t₀ ≡ rep {E A} s t₂ → Unchanging {E A} s t₂
lemE-EqRep→U stab O s () eq
lemE-EqRep→U stab (t₂ >0) (ma , cp) lt eq with ×-inj eq
... | refl , eqcp rewrite lemCP-eq-lookup lt cp (stab cp) eqcp = id

lemS-EqRep→U : {A : Set} → {t₀ : Time} → Stability → (t₂ : Time) → (s : SigVec (S A)) → t₀ < t₂ → rep {S A} s t₀ ≡ rep {S A} s t₂ → Unchanging {S A} s t₂
lemS-EqRep→U stab O s () eq
lemS-EqRep→U stab (t₂ >0) (a , cp) lt eq with ×-inj eq
... | refl , eqcp rewrite lemCP-eq-lookup lt cp (stab cp) eqcp = id

lemE-UB→U-R : {A : Set} → {t₀ : Time} → Stability → (t₂ : Time) → (s : SigVec (E A)) → t₀ < t₂ → UnchangingBetween {E A} s t₀ t₂ → Unchanging {E A} s t₂
lemE-UB→U-R stab t₂ s lt un = lemE-EqRep→U stab t₂ s lt (lem-UB-rep {E _} (inl lt) s un)

lemS-UB→U-R : {A : Set} → {t₀ : Time} → Stability → (t₂ : Time) → (s : SigVec (S A)) → t₀ < t₂ → UnchangingBetween {S A} s t₀ t₂ → Unchanging {S A} s t₂
lemS-UB→U-R stab t₂ s lt un = lemS-EqRep→U stab t₂ s lt (lem-UB-rep {S _} (inl lt) s un)

lem-UB→U-R : {as : SVDesc} → {t₀ t₁ : Time} → Stability → (s : SigVec as) → t₀ < t₁ → UnchangingBetween {as} s t₀ t₁ → Unchanging {as} s t₁
lem-UB→U-R {C _} stab s lt = lemC-UB→U-R _ _ s lt
lem-UB→U-R {E _} stab s lt = lemE-UB→U-R stab _ s lt
lem-UB→U-R {S _} stab s lt = lemS-UB→U-R stab _ s lt
lem-UB→U-R {as , bs} stab (sa , sb) lt = lem-UB→U-R {as} stab sa lt ∥ lem-UB→U-R {bs} stab sb lt

lem-UB→U : {as : SVDesc} → {t₀ t₁ t₂ : Time} → Stability → (s : SigVec as) → t₁ ∈ ⟨ t₀ , t₂ ] → UnchangingBetween {as} s t₀ t₂ → Unchanging {as} s t₁
lem-UB→U {as} stab s (lt₁ , lt₂) unch = lem-UB→U-R {as} stab s lt₁ (lem-UB-reduce {as} stab s (inr refl , ≤ℜ₀-trans (inl lt₁) lt₂) (inl lt₁ , lt₂) unch)

------------------------------------------------------------------------------

lemC-UB : {A : Set} → {t₁ t₂ : Time} → (s : SigVec (C A)) → t₁ ≤ t₂ → UnchangingBetween {C A} s t₂ t₁
lemC-UB s lt t₁' t₂' (p₁ , p₂) (q₁ , q₂) = cong s (≤ℜ₀-antisym (≤ℜ₀-trans p₂ (≤ℜ₀-trans lt q₁)) (≤ℜ₀-trans q₂ (≤ℜ₀-trans lt p₁)))

lem-UB : {as : SVDesc} → {t₁ t₂ : Time} → (s : SigVec as) → t₁ ≤ t₂ → UnchangingBetween {as} s t₂ t₁
lem-UB {C _} s lt = lemC-UB s lt
lem-UB {E _} (ma , cp) p = (λ q → absurd (<≤ℜ₀-asym q p))
lem-UB {S _} (a , cp) p = (λ q → absurd (<≤ℜ₀-asym q p))
lem-UB {as , bs} (sa , sb) lt = lem-UB {as} sa lt , lem-UB {bs} sb lt

------------------------------------------------------------------------------

lemE-U→nothing : {A : Set} → (s : SigVec (E A)) → (t : Time) → Unchanging {E A} s t → at (E A) s t ≡ nothing
lemE-U→nothing (ma , cp) O un = lem-isNothing un
lemE-U→nothing (ma , cp) (t >0) un = lem-isNothing un

lemE-U-Eq : {A : Set} → (s₁ s₂ : SigVec (E A)) → Always (Unchanging {E A} s₁ ⇒ Unchanging {E A} s₂ ⇒ EqAt {E A} s₁ s₂)
lemE-U-Eq s₁ s₂ t un₁ un₂ = trans (lemE-U→nothing s₁ t un₁) (sym (lemE-U→nothing s₂ t un₂))

lemE-HEq⇒U⇒HʳEq : {A : Set} → (s₁ s₂ : SigVec (E A)) → Always (H (EqAt {E A} s₁ s₂) ⇒ Unchanging {E A} s₁ ⇒ Unchanging {E A} s₂ ⇒ Hʳ (EqAt {E A} s₁ s₂))
lemE-HEq⇒U⇒HʳEq s₁ s₂ t Heq un₁ un₂ = lemE-U-Eq s₁ s₂ t un₁ un₂ , Heq

lemE-HEq⇒U⇒EqRep : {A : Set} → Stability → (s₁ s₂ : SigVec (E A)) → Always (H (EqAt {E A} s₁ s₂) ⇒ Unchanging {E A} s₁ ⇒ Unchanging {E A} s₂ ⇒ EqRep {E A} s₁ s₂)
lemE-HEq⇒U⇒EqRep stab s₁ s₂ t Heq un₁ un₂ = lemE-HʳEqAt⇒Eq stab s₁ s₂ t (lemE-HEq⇒U⇒HʳEq s₁ s₂ t Heq un₁ un₂)

lemE-EqH⇒U⇒EqR : {A : Set} → Stability → (s₁ s₂ : SigVec (E A)) → Always (H (EqRep {E A} s₁ s₂) ⇒ Unchanging {E A} s₁ ⇒ Unchanging {E A} s₂ ⇒ EqRep {E A} s₁ s₂)
lemE-EqH⇒U⇒EqR stab s₁ s₂ t Heq un₁ un₂ = lemE-HEq⇒U⇒EqRep stab s₁ s₂ t (lem-rep-Hat {E _} s₁ s₂ t Heq) un₁ un₂

------------------------------------------------------------------------------

lemS-U→nothing : {A : Set} → (s : SigVec (S A)) → (t : Time) → Unchanging {S A} s t → at (S A) s t ≡ nothing
lemS-U→nothing (a , cp) O ()
lemS-U→nothing (a , cp) (t >0) un = lem-isNothing un

lemS-U-Eq : {A : Set} → (s₁ s₂ : SigVec (S A)) → Always (Unchanging {S A} s₁ ⇒ Unchanging {S A} s₂ ⇒ EqAt {S A} s₁ s₂)
lemS-U-Eq s₁ s₂ t un₁ un₂ = trans (lemS-U→nothing s₁ t un₁) (sym (lemS-U→nothing s₂ t un₂))

lemS-HEq⇒U⇒HʳEq : {A : Set} → (s₁ s₂ : SigVec (S A)) → Always (H (EqAt {S A} s₁ s₂) ⇒ Unchanging {S A} s₁ ⇒ Unchanging {S A} s₂ ⇒ Hʳ (EqAt {S A} s₁ s₂))
lemS-HEq⇒U⇒HʳEq s₁ s₂ t Heq un₁ un₂ = lemS-U-Eq s₁ s₂ t un₁ un₂ , Heq

lemS-HEq⇒U⇒EqRep : {A : Set} → Stability → (s₁ s₂ : SigVec (S A)) → Always (H (EqAt {S A} s₁ s₂) ⇒ Unchanging {S A} s₁ ⇒ Unchanging {S A} s₂ ⇒ EqRep {S A} s₁ s₂)
lemS-HEq⇒U⇒EqRep stab s₁ s₂ t Heq un₁ un₂ = lemS-HʳEqAt⇒Eq stab s₁ s₂ t (lemS-HEq⇒U⇒HʳEq s₁ s₂ t Heq un₁ un₂)

lemS-EqH⇒U⇒EqR : {A : Set} → Stability → (s₁ s₂ : SigVec (S A)) → Always (H (EqRep {S A} s₁ s₂) ⇒ Unchanging {S A} s₁ ⇒ Unchanging {S A} s₂ ⇒ EqRep {S A} s₁ s₂)
lemS-EqH⇒U⇒EqR stab s₁ s₂ t Heq un₁ un₂ = lemS-HEq⇒U⇒EqRep stab s₁ s₂ t (lem-rep-Hat {S _} s₁ s₂ t Heq) un₁ un₂

------------------------------------------------------------------------------

lemC-EqH⇒U⇒EqR : {A : Set} → (s₁ s₂ : SigVec (C A)) → Always (H (EqRep {C A} s₁ s₂) ⇒ Unchanging {C A} s₁ ⇒ Unchanging {C A} s₂ ⇒ s₁ ≐ s₂)
lemC-EqH⇒U⇒EqR s₁ s₂ t Heq (t₁ , lt₁ , p₁) (t₂ , lt₂ , p₂) with compareLeq t₁ t₂
... | leq  p  = trans2 (p₁ t t₂ (inl lt₁ , inr refl) (p , inl lt₂)) (Heq t₂ lt₂) (p₂ t₂ t (inr refl , inl lt₂) (inl lt₂ , inr refl))
... | more p  = trans2 (p₁ t t₁ (inl lt₁ , inr refl) (inr refl , inl lt₁)) (Heq t₁ lt₁) (p₂ t₁ t (inl p , inl lt₁) (inl lt₂ , inr refl))

------------------------------------------------------------------------------

lem-EqH⇒U⇒EqR : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (H (EqRep {as} s₁ s₂) ⇒ Unchanging {as} s₁ ⇒ Unchanging {as} s₂ ⇒ EqRep {as} s₁ s₂)
lem-EqH⇒U⇒EqR {C _} stab s₁ s₂ t Heq un₁ un₂ = lemC-EqH⇒U⇒EqR s₁ s₂ t Heq un₁ un₂
lem-EqH⇒U⇒EqR {E _} stab s₁ s₂ t Heq un₁ un₂ = lemE-EqH⇒U⇒EqR stab s₁ s₂ t Heq un₁ un₂
lem-EqH⇒U⇒EqR {S _} stab s₁ s₂ t Heq un₁ un₂ = lemS-EqH⇒U⇒EqR stab s₁ s₂ t Heq un₁ un₂
lem-EqH⇒U⇒EqR {as , bs} stab (sa₁ , sb₁) (sa₂ , sb₂) t Heq (una₁ , unb₁) (una₂ , unb₂) with ×-inj3' Heq
... | Heqa , Heqb = ×-cong (lem-EqH⇒U⇒EqR {as} stab sa₁ sa₂ t Heqa una₁ una₂) (lem-EqH⇒U⇒EqR {bs} stab sb₁ sb₂ t Heqb unb₁ unb₂)

lem-EqH⇒U⇒EqHʳ : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (H (EqRep {as} s₁ s₂) ⇒ Unchanging {as} s₁ ⇒ Unchanging {as} s₂ ⇒ Hʳ (EqRep {as} s₁ s₂))
lem-EqH⇒U⇒EqHʳ {as} stab s₁ s₂ t Heq un₁ un₂ = lem-EqH⇒U⇒EqR {as} stab s₁ s₂ t Heq un₁ un₂ , Heq

lem-C→EqRep : {as : SVDesc} → {t₁ t₂ : Time} → (s : SigVec as) → t₁ ≤ t₂ → ChangelessSV {as} s t₁ → rep {as} s t₁ ≡ rep {as} s t₂
lem-C→EqRep {as} {t₁} {t₂} s (inl p) ch = lem-UB-rep {as} (inl p) s (ch t₂ p)
lem-C→EqRep s (inr refl) ch = refl

lem-EqHʳ⇒C⇒Eq : {as : SVDesc} → (s₁ s₂ : SigVec as) → Always (Hʳ (EqRep {as} s₁ s₂) ⇒ ChangelessSV {as} s₁ ⇒ ChangelessSV {as} s₂ ⇒ (λ _ → Always (EqRep {as} s₁ s₂)))
lem-EqHʳ⇒C⇒Eq s₁ s₂ t (eq , Heq) ch₁ ch₂ t' with compare t' t
lem-EqHʳ⇒C⇒Eq s₁ s₂ t (eq , Heq) ch₁ ch₂ .t | refl   = eq
lem-EqHʳ⇒C⇒Eq s₁ s₂ t (eq , Heq) ch₁ ch₂ t' | less p = Heq t' p
lem-EqHʳ⇒C⇒Eq {as} s₁ s₂ t (eq , Heq) ch₁ ch₂ t' | more p = trans2 (sym (lem-C→EqRep {as} s₁ (inl p) ch₁)) eq (lem-C→EqRep {as} s₂ (inl p) ch₂)

lem-EqH⇒Cʳ⇒Eq : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (H (EqRep {as} s₁ s₂) ⇒ ChangelessSVʳ {as} s₁ ⇒ ChangelessSVʳ {as} s₂ ⇒ (λ _ → Always (EqRep {as} s₁ s₂)))
lem-EqH⇒Cʳ⇒Eq {as} stab s₁ s₂ t Heq ch₁ ch₂ = lem-EqHʳ⇒C⇒Eq {as} s₁ s₂ t (lem-EqH⇒U⇒EqHʳ {as} stab s₁ s₂ t Heq (ChangelessSVʳ⇒Unchanging {as} s₁ t ch₁) (ChangelessSVʳ⇒Unchanging {as} s₂ t ch₂)) (ChangelessSVʳ⇒ChangelessSV {as} s₁ t ch₁) (ChangelessSVʳ⇒ChangelessSV {as} s₂ t ch₂)

lem-HEq⇒Cʳ⇒GʳEq : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (H (EqAt {as} s₁ s₂) ⇒ ChangelessSVʳ {as} s₁ ⇒ ChangelessSVʳ {as} s₂ ⇒ Gʳ (EqAt {as} s₁ s₂))
lem-HEq⇒Cʳ⇒GʳEq {as} stab s₁ s₂ t Heq ch₁ ch₂ with lem-EqH⇒Cʳ⇒Eq {as} stab s₁ s₂ t (lem-HEqAt⇒EqH {as} stab s₁ s₂ t Heq) ch₁ ch₂
... | eqA = lem-rep-at {as} s₁ s₂ t (eqA t) , λ t' _ → lem-rep-at {as} s₁ s₂ t' (eqA t')

lem-HʳEq⇒C⇒GEq : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (Hʳ (EqAt {as} s₁ s₂) ⇒ ChangelessSV {as} s₁ ⇒ ChangelessSV {as} s₂ ⇒ G (EqAt {as} s₁ s₂))
lem-HʳEq⇒C⇒GEq {as} stab s₁ s₂ t Heq ch₁ ch₂ t' _ with lem-EqHʳ⇒C⇒Eq {as} s₁ s₂ t (lem-HʳEqAt⇒EqHʳ {as} stab s₁ s₂ t Heq) ch₁ ch₂
... | eqA = lem-rep-at {as} s₁ s₂ t' (eqA t')

------------------------------------------------------------------------------

------------------------------------------------------------------

lemS-HEqSample⇒U⇒EqSample : {A : Set} → Stability → (s₁ s₂ : SigVec (S A)) → Always (H (EqSample {S A} s₁ s₂) ⇒ Unchanging {S A} s₁ ⇒ Unchanging {S A} s₂ ⇒ EqSample {S A} s₁ s₂)
lemS-HEqSample⇒U⇒EqSample stab (a₁ , cp₁) (a₂ , cp₂) O Heq () ()
lemS-HEqSample⇒U⇒EqSample stab (a₁ , cp₁) (a₂ , cp₂) (t >0) Heq un₁ un₂ = lemCP-Hval⇒val cp₁ cp₂ (stab cp₁) (stab cp₂) t un₁ un₂ Heq

lemE-U⇒EqR : {A : Set} → (s₁ s₂ : SigVec (E A)) → Always (Unchanging {E A} s₁ ⇒ Unchanging {E A} s₂ ⇒ EqSample {E A} s₁ s₂)
lemE-U⇒EqR (nothing , _) (nothing , _) O      un₁ un₂ = refl
lemE-U⇒EqR _             (just y' , _) O      un₁ un₂ = magic un₂
lemE-U⇒EqR (just _ , _)  _             O      un₁ un₂ = magic un₁
lemE-U⇒EqR (_ , fdas₁)   (_ , fdas₂)   (t >0) un₁ un₂ = lem-isNothingEq un₁ un₂

lemC-HEqSample⇒U⇒EqSample : {A : Set} → (s₁ s₂ : SigVec (C A)) → Always (H (EqSample {C A} s₁ s₂) ⇒ Unchanging {C A} s₁ ⇒ Unchanging {C A} s₂ ⇒ EqSample {C A} s₁ s₂)
lemC-HEqSample⇒U⇒EqSample s₁ s₂ t Heq (t₁ , lt₁ , p₁) (t₂ , lt₂ , p₂) with compareℜ₀ t₁ t₂ | Heq t₁ lt₁ | Heq t₂ lt₂
lemC-HEqSample⇒U⇒EqSample s₁ s₂ t Heq (t₁ , lt₁ , p₁) (._ , lt₂ , p₂) | refl   | eq₁₂ | _     = trans2 (sym (p₁ t₁ t (inr refl , inl lt₂) (inl lt₂ , inr refl))) eq₁₂ (p₂ t₁ t (inr refl , inl lt₂) (inl lt₂ , inr refl))
lemC-HEqSample⇒U⇒EqSample s₁ s₂ t Heq (t₁ , lt₁ , p₁) (t₂ , lt₂ , p₂) | less q | _    | eq₂₁  = trans2 (sym (p₁ t₂ t (inl q , inl lt₂) (inl lt₁ , inr refl))) eq₂₁ (p₂ t₂ t (inr refl , inl lt₂) (inl lt₂ , inr refl))
lemC-HEqSample⇒U⇒EqSample s₁ s₂ t Heq (t₁ , lt₁ , p₁) (t₂ , lt₂ , p₂) | more q | eq₁₂ | _     = trans2 (sym (p₁ t₁ t (inr refl , inl lt₁) (inl lt₁ , inr refl))) eq₁₂ (p₂ t₁ t (inl q , inl lt₁) (inl lt₂ , inr refl))

lem-HEqSample⇒U⇒EqSample : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (H (EqSample {as} s₁ s₂) ⇒ Unchanging {as} s₁ ⇒ Unchanging {as} s₂ ⇒ EqSample {as} s₁ s₂)
lem-HEqSample⇒U⇒EqSample {C _} stab s₁ s₂ t Heq un₁ un₂ = lemC-HEqSample⇒U⇒EqSample s₁ s₂ t Heq un₁ un₂
lem-HEqSample⇒U⇒EqSample {S _} stab s₁ s₂ t Heq un₁ un₂ = lemS-HEqSample⇒U⇒EqSample stab s₁ s₂ t Heq un₁ un₂
lem-HEqSample⇒U⇒EqSample {E _} stab s₁ s₂ t Heq un₁ un₂ = lemE-U⇒EqR s₁ s₂ t un₁ un₂
lem-HEqSample⇒U⇒EqSample {as , bs} stab (sa₁ , sb₁) (sa₂ , sb₂) t Heq (una₁ , unb₁) (una₂ , unb₂) with ×-split2' (result2' ×-inj Heq)
... | Heqa , Heqb = ×-cong (lem-HEqSample⇒U⇒EqSample {as} stab sa₁ sa₂ t Heqa una₁ una₂) (lem-HEqSample⇒U⇒EqSample {bs} stab sb₁ sb₂ t Heqb unb₁ unb₂)

lem-HEqSample⇒Cʳ⇒EqSample : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (H (EqSample {as} s₁ s₂) ⇒ ChangelessSVʳ {as} s₁ ⇒ ChangelessSVʳ {as} s₂ ⇒ EqSample {as} s₁ s₂)
lem-HEqSample⇒Cʳ⇒EqSample {as} stab s₁ s₂ t Heq (un₁ , Gun₁) (un₂ , Gun₂) = lem-HEqSample⇒U⇒EqSample {as} stab s₁ s₂ t Heq un₁ un₂

------------------------------------------------------------------------------

lemC-HʳEqSample⇒ChangelessSV⇒GEqSample : {A : Set} → (s₁ s₂ : SigVec (C A)) → Always (EqSample {C A} s₁ s₂ ⇒ ChangelessSV {C A} s₁ ⇒ ChangelessSV {C A} s₂ ⇒ G (EqSample {C A} s₁ s₂))
lemC-HʳEqSample⇒ChangelessSV⇒GEqSample s₁ s₂ t₁ eq ch₁ ch₂ t₂ lt = trans2 (ch₁ t₂ lt t₂ t₁ (inl lt , inr refl) (inr refl , inl lt)) eq (ch₂ t₂ lt t₁ t₂ (inr refl , inl lt) (inl lt , inr refl))

lemE-HʳEqSample⇒ChangelessSV⇒GEqSample : {A : Set} → Stability → (s₁ s₂ : SigVec (E A)) → Always (EqSample {E A} s₁ s₂ ⇒ ChangelessSV {E A} s₁ ⇒ ChangelessSV {E A} s₂ ⇒ G (EqSample {E A} s₁ s₂))
lemE-HʳEqSample⇒ChangelessSV⇒GEqSample stab (ma₁ , cp₁) (ma₂ , cp₂) t₁ eq ch₁ ch₂ O ()
lemE-HʳEqSample⇒ChangelessSV⇒GEqSample stab (ma₁ , cp₁) (ma₂ , cp₂) O eq ch₁ ch₂ (t₂ >0) lt with lemCP-O-empty cp₁ (stab cp₁) | lemCP-O-empty cp₂ (stab cp₂)
... | eq₁ | eq₂ with trans (sym (ch₁ (t₂ >0) lt lt)) eq₁ | trans (sym (ch₂ (t₂ >0) lt lt)) eq₂
...     | eqe₁ | eqe₂ rewrite eqe₁ | eqe₂ = refl
lemE-HʳEqSample⇒ChangelessSV⇒GEqSample stab (ma₁ , cp₁) (ma₂ , cp₂) (t₁ >0) eq ch₁ ch₂ (t₂ >0) lt with ch₁ (t₂ >0) lt lt | ch₂ (t₂ >0) lt lt
... | eq₁ | eq₂ rewrite sym eq₁ | sym eq₂ = lemCP-lookupBeyondEq cp₁ cp₂ (stab cp₁) (stab cp₂) lt

lemS-HʳEqSample⇒ChangelessSV⇒GEqSample : {A : Set} → (s₁ s₂ : SigVec (S A)) → Always (EqSample {S A} s₁ s₂ ⇒ ChangelessSV {S A} s₁ ⇒ ChangelessSV {S A} s₂ ⇒ G (EqSample {S A} s₁ s₂))
lemS-HʳEqSample⇒ChangelessSV⇒GEqSample (a₁ , cp₁) (a₂ , cp₂) t₁ eq ch₁ ch₂ t₂ lt with ch₁ t₂ lt lt | ch₂ t₂ lt lt
... | eq₁ | eq₂ rewrite sym eq₁ | sym eq₂ with reverse (cp₁ t₁) | reverse (cp₂ t₁)
... | [] | [] = eq
... | (_ , a') ∷ _ | [] = eq
... | [] | (_ , a') ∷ _ = eq
... | (_ , a₁') ∷ _ | (_ , a₂') ∷ _ = eq

lem-EqSample⇒ChangelessSV⇒GEqSample : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (EqSample {as} s₁ s₂ ⇒ ChangelessSV {as} s₁ ⇒ ChangelessSV {as} s₂ ⇒ G (EqSample {as} s₁ s₂))
lem-EqSample⇒ChangelessSV⇒GEqSample {C _} stab s₁ s₂ t₁ eq ch₁ ch₂ = lemC-HʳEqSample⇒ChangelessSV⇒GEqSample s₁ s₂ t₁ eq ch₁ ch₂
lem-EqSample⇒ChangelessSV⇒GEqSample {E _} stab s₁ s₂ t₁ eq ch₁ ch₂ = lemE-HʳEqSample⇒ChangelessSV⇒GEqSample stab s₁ s₂ t₁ eq ch₁ ch₂
lem-EqSample⇒ChangelessSV⇒GEqSample {S _} stab s₁ s₂ t₁ eq ch₁ ch₂ = lemS-HʳEqSample⇒ChangelessSV⇒GEqSample s₁ s₂ t₁ eq ch₁ ch₂
lem-EqSample⇒ChangelessSV⇒GEqSample {as , bs} stab (sa₁ , sb₁) (sa₂ , sb₂) t₁ eq ch₁ ch₂ with ×-inj eq | ×-split2' ch₁ | ×-split2' ch₂
... | eqa , eqb | cha₁ , chb₁ | cha₂ , chb₂ = ×-cong3 (lem-EqSample⇒ChangelessSV⇒GEqSample {as} stab sa₁ sa₂ t₁ eqa cha₁ cha₂) (lem-EqSample⇒ChangelessSV⇒GEqSample {bs} stab sb₁ sb₂ t₁ eqb chb₁ chb₂)

------------------------------------------------------------------------------

lem-HEqSample⇒ChangelessSVʳ⇒GʳEqSample : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (H (EqSample {as} s₁ s₂) ⇒ ChangelessSVʳ {as} s₁ ⇒ ChangelessSVʳ {as} s₂ ⇒ Gʳ (EqSample {as} s₁ s₂))
lem-HEqSample⇒ChangelessSVʳ⇒GʳEqSample {as} stab s₁ s₂ t Heq ch₁ ch₂ with lem-HEqSample⇒Cʳ⇒EqSample {as} stab s₁ s₂ t Heq ch₁ ch₂
... | eq = eq , lem-EqSample⇒ChangelessSV⇒GEqSample {as} stab s₁ s₂ t eq (snd ch₁) (snd ch₂)

------------------------------------------------------------------------------

lem-AlwaysEqAt : {as : SVDesc} → (s₁ s₂ : SigVec as) → Always (H (EqAt {as} s₁ s₂) ⇒ (EqAt {as} s₁ s₂) ⇒ G (EqAt {as} s₁ s₂) ⇒ (λ _ → Always (EqAt {as} s₁ s₂)))
lem-AlwaysEqAt {as} s₁ s₂ = lem-AlwaysEq (at as s₁) (at as s₂)

------------------------------------------------------------------------------

lemC-AlwaysEqRep⇒C⇒C : {A : Set} → (s₁ s₂ : SigVec (C A)) → Always (EqRep {C A} s₁ s₂) → Always (ChangelessSV {C A} s₁ ⇒ ChangelessSV {C A} s₂)
lemC-AlwaysEqRep⇒C⇒C s₁ s₂ eqA t₀ ch t₃ lt t₁ t₂ (p₁ , p₂) (q₁ , q₂) = trans2 (sym (eqA t₁)) (ch t₃ lt t₁ t₂ (p₁ , p₂) (q₁ , q₂)) (eqA t₂)

lemE-AlwaysEqRep⇒C⇒C : {A : Set} (s₁ s₂ : SigVec (E A)) → Always (EqRep {E A} s₁ s₂) → Always (ChangelessSV {E A} s₁ ⇒ ChangelessSV {E A} s₂)
lemE-AlwaysEqRep⇒C⇒C (ma₁ , cp₁) (ma₂ , cp₂) eq t₀ ch t₂ lt₁ with ×-inj2' eq
... | _ , eqcp = λ _ → trans2 (sym (eqcp t₀)) (ch t₂ lt₁ lt₁) (eqcp t₂)

lemS-AlwaysEqRep⇒C⇒C : {A : Set} (s₁ s₂ : SigVec (S A)) → Always (EqRep {S A} s₁ s₂) → Always (ChangelessSV {S A} s₁ ⇒ ChangelessSV {S A} s₂)
lemS-AlwaysEqRep⇒C⇒C (a₁ , cp₁) (a₂ , cp₂) eq t₀ ch t₂ lt₁ with ×-inj2' eq
... | _ , eqcp = λ _ → trans2 (sym (eqcp t₀)) (ch t₂ lt₁ lt₁) (eqcp t₂)

lem-AlwaysEqRep⇒C⇒C : {as : SVDesc} → (s₁ s₂ : SigVec as) → Always (EqRep {as} s₁ s₂) → Always (ChangelessSV {as} s₁ ⇒ ChangelessSV {as} s₂)
lem-AlwaysEqRep⇒C⇒C {C _} s₁ s₂ eq t un = lemC-AlwaysEqRep⇒C⇒C s₁ s₂ eq t un
lem-AlwaysEqRep⇒C⇒C {E _} s₁ s₂ eq t un = lemE-AlwaysEqRep⇒C⇒C s₁ s₂ eq t un
lem-AlwaysEqRep⇒C⇒C {S _} s₁ s₂ eq t un = lemS-AlwaysEqRep⇒C⇒C s₁ s₂ eq t un
lem-AlwaysEqRep⇒C⇒C {as , bs} (sa₁ , sb₁) (sa₂ , sb₂) eq t un with ×-split2' un | ×-inj2' eq
... | una , unb | eqa , eqb = lem-AlwaysEqRep⇒C⇒C {as} sa₁ sa₂ eqa t una &₂ lem-AlwaysEqRep⇒C⇒C {bs} sb₁ sb₂ eqb t unb

lem-AlwaysEqAt⇒C⇒C : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → Always (EqAt {as} s₁ s₂) → Always (ChangelessSV {as} s₁ ⇒ ChangelessSV {as} s₂)
lem-AlwaysEqAt⇒C⇒C {as} stab s₁ s₂ eqAt = lem-AlwaysEqRep⇒C⇒C {as} s₁ s₂ (lem-AlwaysEqAt→AlwaysEqRep {as} stab s₁ s₂ eqAt)

------------------------------------------------------------------------------

lem-Eq0-ChangelessSVʳ : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → EqAt {as} s₁ s₂ O → ChangelessSVʳ {as} s₁ O → ChangelessSVʳ {as} s₂ O → Always (EqAt {as} s₁ s₂)
lem-Eq0-ChangelessSVʳ {as} stab s₁ s₂ eq ch₁ ch₂ t with lem-HEq⇒Cʳ⇒GʳEq {as} stab s₁ s₂ O (λ _ ()) ch₁ ch₂
lem-Eq0-ChangelessSVʳ stab s₁ s₂ eq ch₁ ch₂ O | eqAt , Geq = eq
lem-Eq0-ChangelessSVʳ stab s₁ s₂ eq ch₁ ch₂ (t >0) | eqAt , Geq = Geq (t >0) _

lemC-ChangelessSVʳ-Eq0 : {A : Set} → (s₁ s₂ : SigVec (C A)) → (Unchanging {C A} s₁ ⇒ Unchanging {C A} s₂ ⇒ EqAt {C A} s₁ s₂) O
lemC-ChangelessSVʳ-Eq0 s₁ s₂ (t₀ , () , _) (t₁ , () , _)

lemE-ChangelessSVʳ-Eq0 : {A : Set} → (s₁ s₂ : SigVec (E A)) → (Unchanging {E A} s₁ ⇒ Unchanging {E A} s₂ ⇒ EqAt {E A} s₁ s₂) O
lemE-ChangelessSVʳ-Eq0 (ma₁ , cp₁) (ma₂ , cp₂) un₁ un₂ = lem-isNothingEq un₁ un₂

lemS-ChangelessSVʳ-Eq0 : {A : Set} → (s₁ s₂ : SigVec (S A)) → (Unchanging {S A} s₁ ⇒ Unchanging {S A} s₂ ⇒ EqAt {S A} s₁ s₂) O
lemS-ChangelessSVʳ-Eq0 (a₁ , cp₁) (a₂ , cp₂) () ()

lem-Unchanging0-Eq0 : {as : SVDesc} → (s₁ s₂ : SigVec as) → (Unchanging {as} s₁ ⇒ Unchanging {as} s₂ ⇒ EqAt {as} s₁ s₂) O
lem-Unchanging0-Eq0 {C _} s₁ s₂ un₁ un₂ = lemC-ChangelessSVʳ-Eq0 s₁ s₂ un₁ un₂
lem-Unchanging0-Eq0 {E _} s₁ s₂ un₁ un₂ = lemE-ChangelessSVʳ-Eq0 s₁ s₂ un₁ un₂
lem-Unchanging0-Eq0 {S _} s₁ s₂ un₁ un₂ = lemS-ChangelessSVʳ-Eq0 s₁ s₂ un₁ un₂
lem-Unchanging0-Eq0 {as , bs} (sa₁ , sb₁) (sa₂ , sb₂) (una₁ , unb₁) (una₂ , unb₂) = ×-cong (lem-Unchanging0-Eq0 {as} sa₁ sa₂ una₁ una₂)
                                                                                           (lem-Unchanging0-Eq0 {bs} sb₁ sb₂ unb₁ unb₂)

lem-ChangelessSVʳ-Eq0 : {as : SVDesc} → (s₁ s₂ : SigVec as) → (ChangelessSVʳ {as} s₁ ⇒ ChangelessSVʳ {as} s₂ ⇒ EqAt {as} s₁ s₂) O
lem-ChangelessSVʳ-Eq0 {as} s₁ s₂ (ch₁ , _) (ch₂ , _) = lem-Unchanging0-Eq0 {as} s₁ s₂ ch₁ ch₂

lem-ChangelessSVʳ-EqAlways : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → ChangelessSVʳ {as} s₁ O → ChangelessSVʳ {as} s₂ O → Always (EqAt {as} s₁ s₂)
lem-ChangelessSVʳ-EqAlways {as} stab s₁ s₂ ch₁ ch₂ = lem-Eq0-ChangelessSVʳ {as} stab s₁ s₂ (lem-ChangelessSVʳ-Eq0 {as} s₁ s₂ ch₁ ch₂) ch₁ ch₂

------------------------------------------------------------------------------

lem-ChangelessSVʳ-AlwaysEqRep : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → ChangelessSVʳ {as} s₁ O → ChangelessSVʳ {as} s₂ O → Always (EqRep {as} s₁ s₂)
lem-ChangelessSVʳ-AlwaysEqRep {as} stab s₁ s₂ ch₁ ch₂ = lem-EqH⇒Cʳ⇒Eq {as} stab s₁ s₂ O (λ _ ()) ch₁ ch₂


lemE-ChangelessSVʳ-rep-nothing : {A : Set} → Stability → (s : SigVec (E A)) → (t : Time) → ChangelessSVʳ {E A} s O → rep {E A} s t ≡ (nothing , [])
lemE-ChangelessSVʳ-rep-nothing stab (ma , cp) O      (isn , cp0) = ×-cong (lem-isNothing isn) (lemCP-O-empty cp (stab cp))
lemE-ChangelessSVʳ-rep-nothing stab (ma , cp) (t >0) (isn , cp0) = ×-cong (lem-isNothing isn) (trans (sym (cp0 (t >0) _ _)) (lemCP-O-empty cp (stab cp)))

lem-ChangelessSVʳ-EqRep : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → (t₁ t₂ : Time) → ChangelessSVʳ {as} s₁ O → ChangelessSVʳ {as} s₂ O → rep {as} s₁ t₁ ≡ rep {as} s₂ t₂
lem-ChangelessSVʳ-EqRep {C _} stab s₁ s₂ t₁ t₂ ((_ , () , _ ) , _) _
lem-ChangelessSVʳ-EqRep {E _} stab s₁ s₂ t₁ t₂ ch₁ ch₂ = trans (lemE-ChangelessSVʳ-rep-nothing stab s₁ t₁ ch₁) (sym (lemE-ChangelessSVʳ-rep-nothing stab s₂ t₂ ch₂))
lem-ChangelessSVʳ-EqRep {S _} stab s₁ s₂ t₁ t₂ (() , _) _
lem-ChangelessSVʳ-EqRep {as , bs} stab (sa₁ , sb₁) (sa₂ , sb₂) t₁ t₂ ((una₁ , unb₁) , unch₁) ((una₂ , unb₂) , unch₂) with ×-split2' unch₁ | ×-split2' unch₂
... | uncha₁ , unchb₁ | uncha₂ , unchb₂ = ×-cong (lem-ChangelessSVʳ-EqRep {as} stab sa₁ sa₂ t₁ t₂ (una₁ , uncha₁) (una₂ , uncha₂))
                                                 (lem-ChangelessSVʳ-EqRep {bs} stab sb₁ sb₂ t₁ t₂ (unb₁ , unchb₁) (unb₂ , unchb₂))

lemE-ChangelessSVʳ-sample-nothing : {A : Set} → Stability → (s : SigVec (E A)) → (t : Time) → ChangelessSVʳ {E A} s O → sample {E A} s t ≡ nothing
lemE-ChangelessSVʳ-sample-nothing stab (ma , cp) O (isn , cp0) = lem-isNothing isn
lemE-ChangelessSVʳ-sample-nothing stab (ma , cp) (t >0) (isn , cp0) with lemCP-O-empty cp (stab cp) | cp0 (t >0) _ _
... | eq1 | eq2 rewrite (trans (sym eq2) eq1) = refl

lem-ChangelessSVʳ-EqSample : {as : SVDesc} → Stability → (s₁ s₂ : SigVec as) → (t₁ t₂ : Time) → ChangelessSVʳ {as} s₁ O → ChangelessSVʳ {as} s₂ O → sample {as} s₁ t₁ ≡ sample {as} s₂ t₂
lem-ChangelessSVʳ-EqSample {C _} stab s₁ s₂ t₁ t₂ ((_ , () , _ ) , _) _
lem-ChangelessSVʳ-EqSample {E _} stab s₁ s₂ t₁ t₂ ch₁ ch₂ = trans (lemE-ChangelessSVʳ-sample-nothing stab s₁ t₁ ch₁) (sym (lemE-ChangelessSVʳ-sample-nothing stab s₂ t₂ ch₂))
lem-ChangelessSVʳ-EqSample {S _} stab s₁ s₂ t₁ t₂ (() , _) _
lem-ChangelessSVʳ-EqSample {as , bs} stab (sa₁ , sb₁) (sa₂ , sb₂) t₁ t₂ ((una₁ , unb₁) , unch₁) ((una₂ , unb₂) , unch₂) with ×-split2' unch₁ | ×-split2' unch₂
... | uncha₁ , unchb₁ | uncha₂ , unchb₂ = ×-cong (lem-ChangelessSVʳ-EqSample {as} stab sa₁ sa₂ t₁ t₂ (una₁ , uncha₁) (una₂ , uncha₂))
                                                 (lem-ChangelessSVʳ-EqSample {bs} stab sb₁ sb₂ t₁ t₂ (unb₁ , unchb₁) (unb₂ , unchb₂))

------------------------------------------------------------------------------

lemC-Heq-splice : {A : Set} → (s₁ s₂ s : SigVec (C A)) → (t t' : Time) → H (EqRep {C A} s₁ s₂) t → spliceC s₁ s t t' ≡ spliceC s₂ s t t'
lemC-Heq-splice s₁ s₂ s t t' Heq with compareGeq t' t
... | less p = Heq t' p
... | geq  p = refl

-- postulate lemCP-occ-takeExcl'  : {A : Set} → {ma₁ ma₂ : Maybe A} → (cp₁ cp₂ : ChangePrefix A) → (t : Time⁺) → H (occ (ma₁ , cp₁) ≐ occ (ma₂ , cp₂)) (t >0) → ma₁ ≡ ma₂ × takeExcl⁺ t (cp₁ (t >0)) ≡ takeExcl⁺ t (cp₂ (t >0))

-- lemCP-Heq-splice : {A : Set} → Extensionality → (cp₁ cp₂ cp : ChangePrefix A) → (t : Time⁺) → takeExcl⁺ t δas ++ (ℜ⁺₀⁺-minus t (sumℜ⁺ (map fst (takeExcl⁺ t (cp₁ (t >0)))))
--        (lemTDL-sumTakeExcl⁺ t (cp₁ (t >0)))
--        , ma)
--       ∷ cp (ℜ₀⁺₀-minus t' t ≤-trustme)

postulate lemE-Heq-splice : {A : Set} → Extensionality → (s₁ s₂ s : SigVec (E A)) → (t : Time) → H (EqRep {E A} s₁ s₂) t → spliceE s₁ s t ≡ spliceE s₂ s t
-- lemE-Heq-splice ext (ma₁ , cp₁) (ma₂ , cp₂) (ma , cp) t Heq = ×-cong (fst (×-inj (Heq O _))) (ext (λ t' → {!!}))

-- -- lemE-Heq-splice⁺ ext (ma₁ , cp₁) (ma₂ , cp₂) (ma , cp) t Heq with lemCP-occ-takeExcl' cp₁ cp₂ t Heq
-- -- lemE-Heq-splice⁺ ext (.ma₂ , cp₁) (ma₂ , cp₂) (ma , cp) t Heq | refl , eqcp = ×-congL (ext (λ s → cong2 _++_ eqcp (cong2R (delayTDLinit ma) (trans (ℜ⁺₀⁺-minus-proof-irrelevence {!!} {!!} {!!} {!!}) {!cong2 (ℜ⁺₀⁺-minus t) ? ?!}))))

-- --- with takeExcl⁺ t (cp₁ (t >0)) | takeExcl⁺ t (cp₂ (t >0))
-- --- lemE-Heq-splice⁺ (.ma₂ , cp₁) (ma₂ , cp₂) (ma , cp) t Heq | refl , eqcp | δas₁ | δas₂ = {!!}


postulate lemS-Heq-splice : {A : Set} → Extensionality → (s₁ s₂ s : SigVec (S A)) → (t : Time) → H (EqRep {S A} s₁ s₂) t → spliceS s₁ s t ≡ spliceS s₂ s t
-- lemS-Heq-splice ext (a₁ , cp₁) (a₂ , cp₂) (a , cp) O Heq = refl
-- lemS-Heq-splice ext (a₁ , cp₁) (a₂ , cp₂) (a , cp) (t >0) Heq with ×-inj3' Heq
-- ... | Heqa , Heqcp = ×-cong (Heqa O _) (ext (λ t' → {!!}))

lem-Heq-splice : {as : SVDesc} → Extensionality → (s₁ s₂ s : SigVec as) → (t : Time) → H (EqRep {as} s₁ s₂) t → splice {as} s₁ s t ≡ splice {as} s₂ s t
lem-Heq-splice {C _} ext s₁ s₂ s t Heq = ext (λ t' → lemC-Heq-splice s₁ s₂ s t t' Heq)
lem-Heq-splice {E _} ext s₁ s₂ s t Heq = lemE-Heq-splice ext s₁ s₂ s t Heq
lem-Heq-splice {S _} ext s₁ s₂ s t Heq = lemS-Heq-splice ext s₁ s₂ s t Heq
lem-Heq-splice {as , bs} ext (sa₁ , sb₁) (sa₂ , sb₂) (sa , sb) t Heq with ×-inj3' Heq
... | Heqa , Heqb = ×-cong (lem-Heq-splice {as} ext sa₁ sa₂ sa t Heqa) (lem-Heq-splice {bs} ext sb₁ sb₂ sb t Heqb)

-- lem-HEqRep-splice : {as : SVDesc} → Extensionality → (s₁ s₂ s : SigVec as) →  Always (H (EqRep {as} s₁ s₂) ⇒ splice {as} s₁ s ≐ splice {as} s₂ s)
-- lem-HEqRep-splice {as} ext s₁ s₂ s O      Heq = refl
-- lem-HEqRep-splice {as} ext s₁ s₂ s (t >0) Heq = lem-Heq-splice⁺ {as} ext s₁ s₂ s t Heq

------------------------------------------------------------------------------

postulate lemE-sample-advance-plus : {A : Set} → (d t : Time) → (s : SigVec (E A)) → sample {E A} (advance {E A} d s) t ≡ sample {E A} s (t ₀+₀ d)
-- lemE-sample-advance-plus d      O      (ma , cp) = refl
-- lemE-sample-advance-plus O      (t >0) (ma , cp) = {!!}
-- lemE-sample-advance-plus (d >0) (t >0) (ma , cp) = {!!}

postulate lemS-sample-advance-plus : {A : Set} → Stability → (d t : Time) → (s : SigVec (S A)) → sample {S A} (advance {S A} d s) t ≡ sample {S A} s (t ₀+₀ d)
-- lemS-sample-advance-plus stab O      O      (a , cp) rewrite lemCP-O-empty cp (stab cp) = refl
-- lemS-sample-advance-plus stab (d >0) O      (a , cp) = {!!}
-- lemS-sample-advance-plus stab O      (t >0) (a , cp) rewrite lemCP-O-empty cp (stab cp) = {!!}
-- lemS-sample-advance-plus stab (d >0) (t >0) (a , cp) = {!!}

lem-sample-advance-plus : {as : SVDesc} → Stability → (d t : Time) → (s : SigVec as) → sample {as} (advance {as} d s) t ≡ sample {as} s (t ₀+₀ d)
lem-sample-advance-plus {C _} stab d t s = refl
lem-sample-advance-plus {E _} stab d t s = lemE-sample-advance-plus d t s
lem-sample-advance-plus {S _} stab d t s = lemS-sample-advance-plus stab d t s
lem-sample-advance-plus {as , bs} stab d t (sa , sb) = ×-cong (lem-sample-advance-plus {as} stab d t sa) (lem-sample-advance-plus {bs} stab d t sb)

lemC-sample-splice-eq : {A : Set} → (t₁ t₂ t : Time) → (s₁ s₂ s : SigVec (C A)) → sample {C A} s₁ t₁ ≡ sample {C A} s₂ t₂ → sample {C A} (splice {C A} s₁ s t₁) (t ₀+₀ t₁) ≡ sample {C A} (splice {C A} s₂ s t₂) (t ₀+₀ t₂)
lemC-sample-splice-eq t₁ t₂ t s₁ s₂ s eq with compareGeq (t ₀+₀ t₁) t₁ | compareGeq (t ₀+₀ t₂) t₂
... | less p | _      = absurd (lem-ℜ₀-+-x+y≮y t p)
... | _      | less q = absurd (lem-ℜ₀-+-x+y≮y t q)
... | geq p  | geq q  = cong s (trans (lem-ℜ₀-[x+y]-y=x {t} p) (sym (lem-ℜ₀-[x+y]-y=x q)))

postulate lemE-sample-splice-eq : {A : Set} → Stability → (t₁ t₂ t : Time) → (s₁ s₂ s : SigVec (E A)) → sample {E A} s₁ t₁ ≡ sample {E A} s₂ t₂ → sample {E A} (splice {E A} s₁ s t₁) (t ₀+₀ t₁) ≡ sample {E A} (splice {E A} s₂ s t₂) (t ₀+₀ t₂)
-- lemE-sample-splice-eq stab O O t (ma₁ , cp₁) (ma₂ , cp₂) (ma , cp) eq = refl
-- lemE-sample-splice-eq stab O (t₂ >0) t (ma₁ , cp₁) (ma₂ , cp₂) (ma , cp) eq = {!!}
-- lemE-sample-splice-eq stab (t₁ >0) O t (ma₁ , cp₁) (ma₂ , cp₂) (ma , cp) eq = {!!}
-- lemE-sample-splice-eq stab (t₁ >0) (t₂ >0) t (ma₁ , cp₁) (ma₂ , cp₂) (ma , cp) eq = {!!}

postulate lemS-sample-splice-eq : {A : Set} → Stability → (t₁ t₂ t : Time) → (s₁ s₂ s : SigVec (S A)) → sample {S A} s₁ t₁ ≡ sample {S A} s₂ t₂ → sample {S A} (splice {S A} s₁ s t₁) (t ₀+₀ t₁) ≡ sample {S A} (splice {S A} s₂ s t₂) (t ₀+₀ t₂)
-- lemS-sample-splice-eq stab t₁ t₂ t s₁ s₂ s eq = {!!}

lem-sample-splice-eq : {as : SVDesc} → Stability → (t₁ t₂ t : Time) → (s₁ s₂ s : SigVec as) → sample {as} s₁ t₁ ≡ sample {as} s₂ t₂ → sample {as} (splice {as} s₁ s t₁) (t ₀+₀ t₁) ≡ sample {as} (splice {as} s₂ s t₂) (t ₀+₀ t₂)
lem-sample-splice-eq {C _} stab t₁ t₂ t s₁ s₂ s eq = lemC-sample-splice-eq t₁ t₂ t s₁ s₂ s eq
lem-sample-splice-eq {E _} stab t₁ t₂ t s₁ s₂ s eq = lemE-sample-splice-eq stab t₁ t₂ t s₁ s₂ s eq
lem-sample-splice-eq {S _} stab t₁ t₂ t s₁ s₂ s eq = lemS-sample-splice-eq stab t₁ t₂ t s₁ s₂ s eq
lem-sample-splice-eq {as , bs} stab t₁ t₂ t (sa₁ , sb₁) (sa₂ , sb₂) (sa , sb) eq with ×-inj eq
... | eqa , eqb = ×-cong (lem-sample-splice-eq {as} stab t₁ t₂ t sa₁ sa₂ sa eqa) (lem-sample-splice-eq {bs} stab t₁ t₂ t sb₁ sb₂ sb eqb)

------------------------------------------------------------------------------

lemC-rep-advance-eq : {A : Set} → (t d : Time) → (s₁ s₂ : SigVec (C A)) → EqRep {C A} s₁ s₂ (t ₀+₀ d) → EqRep {C A} (advance {C A} d s₁) (advance {C A} d s₂) t
lemC-rep-advance-eq t d s₁ s₂ eq = eq

postulate lem-rep-advance-eq : {as : SVDesc} → Stability → (t d : Time) → (s₁ s₂ : SigVec as) → EqRep {as} s₁ s₂ (t ₀+₀ d) → EqRep {as} (advance {as} d s₁) (advance {as} d s₂) t

postulate lem-EqRep-splice : {as : SVDesc} → (s₁ s₂ s : SigVec as) → (tx t : Time) → EqRep {as} s₁ s₂ t → EqRep {as} (splice {as} s s₁ tx) (splice {as} s s₂ tx) (t ₀+₀ tx)

postulate lem-HEqRep-splice : {as : SVDesc} → (s₁ s₂ s : SigVec as) → (tx t : Time) → H (EqRep {as} s₁ s₂) t → H (EqRep {as} (splice {as} s s₁ tx) (splice {as} s s₂ tx)) (t ₀+₀ tx)

------------------------------------------------------------------------------

lem-HeqRep-decrease : {as : SVDesc} → {t₁ t₀ : Time} → (s₁ s₂ : SigVec as) → (t₀ ≤ t₁) → Hʳ (EqRep {as} s₁ s₂) t₁ → Hʳ (EqRep {as} s₁ s₂) t₀
lem-HeqRep-decrease {as} {t₁} {t₀} s₁ s₂ (inl lt) Heq = lem-Hʳ⇒HHʳ {EqRep {as} s₁ s₂} t₁ Heq t₀ lt
lem-HeqRep-decrease s₁ s₂ (inr refl) Heq = Heq

lem-HeqRep-minus : {as : SVDesc} → {δ₁ δ₂ : Δt} → {t : Time} → (s₁ s₂ : SigVec as) → (δ₁ ≤ℜ⁺ δ₂) → (p : (δ₁ >0) ≤ t) → (q : (δ₂ >0) ≤ t) → Hʳ (EqRep {as} s₁ s₂) (ℜ₀⁺₀-minus t δ₁ p) → Hʳ (EqRep {as} s₁ s₂) (ℜ₀⁺₀-minus t δ₂ q)
lem-HeqRep-minus {as} s₁ s₂ lt p q Heq = lem-HeqRep-decrease {as} s₁ s₂ (lem-ℜ₀⁺₀-minus-≤-cancellative q p lt) Heq

------------------------------------------------------------------------------

lem-HeqRep-minus2 : {as : SVDesc} → {δ : Δt} → {t₀ t₁ : Time} → (s₁ s₂ : SigVec as) → (lt : t₀ <ℜ₀ t₁) → (p : (δ >0) ≤ℜ₀ t₀) → (q : (δ >0) ≤ℜ₀ t₁) → Hʳ (EqRep {as} s₁ s₂) (ℜ₀⁺₀-minus t₁ δ q) → Hʳ (EqRep {as} s₁ s₂) (ℜ₀⁺₀-minus t₀ δ p)
lem-HeqRep-minus2 {as} s₁ s₂ lt p q Heq = lem-HeqRep-decrease {as} s₁ s₂ (lem-ℜ₀⁺₀-minus-≤-cancellativeL p q (inl lt)) Heq

------------------------------------------------------------------------------
