{-# OPTIONS --type-in-type #-}

module CPLemmas where

open import NeilPrelude
open import RealTime
open import Logic
open import List hiding ([_])
open import Maybe
open import SigVecs
open import TimeDeltaList
open import TimeDeltaListProps
open import Properties
open import Utilities

------------------------------------------------------------------

Contained : {A : Set} → (cp : ChangePrefix A) → Set
Contained cp = {t : Time} → sumTDL (cp t) ≤ t

Stable→Contained : {A : Set} → (cp : ChangePrefix A) → Stable cp → Contained cp
Stable→Contained cp stab = lemTDL-takeIncl→sum (cp _) (sym (stab (inr refl)))

------------------------------------------------------------------

lemCP-O-empty : {A : Set} → (cp : ChangePrefix A) → Stable cp → cp O ≡ []
lemCP-O-empty cp stab with Stable→Contained cp stab {O}
lemCP-O-empty cp stab | inl ()
lemCP-O-empty cp stab | inr eq with cp O
lemCP-O-empty cp stab | inr eq | [] = refl
lemCP-O-empty cp stab | inr () | _ ∷ _

lemCP-O-equal : {A : Set} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂ → cp₁ O ≡ cp₂ O
lemCP-O-equal cp₁ cp₂ stab₁ stab₂ = trans (lemCP-O-empty cp₁ stab₁) (sym (lemCP-O-empty cp₂ stab₂))

------------------------------------------------------------------

lem-valO : {A : Set} → {a : A} → (cp : ChangePrefix A) → Stable cp → val (a , cp) O ≡ a
lem-valO cp stab with lemCP-O-empty cp stab
... | eq with cp O
lem-valO cp stab | _  | [] = refl
lem-valO cp stab | () | _ ∷ _

lem-valO-eq : {A : Set} → {a₁ a₂ : A} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂ → val (a₁ , cp₁) O ≡ val (a₂ , cp₂) O → a₁ ≡ a₂
lem-valO-eq cp₁ cp₂ stab₁ stab₂ eq = trans2 (sym (lem-valO cp₁ stab₁)) eq (lem-valO cp₂ stab₂)

------------------------------------------------------------------

lemCP-lookupStable : {A : Set} → {t₁ t₂ : Time} → (cp : ChangePrefix A) → Stable cp → t₁ ≤ℜ₀ t₂ → lookupTDL (cp t₂) t₁ ≡ lookupTDL (cp t₁) t₁
lemCP-lookupStable               cp stab lt with stab lt
lemCP-lookupStable {_} {t₁}      cp stab lt | eq with cp t₁
lemCP-lookupStable {_} {t₁} {t₂} cp stab lt | refl | ._ = sym (lemTDL-lookupTaken t₁ t₁ ≤ℜ₀-refl (cp t₂))

lemCP-lookupAt : {A : Set} → {t₁ t₂ : Time} → (cp : ChangePrefix A) → Stable cp → t₁ ≤ℜ₀ t₂ → IsJust (lookupTDL (cp t₂) t₁) → IsJust (lookupTDL (cp t₁) t₁)
lemCP-lookupAt cp stab lt = subst (cong IsJust (lemCP-lookupStable cp stab lt))

lemCP-lookupBeyond : {A : Set} → {t₁ t₂ : Time} → (cp : ChangePrefix A) → Stable cp → t₁ <ℜ₀ t₂ → lookupTDL (cp t₁) t₂ ≡ nothing
lemCP-lookupBeyond {_} {t₁} {t₂} cp stab lt = lemTDL-lookupBeyondSum t₂ (cp t₁) (≤<ℜ₀-trans (Stable→Contained cp stab) lt)

lemCP-lookup : {A : Set} → (t₁ t₂ : Time) → (cp : ChangePrefix A) → Stable cp → IsJust (lookupTDL (cp t₂) t₁) → IsJust (lookupTDL (cp t₁) t₁)
lemCP-lookup t₁ t₂ cp stab p with compareLeqℜ₀ t₁ t₂
lemCP-lookup t₁ t₂ cp stab p | leq q  = lemCP-lookupAt cp stab q p
lemCP-lookup t₁ t₂ cp stab p | more q rewrite lemCP-lookupBeyond cp stab q = absurd p

lemCP-eq-lookup : {A : Set} → {t₀ t₂ : Time} → t₀ < t₂ → (cp : ChangePrefix A) → Stable cp → cp t₀ ≡ cp t₂ → lookupTDL (cp t₂) t₂ ≡ nothing
lemCP-eq-lookup lt cp stab eq rewrite (sym eq) = lemCP-lookupBeyond cp stab lt

------------------------------------------------------------------

lemCP-take-Beyond : {A : Set} → {t₁ t₂ : Time} → (cp : ChangePrefix A) → Stable cp → t₁ ≤ t₂ → takeIncl t₂ (cp t₁) ≡ cp t₁
lemCP-take-Beyond {A} {t₁} {t₂} cp stab lt = lemTDL-sum→takeIncl t₂ (cp t₁) ( ≤ℜ₀-trans (Stable→Contained cp stab) lt)

lemCP-stable : {A : Set} → {t₁ t₂ t₃ : Time} → (cp : ChangePrefix A) → Stable cp → t₂ ∈ [ t₁ , t₃ ] → cp t₁ ≡ cp t₃ → cp t₁ ≡ cp t₂
lemCP-stable {_} {t₁} {t₂} cp stab (p , q) eq with lemCP-take-Beyond {_} {t₁} {t₁} cp stab (inr refl) | lemCP-take-Beyond {_} {t₁} {t₂} cp stab p
... | eq₁ | eq₂ = trans2 (stab (≤ℜ₀-trans p q)) (trans3 (sym (cong (takeIncl t₁) eq)) eq₁ (sym eq₂) (cong (takeIncl t₂) eq)) (sym (stab q))

lemCP-stable2 : {A : Set} → {t₀ t₁ t₂ t₃ : Time} → (cp : ChangePrefix A) → Stable cp → t₁ ∈ [ t₀ , t₃ ] → t₂ ∈ [ t₀ , t₃ ] → cp t₀ ≡ cp t₃ → cp t₁ ≡ cp t₂
lemCP-stable2 cp stab p q eq = trans (sym (lemCP-stable cp stab p eq)) (lemCP-stable cp stab q eq)

lemCP-stable-eq : {A : Set} → {t₁ t₂ : Time} → (cp : ChangePrefix A) → Stable cp → t₁ ≤ t₂ → takeIncl t₁ (cp t₁) ≡ takeIncl t₁ (cp t₂)
lemCP-stable-eq cp stab lt = trans (sym (stab (inr refl))) (stab lt)

lemCP-stable-eq-strict : {A : Set} → {t₁ t₂ : Time} → (cp : ChangePrefix A) → Stable cp → t₁ < t₂ → takeIncl t₁ (cp t₁) ≡ takeIncl t₁ (cp t₂)
lemCP-stable-eq-strict cp stab lt = lemCP-stable-eq cp stab (inl lt)

lemCP-stable-eq' : {A : Set} → {t₀ : Time} → (cp : ChangePrefix A) → Stable cp → (t₂ : Time) → t₀ < t₂ → (t₁ : Time) → (t₀ < t₁) × (t₁ ≤ t₂) → takeIncl t₀ (cp t₁) ≡ takeIncl t₀ (cp t₀)
lemCP-stable-eq' cp stab t₂ lt t₁ (p , _) = sym (lemCP-stable-eq cp stab (inl p))

------------------------------------------------------------------

lemCP-lookupTaken : {A : Set} → {t₀ t₁ : Time} → t₀ ≤ t₁ → (cp : ChangePrefix A) → lookupTDL (cp t₀) t₀ ≡ lookupTDL (takeIncl t₁ (cp t₀)) t₀
lemCP-lookupTaken {_} {t₀} {t₁} lt cp = sym (lemTDL-lookupTaken t₀ t₁ lt (cp t₀))

------------------------------------------------------------------

lemCP-eq-stable : {A : Set} → {t₀ t₁ : Time} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂ → t₀ ≤ t₁ → cp₁ t₁ ≡ cp₂ t₁ → cp₁ t₀ ≡ cp₂ t₀
lemCP-eq-stable cp₁ cp₂ stab₁ stab₂ lt eq = trans2 (stab₁ lt) (cong (takeIncl _) eq) (sym (stab₂ lt))

lemCP-lookupStableEq : {A : Set} → {t₁ t₂ : Time} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂ → t₁ ≤ t₂ → lookupTDL (cp₁ t₁) t₁ ≡ lookupTDL (cp₂ t₁) t₁ → lookupTDL (cp₁ t₂) t₁ ≡ lookupTDL (cp₂ t₂) t₁
lemCP-lookupStableEq cp₁ cp₂ stab₁ stab₂ lt eq = trans2 (lemCP-lookupStable cp₁ stab₁ lt) eq (sym (lemCP-lookupStable cp₂ stab₂ lt))

lemCP-lookupBeyondEq : {A : Set} → {t₁ t₂ : Time} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂ → t₁ < t₂ → lookupTDL (cp₁ t₁) t₂ ≡ lookupTDL (cp₂ t₁) t₂
lemCP-lookupBeyondEq cp₁ cp₂ stab₁ stab₂ lt = trans (lemCP-lookupBeyond cp₁ stab₁ lt) (sym (lemCP-lookupBeyond cp₂ stab₂ lt))

------------------------------------------------------------------

lemCP-empty-stable : {A : Set} → {t₁ t₂ : Time} → (cp : ChangePrefix A) → Stable cp → t₁ ≤ t₂ → cp t₂ ≡ [] → cp t₁ ≡ []
lemCP-empty-stable cp stab lt eq rewrite stab lt | eq = refl

lemCP-head-stable : {A : Set} → {t : Time} → {δ : Δt} → {a : A} → {δas₁ : ChangeList A} → (cp : ChangePrefix A) → Stable cp → (δ >0) ≤ t → cp t ≡ (δ , a) ∷ δas₁ → cp (δ >0) ≡ (δ , a) ∷ []
lemCP-head-stable cp stab lt eq rewrite stab lt | eq = lemTDL-takeInclHead _

------------------------------------------------------------------

lemCP-head-leq : {A : Set} → {a : A} → {δ : Δt} → (t : Time) → (cp : ChangePrefix A) → Stable cp → (δas : ChangeList A) → cp t ≡ (δ , a) ∷ δas → (δ >0) ≤ t
lemCP-head-leq {_} {a} {δ} t cp stab δas eq with Stable→Contained cp stab {t}
lemCP-head-leq {_} {a} {δ} t cp stab δas eq | con with cp t
lemCP-head-leq {_} {a} {δ} t cp stab δas refl | con | ._ = lemTDL-sum-head δ a δas con

------------------------------------------------------------------

-- FALSE!!!
-- lemCP-occ-eq : {A : Set} → {ma : Maybe A} → (cp₁ cp₂ : ChangePrefix A) → (t₁ t₂ : Time) → cp₁ t₁ ≡ cp₂ t₂ → occ (ma , cp₁) t₁ ≡ occ (ma , cp₂) t₂
-- lemCP-occ-eq cp₁ cp₂ O O eq = refl
-- lemCP-occ-eq cp₁ cp₂ O (t >0) eq = {!!}
-- lemCP-occ-eq cp₁ cp₂ (t >0) O eq = {!!}
-- lemCP-occ-eq cp₁ cp₂ (t₁ >0) (t₂ >0) eq  = {!refl!}

lemCP-occ : {A : Set} → {ma₁ ma₂ : Maybe A} → (cp₁ cp₂ : ChangePrefix A) → Always ((occ (ma₁ , cp₁) ≐ occ (ma₂ , cp₂)) ⇒ (lookupCP cp₁ ≐ lookupCP cp₂))
lemCP-occ cp₁ cp₂ O eq = refl
lemCP-occ cp₁ cp₂ (t >0) eq = eq

lemCP-Hʳocc : {A : Set} → {ma₁ ma₂ : Maybe A} → (cp₁ cp₂ : ChangePrefix A) → Always (Hʳ (occ (ma₁ , cp₁) ≐ occ (ma₂ , cp₂)) ⇒ Hʳ (lookupCP cp₁ ≐ lookupCP cp₂))
lemCP-Hʳocc cp₁ cp₂ t (eq  , Heq) = lemCP-occ cp₁ cp₂ t eq , (λ t' lt → lemCP-occ cp₁ cp₂ t' (Heq t' lt))

lemCP-change : {A : Set} → {a₁ a₂ : A} → (cp₁ cp₂ : ChangePrefix A) → Always ((change (a₁ , cp₁) ≐ change (a₂ , cp₂)) ⇒ (lookupCP cp₁ ≐ lookupCP cp₂))
lemCP-change cp₁ cp₂ O eq = refl
lemCP-change cp₁ cp₂ (t >0) eq = eq

lemCP-Hʳchange : {A : Set} → {a₁ a₂ : A} → (cp₁ cp₂ : ChangePrefix A) → Always (Hʳ (change (a₁ , cp₁) ≐ change (a₂ , cp₂)) ⇒ Hʳ (lookupCP cp₁ ≐ lookupCP cp₂))
lemCP-Hʳchange cp₁ cp₂ t (eq  , Heq) = lemCP-change cp₁ cp₂ t eq , (λ t' lt → lemCP-change cp₁ cp₂ t' (Heq t' lt))

------------------------------------------------------------------

lemCP-lookup-anywhere : {A : Set} → {t : Time} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂
                      → (Hʳ (lookupCP cp₁ ≐ lookupCP cp₂)) t → Always (lookupTDL (cp₁ t) ≐ lookupTDL (cp₂ t))
lemCP-lookup-anywhere {_} {t} cp₁ cp₂ stab₁ stab₂ (eq , Heq) t' with compare t' t
lemCP-lookup-anywhere cp₁ cp₂ stab₁ stab₂ (eq , Heq) t' | refl = eq
lemCP-lookup-anywhere cp₁ cp₂ stab₁ stab₂ (eq , Heq) t' | less p = lemCP-lookupStableEq cp₁ cp₂ stab₁ stab₂ (inl p) (Heq t' p)
lemCP-lookup-anywhere cp₁ cp₂ stab₁ stab₂ (eq , Heq) t' | more p = lemCP-lookupBeyondEq cp₁ cp₂ stab₁ stab₂ p

lemCP-Hʳlookup⇒eq : {A : Set} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂ 
                  → Always (Hʳ (lookupCP cp₁ ≐  lookupCP cp₂) ⇒ cp₁ ≐ cp₂)
lemCP-Hʳlookup⇒eq cp₁ cp₂ stab₁ stab₂ t Heq = lemTDL-lookupEq (cp₁ t) (cp₂ t) (lemCP-lookup-anywhere cp₁ cp₂ stab₁ stab₂ Heq)

lemCP-Hʳocc⇒eq : {A : Set} → {ma₁ ma₂ : Maybe A} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂ → Always (Hʳ (occ (ma₁ , cp₁) ≐ occ (ma₂ , cp₂)) ⇒ cp₁ ≐ cp₂)
lemCP-Hʳocc⇒eq cp₁ cp₂ stab₁ stab₂ O (eq , Heq) = lemCP-O-equal cp₁ cp₂ stab₁ stab₂
lemCP-Hʳocc⇒eq cp₁ cp₂ stab₁ stab₂ t Heq = lemCP-Hʳlookup⇒eq cp₁ cp₂ stab₁ stab₂ t (lemCP-Hʳocc cp₁ cp₂ t Heq)

lemCP-Hʳocc⇒eq' : {A : Set} → {ma₁ ma₂ : Maybe A} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂ → Always (Hʳ (occ (ma₁ , cp₁) ≐ occ (ma₂ , cp₂)) ⇒ const (ma₁ ≡ ma₂) ∧ (cp₁ ≐ cp₂))
lemCP-Hʳocc⇒eq' cp₁ cp₂ stab₁ stab₂ O (refl , Heq) = refl , lemCP-O-equal cp₁ cp₂ stab₁ stab₂
lemCP-Hʳocc⇒eq' cp₁ cp₂ stab₁ stab₂ (t >0) (eq , Heq) = Heq O _ , lemCP-Hʳocc⇒eq cp₁ cp₂ stab₁ stab₂ (t >0) (eq , Heq)


lemCP-Hʳchange⇒eq : {A : Set} → {a₁ a₂ : A} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂ → Always (Hʳ (change (a₁ , cp₁) ≐ change (a₂ , cp₂)) ⇒ cp₁ ≐ cp₂)
lemCP-Hʳchange⇒eq cp₁ cp₂ stab₁ stab₂ O (eq , Heq) = lemCP-O-equal cp₁ cp₂ stab₁ stab₂
lemCP-Hʳchange⇒eq cp₁ cp₂ stab₁ stab₂ t Heq = lemCP-Hʳlookup⇒eq cp₁ cp₂ stab₁ stab₂ t (lemCP-Hʳchange cp₁ cp₂ t Heq)

lemCP-Hchange⇒eq : {A : Set} → {a₁ a₂ : A} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂ → Always (H (change (a₁ , cp₁) ≐ change (a₂ , cp₂)) ⇒ H (cp₁ ≐ cp₂))
lemCP-Hchange⇒eq cp₁ cp₂ stab₁ stab₂ t₁ Heq t₀ lt = lemCP-Hʳchange⇒eq cp₁ cp₂ stab₁ stab₂ t₀ (Heq t₀ lt , reduce-range-< lt Heq)

lemCP-Hʳchange⇒eq' : {A : Set} → {a₁ a₂ : A} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂ → Always (Hʳ (change (a₁ , cp₁) ≐ change (a₂ , cp₂)) ⇒ const (a₁ ≡ a₂) ∧ (cp₁ ≐ cp₂))
lemCP-Hʳchange⇒eq' cp₁ cp₂ stab₁ stab₂ O (refl , Heq) = refl , lemCP-O-equal cp₁ cp₂ stab₁ stab₂
lemCP-Hʳchange⇒eq' cp₁ cp₂ stab₁ stab₂ (t >0) (eq , Heq) = just-inj (Heq O _) , lemCP-Hʳchange⇒eq cp₁ cp₂ stab₁ stab₂ (t >0) (eq , Heq)

------------------------------------------------------------------

lemCP-lookupBeyond' : {A : Set} → {t₁ t₂ : Time} → (cp : ChangePrefix A) → Stable cp → t₁ < t₂ → cp t₁ ≡ cp t₂ → UnchangingCP cp t₂
lemCP-lookupBeyond' cp stab lt eq = lemTDL-lookupBeyondTake _ lt (trans (sym (stab (inl lt))) eq)

------------------------------------------------------------------

lemCP-drop-empty : {A : Set} → (t₁ t₂ : Time) → t₁ ≤ℜ₀ t₂ → (cp : ChangePrefix A) → Stable cp → dropIncl t₂ (cp t₁) ≡ []
lemCP-drop-empty t₁ t₂ p cp stab = lemTDL-dropInclSum→Empty t₂ (cp t₁) (≤ℜ₀-trans (Stable→Contained cp stab) p)

lemCP-drop-now-empty : {A : Set} → (t : Time) → (cp : ChangePrefix A) → Stable cp → dropIncl t (cp t) ≡ []
lemCP-drop-now-empty t = lemCP-drop-empty t t ≤ℜ₀-refl

------------------------------------------------------------------

lemCP-sumTDLeq : {A : Set} → {t₁ t₂ : Time} → (t₁ ≤ t₂) → (cp : ChangePrefix A) → Stable cp → sumTDL (cp t₂) ≤ t₁ → cp t₁ ≡ cp t₂
lemCP-sumTDLeq {_} {t₁} {t₂} lt cp stab lts = trans (stab lt) (lemTDL-sum→takeIncl t₁ (cp t₂) lts)

lemCP-valEqValSumTDLs' : {A : Set} → (a : A) → (cp : ChangePrefix A) → Stable cp → (t t₀ : Time) → t₀ ∈ [ sumTDL (cp t) , t ] → val (a , cp) t ≡ val (a , cp) t₀
lemCP-valEqValSumTDLs' a cp stab t t₀ (p , q) rewrite lemCP-sumTDLeq q cp stab p with reverse (cp t)
... | [] = refl
... | ((_ , _) ∷ _) = refl

lemCP-valEqValSumTDLs : {A : Set} → (a : A) → (cp : ChangePrefix A) → Stable cp → (t : Time) → val (a , cp) t ≡ val (a , cp) (sumTDL (cp t))
lemCP-valEqValSumTDLs a cp stab t = lemCP-valEqValSumTDLs' a cp stab t (sumTDL (cp t)) (inr refl , Stable→Contained cp stab)

lemCP-lookupIsNothing→sumTDLless : {A : Set} → (cp : ChangePrefix A) → Stable cp → (t : Time⁺) → IsNothing (lookupTDL⁺ (cp (t >0)) t) → sumTDL (cp (t >0)) < (t >0)
lemCP-lookupIsNothing→sumTDLless cp stab t p with Stable→Contained cp stab {t >0}
... | inl lt = lt
... | inr eq = absurd (p (lemTDL-lookupAtSum t (cp (t >0)) eq))

lemCP-Hval⇒val : {A : Set} → {a₁ a₂ : A} → (cp₁ cp₂ : ChangePrefix A) → Stable cp₁ → Stable cp₂ → (t : Time⁺) → IsNothing (lookupTDL⁺ (cp₁ (t >0)) t) → IsNothing (lookupTDL⁺ (cp₂ (t >0)) t) → (H (val (a₁ , cp₁) ≐ val (a₂ , cp₂)) ⇒ (val (a₁ , cp₁) ≐ val (a₂ , cp₂))) (t >0)
lemCP-Hval⇒val {_} {a₁} {a₂} cp₁ cp₂ stab₁ stab₂ t p q r with lemCP-lookupIsNothing→sumTDLless cp₁ stab₁ t p
                                                            | lemCP-lookupIsNothing→sumTDLless cp₂ stab₂ t q
                                                            | lemCP-valEqValSumTDLs' a₁ cp₁ stab₁ (t >0)
                                                            | lemCP-valEqValSumTDLs' a₂ cp₂ stab₂ (t >0)
... | leq₁ | leq₂ | eq₁ | eq₂ with compareGeq (sumTDL (cp₁ (t >0))) (sumTDL (cp₂ (t >0)))
...   | geq  gt = trans2 (eq₁ (sumTDL (cp₁ (t >0))) (inr refl , inl leq₁)) (r (sumTDL (cp₁ (t >0))) leq₁) (sym (eq₂ (sumTDL (cp₁ (t >0))) (gt , inl leq₁)))
...   | less ls = trans2 (eq₁ (sumTDL (cp₂ (t >0))) (inl ls , inl leq₂))   (r (sumTDL (cp₂ (t >0))) leq₂) (sym (eq₂ (sumTDL (cp₂ (t >0))) (inr refl , inl leq₂)))

------------------------------------------------------------------