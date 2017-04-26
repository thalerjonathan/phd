{-# OPTIONS --type-in-type  #-}

open import NeilPrelude
open import List
open import ListProps
open import RealTime
open import Maybe
open import StrictTotalOrder
open import Logic
open import TimeDeltaList

module TimeDeltaListProps where

----------------------------------------------------------------------------------------------------------

lemTDL-delay-inj⁺ : {A : Set} → {t : Time⁺} → Injective {TDList A} (delayTDL⁺ t) 
lemTDL-delay-inj⁺ {A} {t} {[]} {[]} refl = refl
lemTDL-delay-inj⁺ {A} {t} {[]} {(_ , _) ∷ _} ()
lemTDL-delay-inj⁺ {A} {t} {(_ , _) ∷ _} {[]} ()
lemTDL-delay-inj⁺ {A} {t} {(δ₁ , a₁) ∷ δas₁} {(δ₂ , a₂) ∷ δas₂} eq with ∷-inj eq
lemTDL-delay-inj⁺ {A} {t} {(δ₁ , a₁) ∷ .δas₂} {(δ₂ , a₂) ∷ δas₂} eq | eq' , refl with ×-inj eq'
lemTDL-delay-inj⁺ {A} {t} {(δ₁ , .a₂) ∷ .δas₂} {(δ₂ , a₂) ∷ δas₂} eq | eq' , refl | eql , refl with ⁺+⁺-cancL eql
lemTDL-delay-inj⁺ {A} {t} {(.δ₂ , .a₂) ∷ .δas₂} {(δ₂ , a₂) ∷ δas₂} eq | _ , refl | _ , refl | refl = refl 

lemTDL-delayEmpty : {A : Set} → {t : Time⁺} → (δas : TDList A) → delayTDL⁺ t δas ≡ [] → δas ≡ []
lemTDL-delayEmpty [] p = p
lemTDL-delayEmpty ((_ , _) ∷ _) ()

lemTDL-delayEmptyResp : {A : Set} → {t : Time⁺} → (δas : TDList A) → δas ≡ [] → delayTDL⁺ t δas ≡ []
lemTDL-delayEmptyResp .[] refl = refl

lemTDL-dropEmpty→takeAll : {A : Set} → (t : Time⁺) → (δas : TDList A) → dropIncl⁺ t δas ≡ [] → takeIncl⁺ t δas ≡ δas
lemTDL-dropEmpty→takeAll t [] p = refl
lemTDL-dropEmpty→takeAll t ((δ , a) ∷ δas) p with compareℜ⁺ δ t
lemTDL-dropEmpty→takeAll t ((.t , a) ∷ δas) p | refl = ∷-congL (sym (lemTDL-delayEmpty δas p))
lemTDL-dropEmpty→takeAll t ((δ , a) ∷ δas) p | less lt = ∷-congL (lemTDL-dropEmpty→takeAll ((t ⁺-⁺ δ) lt) δas (lemTDL-delayEmpty _ p))
lemTDL-dropEmpty→takeAll t ((δ , a) ∷ δas) () | more _

lemTDL-takeDropEmpty⁺ : {A : Set} → (t : Time⁺) → (δas : TDList A) → dropIncl⁺ t δas ≡ [] → takeIncl⁺ t δas ≡ [] → δas ≡ []
lemTDL-takeDropEmpty⁺ t δas p = trans (sym (lemTDL-dropEmpty→takeAll t δas p))
{-
lemTDL-takeDropEmpty : {A : Set} → (t : Time) → (δas : TDList A) → dropIncl t δas ≡ [] → takeIncl t δas ≡ [] → δas ≡ []
lemTDL-takeDropEmpty O      δas = const
lemTDL-takeDropEmpty (t >0) δas = lemTDL-takeDropEmpty⁺ t δas

lemTDL-takeDropEmptyAbsurd⁺ : {A : Set} → (t : Time⁺) → (δa : Δt × A) → (δas : TDList A) → dropIncl⁺ t (δa ∷ δas) ≡ [] → takeIncl⁺ t (δa ∷ δas) ≡ [] → False
lemTDL-takeDropEmptyAbsurd⁺ t δa δas deq teq with lemTDL-takeDropEmpty⁺ t (δa ∷ δas) deq teq
... | ()


lemTDL-safelastEqTakeAll : {A : Set} → (t : Time⁺) → (δa δa₀ : Δt × A) → (δas : TDList A) → dropIncl⁺ t (δa ∷ δas) ≡ [] → safelast δa δas ≡ safelast δa₀ (takeIncl⁺ t (δa ∷ δas))
lemTDL-safelastEqTakeAll t δa δa₀ δas p with lemTDL-dropEmpty→takeAll t (δa ∷ δas) p
... | q = cong (safelast δa₀) (sym q)

lemTDL-sndsafelastEqTakeAll : {A : Set} → (t : Time⁺) → (δa δa₀ : Δt × A) → (δas : TDList A) → dropIncl⁺ t (δa ∷ δas) ≡ [] → snd (safelast δa δas) ≡ snd (safelast δa₀ (takeIncl⁺ t (δa ∷ δas)))
lemTDL-sndsafelastEqTakeAll t δa δa₀ δas p = cong snd (lemTDL-safelastEqTakeAll t δa δa₀ δas p)

lemTDL-takeDropSafeLast⁺ : {A : Set} → (t : Time⁺) → (δa₁ δa₂ : Δt × A) → (δas₁ δas₂ : TDList A) →
                         dropIncl⁺ t (δa₂ ∷ δas₂) ≡ [] → δa₁ ∷ δas₁ ≡ takeIncl⁺ t (δa₂ ∷ δas₂) → snd (safelast δa₂ δas₂) ≡ snd (safelast δa₁ δas₁)
lemTDL-takeDropSafeLast⁺ t (d₁ , a₁) (d₂ , a₂) δas₁ δas₂ p q with compareℜ⁺ d₂ t
lemTDL-takeDropSafeLast⁺ t (.t , .a₂) (.t , a₂) .[] δas₂ p refl | refl with lemTDL-delayEmpty δas₂ p
lemTDL-takeDropSafeLast⁺ t (.t , .a₂) (.t , a₂) .[] .[] p refl | refl | refl  = refl
lemTDL-takeDropSafeLast⁺ t (d₁ , a₁) (d₂ , a₂) δas₁ δas₂ () () | more _
lemTDL-takeDropSafeLast⁺ t (.d₂ , .a₂) (d₂ , a₂) .[] [] p refl | less _ = refl
lemTDL-takeDropSafeLast⁺ t (.d₂ , .a₂) (d₂ , a₂) ._ (δa₂ ∷ δas₂) p refl | less lt = lemTDL-sndsafelastEqTakeAll ((t ⁺-⁺ d₂) lt) δa₂ (d₂ , a₂) δas₂ (lemTDL-delayEmpty _ p)

lemTDL-takeDropSafeLast : {A : Set} → (t : Time) → (δa₁ δa₂ : Δt × A) → (δas₁ δas₂ : TDList A) →
                          dropIncl t (δa₂ ∷ δas₂) ≡ [] → δa₁ ∷ δas₁ ≡ takeIncl t (δa₂ ∷ δas₂) → snd (safelast δa₂ δas₂) ≡ snd (safelast δa₁ δas₁)
lemTDL-takeDropSafeLast O δa₁ δa₂ δas₁ δas₂ () q
lemTDL-takeDropSafeLast (t >0) δa₁ δa₂ δas₁ δas₂ p q = lemTDL-takeDropSafeLast⁺ t δa₁ δa₂ δas₁ δas₂ p q

----------------------------------------------------------------------------------------------------------

lemTDL-takeDropEqual⁺ : {A : Set} → (t : Time⁺) → (δas₁ δas₂ : TDList A) → takeIncl⁺ t δas₁ ≡ takeIncl⁺ t δas₂ → dropIncl⁺ t δas₁ ≡ dropIncl⁺ t δas₂ → δas₁ ≡ δas₂
lemTDL-takeDropEqual⁺ t [] [] teq deq = refl
lemTDL-takeDropEqual⁺ t [] (δa ∷ δas) teq deq = absurd (lemTDL-takeDropEmptyAbsurd⁺ t δa δas (sym deq) (sym teq))
lemTDL-takeDropEqual⁺ t (δa ∷ δas) [] teq deq = absurd (lemTDL-takeDropEmptyAbsurd⁺ t δa δas deq teq)
lemTDL-takeDropEqual⁺ t ((δ₁ , a) ∷ δas) ((δ₂ , b) ∷ δbs) teq deq with compareℜ⁺ δ₁ t | compareℜ⁺ δ₂ t
lemTDL-takeDropEqual⁺ t ((.t , .b) ∷ δas) ((.t , b) ∷ δbs) refl deq | refl | refl = cong2L _∷_ (lemTDL-delay-inj⁺ deq)
lemTDL-takeDropEqual⁺ t ((.t , a) ∷ δas) ((δ₂ , b) ∷ δbs) () deq | refl | more q
lemTDL-takeDropEqual⁺ t ((δ₁ , a) ∷ δas) ((δ₂ , b) ∷ δbs) () deq | less p | more q
lemTDL-takeDropEqual⁺ t ((δ₁ , a) ∷ δas) ((.t , b) ∷ δbs) () deq | more p | refl
lemTDL-takeDropEqual⁺ t ((δ₁ , a) ∷ δas) ((δ₂ , b) ∷ δbs) () deq | more p | less q
lemTDL-takeDropEqual⁺ t ((.δ₂ , .b) ∷ .δbs) ((δ₂ , b) ∷ δbs) refl refl | more p | more q = refl
lemTDL-takeDropEqual⁺ t ((δ₁ , a) ∷ δas) ((.t , b) ∷ δbs) teq deq | less p | refl with ∷-inj teq
lemTDL-takeDropEqual⁺ t ((.t , a) ∷ δas) ((.t , .a) ∷ δbs) teq deq | less p | refl | refl , teq' = absurd (<ℜ⁺-irreflexive p)
lemTDL-takeDropEqual⁺ t ((.t , a) ∷ δas) ((δ₂ , b) ∷ δbs) teq deq | refl | less q with ∷-inj teq
lemTDL-takeDropEqual⁺ t ((.t , a) ∷ δas) ((.t , .a) ∷ δbs) teq deq | refl | less q | refl , teq' = absurd (<ℜ⁺-irreflexive q)
lemTDL-takeDropEqual⁺ t ((δ₁ , a) ∷ δas) ((δ₂ , b) ∷ δbs) teq deq | less p | less q with ∷-inj teq
lemTDL-takeDropEqual⁺ t ((δ₁ , a) ∷ δas) ((.δ₁ , .a) ∷ δbs) teq deq | less p | less q | refl , teq' with lemTDL-delay-inj⁺ deq
lemTDL-takeDropEqual⁺ t ((δ₁ , a) ∷ δas) ((.δ₁ , .a) ∷ δbs) teq deq | less p | less q | refl , teq' | deq' with ℜ⁺-minus t δ₁ p | ℜ⁺-minus t δ₁ q | lem-ℜ⁺-minus-proofirrelevence p q
... | t' | .t' | refl = cong2L _∷_ (lemTDL-takeDropEqual⁺ t' δas δbs teq' deq')


lemTDL-takeDropEqual : {A : Set} → (t : Time) → (δas₁ δas₂ : TDList A) → takeIncl t δas₁ ≡ takeIncl t δas₂ → dropIncl t δas₁ ≡ dropIncl t δas₂ → δas₁ ≡ δas₂
lemTDL-takeDropEqual O = λ _ _ _ → id
lemTDL-takeDropEqual (t >0) = lemTDL-takeDropEqual⁺ t
-}
----------------------------------------------------------------------------------------------------------

lemTDL-dropInclSum→Empty⁺ : {A : Set} → (t : Time⁺) → (δas : TDList A) → sumTDL δas ≤ℜ₀ (t >0) → dropIncl⁺ t δas ≡ []
lemTDL-dropInclSum→Empty⁺ t [] p = refl
lemTDL-dropInclSum→Empty⁺ t ((δ , a) ∷ δas) p with >0-inj-≤ p | compareℜ⁺ δ t
lemTDL-dropInclSum→Empty⁺ t ((.t , a) ∷ []) p | p' | refl = refl
lemTDL-dropInclSum→Empty⁺ t ((.t , a) ∷ δa ∷ δas) p | p' | refl = absurd (<≤ℜ⁺-asym lem-ℜ⁺-+-<-increasingR p')
lemTDL-dropInclSum→Empty⁺ t ((δ , a) ∷ δas) p | p' | more q = absurd (<≤ℜ⁺-asym (≤<ℜ⁺-trans p' q) (lem-ℜ⁺₀-⁺+-increasingR (sumℜ⁺ (map fst δas))))
lemTDL-dropInclSum→Empty⁺ t ((δ , a) ∷ δas) p | p' | less q = lemTDL-delayEmptyResp _ (lemTDL-dropInclSum→Empty⁺ ((t ⁺-⁺ δ) q) δas (lem-ℜ₀-x⁺+₀y≤z→y≤z-x q p') )

lemTDL-dropInclSum→Empty : {A : Set} → (t : Time) → (δas : TDList A) → sumTDL δas ≤ℜ₀ t → dropIncl t δas ≡ []
lemTDL-dropInclSum→Empty (t >0) δas p = lemTDL-dropInclSum→Empty⁺ t δas p
lemTDL-dropInclSum→Empty O δas (inl ())
lemTDL-dropInclSum→Empty O [] (inr q) = refl
lemTDL-dropInclSum→Empty O (δa ∷ δas) (inr ())

----------------------------------------------------------------------------------------------------------

lemTDL-lookupHead : {A : Set} → {a : A} → {δas : TDList A} → (δ : Δt) → lookupTDL⁺ ((δ , a) ∷ δas) δ ≡ just a
lemTDL-lookupHead δ with compareℜ⁺ δ δ
lemTDL-lookupHead δ | refl   = refl
lemTDL-lookupHead δ | less p = absurd (<ℜ⁺-irreflexive p)
lemTDL-lookupHead δ | more p = absurd (<ℜ⁺-irreflexive p)

lemTDL-lookupHead' : {A : Set} → {a : A} → {δas : TDList A} → (δ : Δt) → IsJust (lookupTDL⁺ ((δ , a) ∷ δas) δ)
lemTDL-lookupHead' {A} {a} {δas} δ with lemTDL-lookupHead {A} {a} {δas} δ
lemTDL-lookupHead' {A} {a} {δas} δ | eq with lookupTDL⁺ ((δ , a) ∷ δas) δ
lemTDL-lookupHead' {A} {a} {δas} δ | refl | ._ = _

lemTDL-lookupBeforeHead⁺ : {A : Set} → {δ : Δt} → {t : Time⁺} → {a : A} → {δas : TDList A} → t <ℜ⁺ δ → lookupTDL⁺ ((δ , a) ∷ δas) t ≡ nothing
lemTDL-lookupBeforeHead⁺ {_} {δ} {t} lt with compareℜ⁺ δ t
lemTDL-lookupBeforeHead⁺ lt | refl = absurd (<ℜ⁺-irreflexive lt)
lemTDL-lookupBeforeHead⁺ lt | less p = absurd (<ℜ⁺-asym p lt)
lemTDL-lookupBeforeHead⁺ lt | more p = refl

lemTDL-lookupBeforeHead⁺' : {A : Set} → {δ : Δt} → {t : Time⁺} → {a : A} → {δas : TDList A} → t <ℜ⁺ δ → IsNothing (lookupTDL⁺ ((δ , a) ∷ δas) t)
lemTDL-lookupBeforeHead⁺' {A} {δ} {t} {a} {δas} lt with lemTDL-lookupBeforeHead⁺ {A} {δ} {t} {a} {δas} lt
lemTDL-lookupBeforeHead⁺' {A} {δ} {t} {a} {δas} lt | eq with lookupTDL⁺ ((δ , a) ∷ δas) t
lemTDL-lookupBeforeHead⁺' {A} {δ} {t} {a} {δas} lt | refl | ._ = id

lemTDL-lookupBeforeHead : {A : Set} → {δ : Δt} → {t : Time} → {a : A} → {δas : TDList A} → (t <ℜ₀ (δ >0)) → lookupTDL ((δ , a) ∷ δas) t ≡ nothing
lemTDL-lookupBeforeHead {_} {_} {O} lt = refl
lemTDL-lookupBeforeHead {_} {_} {t >0} lt = lemTDL-lookupBeforeHead⁺ lt

lemTDL-lookupBeforeHead' : {A : Set} → {δ : Δt} → {t : Time} → {a : A} → {δas : TDList A} → t <ℜ₀ (δ >0) → IsNothing (lookupTDL ((δ , a) ∷ δas) t)
lemTDL-lookupBeforeHead' {A} {δ} {t} {a} {δas} lt rewrite lemTDL-lookupBeforeHead {A} {δ} {t} {a} {δas} lt = id

lemTDL-lookupAfterHeadAux⁺ : {A : Set} → {a : A} → (δ t t' : Time⁺) → (δas : TDList A) → δ ⁺+⁺ t ≡ t' → lookupTDL⁺ ((δ , a) ∷ δas) t' ≡ lookupTDL⁺ δas t
lemTDL-lookupAfterHeadAux⁺ δ t t' δas eq with compareℜ⁺ δ t'
lemTDL-lookupAfterHeadAux⁺ δ t .δ δas eq | refl = absurd (lem-ℜ⁺-+-≱-increasingR (inr eq))
lemTDL-lookupAfterHeadAux⁺ δ t .(δ ⁺+⁺ t) δas refl | less p = cong (lookupTDL⁺ δas) (lem-ℜ⁺-[x+y]-x=y p)
lemTDL-lookupAfterHeadAux⁺ δ t .(δ ⁺+⁺ t) δas refl | more p = absurd (lem-ℜ⁺-+-≱-increasingR (inl p))

lemTDL-lookupAfterHead⁺ : {A : Set} → {a : A} → (δ t : Time⁺) → (δas : TDList A) → lookupTDL⁺ ((δ , a) ∷ δas) (δ ⁺+⁺ t) ≡ lookupTDL⁺ δas t
lemTDL-lookupAfterHead⁺ δ t δas = lemTDL-lookupAfterHeadAux⁺ δ t (δ ⁺+⁺ t) δas refl

----------------------------------------------------------------------------------------------------------

lemTDL-lookupHead-eq⁺ : {A : Set} → {δ : Δt} → {a : A} → {δas₁ δas₂ : TDList A} → lookupTDL⁺ ((δ , a) ∷ δas₁) δ ≡ lookupTDL⁺ ((δ , a) ∷ δas₂) δ
lemTDL-lookupHead-eq⁺ = trans (lemTDL-lookupHead _) (sym (lemTDL-lookupHead _))

lemTDL-lookupBeforeHead-eq⁺ : {A : Set} → {δ : Δt} → {t : Time⁺} → {a : A} → {δas₁ δas₂ : TDList A} → t <ℜ⁺ δ → lookupTDL⁺ ((δ , a) ∷ δas₁) t ≡  lookupTDL⁺ ((δ , a) ∷ δas₂) t
lemTDL-lookupBeforeHead-eq⁺ lt = trans (lemTDL-lookupBeforeHead⁺ lt) (sym (lemTDL-lookupBeforeHead⁺ lt))

lemTDL-lookupUptoHead-eq⁺ : {A : Set} → {δ : Δt} → {t : Time⁺} → {a : A} → {δas₁ δas₂ : TDList A} → t ≤ℜ⁺ δ → lookupTDL⁺ ((δ , a) ∷ δas₁) t ≡  lookupTDL⁺ ((δ , a) ∷ δas₂) t
lemTDL-lookupUptoHead-eq⁺ (inl p) = lemTDL-lookupBeforeHead-eq⁺ p
lemTDL-lookupUptoHead-eq⁺ (inr refl) = lemTDL-lookupHead-eq⁺


lemTDL-lookupUptoHead-eq : {A : Set} → {δ : Δt} → {t : Time} → {a : A} → {δas₁ δas₂ : TDList A} → t ≤ℜ₀ (δ >0) → lookupTDL ((δ , a) ∷ δas₁) t ≡  lookupTDL ((δ , a) ∷ δas₂) t
lemTDL-lookupUptoHead-eq {A} {δ} {O} lt = refl
lemTDL-lookupUptoHead-eq {A} {δ} {t >0} lt = lemTDL-lookupUptoHead-eq⁺ (>0-inj-≤ lt)


----------------------------------------------------------------------------------------------------------

lemTDL-lookup⁺-neq : {A : Set} → (δ : Δt) → (a : A) → (δas : TDList A) → lookupTDL⁺ ((δ , a) ∷ δas) δ ≢ lookupTDL⁺ [] δ
lemTDL-lookup⁺-neq δ a δas eq with trans (sym eq) (lemTDL-lookupHead {_} {a} {δas} δ)
... | ()

lemTDL-lookup⁺-neq' : {A : Set} → {δ : Δt} → {a : A} → {δas δas₁ δas₂ : TDList A} → δas₁ ≡ ((δ , a) ∷ δas) → δas₂ ≡ [] → lookupTDL⁺ δas₁ δ ≢ lookupTDL⁺ δas₂ δ
lemTDL-lookup⁺-neq' {A} {δ} {a} refl refl = lemTDL-lookup⁺-neq δ a _

----------------------------------------------------------------------------------------------------------

lemTDL-lookupEqTail⁺ : {A : Set} → {a : A} → (δ t : Time⁺) → (δas₁ δas₂ : TDList A) → lookupTDL⁺ ((δ , a) ∷ δas₁) (δ ⁺+⁺ t) ≡ lookupTDL⁺ ((δ , a) ∷ δas₂) (δ ⁺+⁺ t) → lookupTDL⁺ δas₁ t ≡ lookupTDL⁺ δas₂ t
lemTDL-lookupEqTail⁺ δ t δas₁ δas₂ eq = trans2 (sym (lemTDL-lookupAfterHead⁺ δ t δas₁)) eq (lemTDL-lookupAfterHead⁺ δ t δas₂)

lemTDL-lookupEqTail : {A : Set} → {a : A} → (δ : Δt) → (δas₁ δas₂ : TDList A) → ((t : Time) → lookupTDL ((δ , a) ∷ δas₁) t ≡ lookupTDL ((δ , a) ∷ δas₂) t) → ((t : Time) → lookupTDL δas₁ t ≡ lookupTDL δas₂ t)
lemTDL-lookupEqTail δ δas₁ δas₂ p O = refl
lemTDL-lookupEqTail δ δas₁ δas₂ p (t >0) = lemTDL-lookupEqTail⁺ δ t δas₁ δas₂ (p ((δ ⁺+⁺ t) >0))

lemTDL-lookupEq : {A : Set} → (δas₁ δas₂ : TDList A) → ((t : Time) → lookupTDL δas₁ t ≡ lookupTDL δas₂ t) → δas₁ ≡ δas₂
lemTDL-lookupEq [] [] p = refl
lemTDL-lookupEq [] ((δ , a) ∷ δas) p = absurd (lemTDL-lookup⁺-neq δ a δas (sym (p (δ >0))))
lemTDL-lookupEq ((δ , a) ∷ δas) [] p = absurd (lemTDL-lookup⁺-neq δ a δas (p (δ >0)))
lemTDL-lookupEq ((δ₁ , a₁) ∷ δas₁) ((δ₂ , a₂) ∷ δas₂) p with p (δ₁ >0) | p (δ₂ >0)
lemTDL-lookupEq ((δ₁ , a₁) ∷ δas₁) ((δ₂ , a₂) ∷ δas₂) p | eq₁ | eq₂ with compareℜ⁺ δ₁ δ₂
lemTDL-lookupEq ((δ₁ , a₁) ∷ δas₁) ((δ₂ , a₂) ∷ δas₂) p | eq₁ | eq₂ | less q = absurd (lem-just≠nothing (trans2 (sym (lemTDL-lookupHead δ₁)) eq₁ (lemTDL-lookupBeforeHead q)))
lemTDL-lookupEq ((δ₁ , a₁) ∷ δas₁) ((δ₂ , a₂) ∷ δas₂) p | eq₁ | eq₂ | more q = absurd (lem-just≠nothing (sym (trans eq₂ (lemTDL-lookupHead δ₂))))
lemTDL-lookupEq ((.δ₂ , a₁) ∷ δas₁) ((δ₂ , a₂) ∷ δas₂) p | eq₁ | eq₂ | refl with trans eq₂ (lemTDL-lookupHead δ₂)
lemTDL-lookupEq ((.δ₂ , ._) ∷ δas₁) ((δ₂ , a₂) ∷ δas₂) p | eq₁ | eq₂ | refl | refl = ∷-congL (lemTDL-lookupEq δas₁ δas₂ (lemTDL-lookupEqTail δ₂ δas₁ δas₂ p))

----------------------------------------------------------------------------------------------------------

lemTDL-lookupDelay : {A : Set} → (δas : TDList A) → (d t : Time⁺) → (q : d <ℜ⁺ t)→ lookupTDL⁺ (delayTDL⁺ d δas) t ≡ lookupTDL⁺ δas ((t ⁺-⁺ d) q)
lemTDL-lookupDelay [] d t q = refl
lemTDL-lookupDelay ((δ , a) ∷ δas) d t q with compareℜ⁺ (d ⁺+⁺ δ) t
lemTDL-lookupDelay ((δ , a) ∷ δas) d ._ q | refl = trans (sym (lemTDL-lookupHead δ))
                                                         (cong (lookupTDL⁺ ((δ , a) ∷ δas)) (sym (lem-ℜ⁺-[x+y]-x=y q)))
lemTDL-lookupDelay ((δ , a) ∷ δas) d t q | less r with compareℜ⁺ δ ((t ⁺-⁺ d) q)
lemTDL-lookupDelay ((._ , a) ∷ δas) d t q | less r | refl  = absurd (<ℜ⁺-irreflexive (<ℜ⁺-substL (lem-ℜ⁺-x+[y-x]=y q) r))
lemTDL-lookupDelay ((δ , a) ∷ δas) d t q | less r | less s = cong (lookupTDL⁺ δas) (lem-ℜ⁺-x-[y+z]=[x-y]-z r q s)
lemTDL-lookupDelay ((δ , a) ∷ δas) d t q | less r | more s = absurd (<ℜ⁺-asym r (lem-ℜ⁺-x-y<z→x<y+z q s))
lemTDL-lookupDelay ((δ , a) ∷ δas) d t q | more r with compareℜ⁺ δ ((t ⁺-⁺ d) q)
lemTDL-lookupDelay ((._ , a) ∷ δas) d t q | more r | refl  = absurd (<ℜ⁺-irreflexive (<ℜ⁺-substR (lem-ℜ⁺-x+[y-x]=y q) r))
lemTDL-lookupDelay ((δ , a) ∷ δas) d t q | more r | less s = absurd (<ℜ⁺-asym r (lem-ℜ⁺-y<z-x→x+y<z q s))
lemTDL-lookupDelay ((δ , a) ∷ δas) d t q | more _ | more _ = refl

lemTDL-lookupLessThanDelay : {A : Set} → (δas : TDList A) → (t₁ t₂ : Time⁺) → t₁ <ℜ⁺ t₂ → IsNothing (lookupTDL⁺ (delayTDL⁺ t₂ δas) t₁)
lemTDL-lookupLessThanDelay [] t₁ t₂ lt p = p
lemTDL-lookupLessThanDelay ((δ , a) ∷ δas) t₁ t₂ lt p with compareℜ⁺ (t₂ ⁺+⁺ δ) t₁
lemTDL-lookupLessThanDelay ((δ , a) ∷ δas) .(t₂ ⁺+⁺ δ) t₂ lt p | refl = <ℜ⁺-asym lt lem-ℜ⁺-+-<-increasingR
lemTDL-lookupLessThanDelay ((δ , a) ∷ δas) t₁ t₂ lt p | less q = <ℜ⁺-asym q (<ℜ⁺-trans lt lem-ℜ⁺-+-<-increasingR)
lemTDL-lookupLessThanDelay ((δ , a) ∷ δas) t₁ t₂ lt p | more q = p

lemTDL-lookupDelayed→Nothing : {A : Set} → (t : Time⁺) → (δas : TDList A) → lookupTDL⁺ (delayTDL⁺ t δas) t ≡ nothing
lemTDL-lookupDelayed→Nothing t [] = refl
lemTDL-lookupDelayed→Nothing t ((δ , a) ∷ δas) = lemTDL-lookupBeforeHead⁺ lem-ℜ⁺-+-<-increasingR 

lemTDL-lookupDelayed→Nothing' : {A : Set} → (t : Time⁺) → (δas : TDList A) → IsNothing (lookupTDL⁺ (delayTDL⁺ t δas) t)
lemTDL-lookupDelayed→Nothing' t δas rewrite lemTDL-lookupDelayed→Nothing t δas = id

{-
lemTDL-lookupDropped→Nothing1 : {A : Set} → (t : Time⁺) → (δas : TDList A) → IsNothing (lookupTDL⁺ (dropIncl⁺ t δas) t)
lemTDL-lookupDropped→Nothing1 t [] () 
lemTDL-lookupDropped→Nothing1 t ((δ , a) ∷ δas) p with compareℜ⁺ δ t
lemTDL-lookupDropped→Nothing1 t ((.t , a) ∷ δas) p | refl   = lemTDL-lookupDelayed→Nothing t δas p
lemTDL-lookupDropped→Nothing1 t ((δ , a) ∷ δas) p  | more q = lemTDL-lookupLessThenDelta2⁺ t δ q p
lemTDL-lookupDropped→Nothing1 t ((δ , a) ∷ δas) p  | less q = lemTDL-lookupDropped→Nothing1 ((t ⁺-⁺ δ) q) δas (subst (cong IsJust (lemTDL-lookupDelay (dropIncl⁺ ((t ⁺-⁺ δ) q) δas) δ t q)) p)

lemTDL-lookupDropped→Nothing2 : {A : Set} → (t₁ t₂ : Time⁺) → t₁ <ℜ⁺ t₂ → (δas : TDList A) → IsNothing (lookupTDL⁺ (dropIncl⁺ t₂ δas) t₁)
lemTDL-lookupDropped→Nothing2 t₁ t₂ lt [] p = p
lemTDL-lookupDropped→Nothing2 t₁ t₂ lt ((δ , a) ∷ δas) p with compareℜ⁺ δ t₂
lemTDL-lookupDropped→Nothing2 t₁ t₂ lt ((.t₂ , a) ∷ δas) p | refl   = lemTDL-lookupLessThanDelay δas t₁ t₂ lt p
lemTDL-lookupDropped→Nothing2 t₁ t₂ lt ((δ , a)   ∷ δas) p | more q = lemTDL-lookupLessThenDelta2⁺ t₁ δ (<ℜ⁺-trans lt q) p
lemTDL-lookupDropped→Nothing2 t₁ t₂ lt ((δ , a)   ∷ δas) p | less q with compareℜ⁺ δ t₁
lemTDL-lookupDropped→Nothing2 t₁ t₂ lt ((.t₁ , a) ∷ δas) p | less q | refl = lemTDL-lookupDelayed→Nothing t₁ (dropIncl⁺ ((t₂ ⁺-⁺ t₁) q) δas) p
lemTDL-lookupDropped→Nothing2 t₁ t₂ lt ((δ , a) ∷ δas) p | less q | less r = lemTDL-lookupDropped→Nothing2 ((t₁ ⁺-⁺ δ) r) ((t₂ ⁺-⁺ δ) q) (lem-ℜ⁺-minus-<-cancellative r q lt) δas (subst (cong IsJust (lemTDL-lookupDelay (dropIncl⁺ ((t₂ ⁺-⁺ δ) q) δas) δ t₁ r)) p)
lemTDL-lookupDropped→Nothing2 t₁ t₂ lt ((δ , a) ∷ δas) p | less q | more r = lemTDL-lookupLessThanDelay (dropIncl⁺ ((t₂ ⁺-⁺ δ) q) δas) t₁ δ r p 

lemTDL-lookupDropped→Nothing : {A : Set} → (t₁ t₂ : Time⁺) → t₁ ≤ℜ⁺ t₂ → (δas : TDList A) → IsNothing (lookupTDL⁺ (dropIncl⁺ t₂ δas) t₁)
lemTDL-lookupDropped→Nothing t₁ t₂ (inl lt)    = lemTDL-lookupDropped→Nothing2 t₁ t₂ lt
lemTDL-lookupDropped→Nothing t₁ .t₁ (inr refl) = lemTDL-lookupDropped→Nothing1 t₁

-----------------------------------------------------
-}
-- lemTDL-lookupTaken⁺ : {A : Set} → (t₁ t₂ : Time⁺) → t₁ ≤ℜ⁺ t₂ → (δas : TDList A) → lookupTDL⁺ (takeIncl⁺ t₂ δas) t₁ ≡ lookupTDL⁺ δas t₁
-- lemTDL-lookupTaken⁺ t₁ t₂ lt [] = refl
-- lemTDL-lookupTaken⁺ t₁ t₂ lt ((δ , a) ∷ δas) with compareℜ⁺ δ t₁ | compareℜ⁺ δ t₂
-- lemTDL-lookupTaken⁺ .t₂ t₂ lt ((.t₂ , a) ∷ δas) | refl | refl = lemTDL-lookupHead t₂
-- lemTDL-lookupTaken⁺ t₁ t₂ lt ((.t₁ , a) ∷ δas)  | refl | less q = lemTDL-lookupHead t₁
-- lemTDL-lookupTaken⁺ t₁ t₂ lt ((.t₁ , a) ∷ δas)  | refl | more q = absurd (<≤ℜ⁺-asym q lt)
-- lemTDL-lookupTaken⁺ t₁ t₂ lt ((.t₂ , a) ∷ δas) | more p | refl = lemTDL-lookupBeforeHead-eq⁺ p
-- lemTDL-lookupTaken⁺ t₁ t₂ lt ((δ , a) ∷ δas) | more p | less q = lemTDL-lookupBeforeHead-eq⁺ p
-- lemTDL-lookupTaken⁺ t₁ t₂ lt ((δ , a) ∷ δas) | more p | more q = refl
-- lemTDL-lookupTaken⁺ t₁ t₂ lt ((.t₂ , a) ∷ δas) | less p | refl = absurd (<≤ℜ⁺-asym p lt)
-- lemTDL-lookupTaken⁺ t₁ t₂ lt ((δ , a) ∷ δas) | less p | more q = absurd (<ℜ⁺-asym p (≤<ℜ⁺-trans lt q))
-- lemTDL-lookupTaken⁺ t₁ t₂ lt ((δ , a) ∷ δas) | less p | less q with compareℜ⁺ δ t₁
-- lemTDL-lookupTaken⁺ .δ t₂ lt ((δ , a) ∷ δas) | less p | less q | refl   = absurd (<ℜ⁺-irreflexive p)
-- lemTDL-lookupTaken⁺ t₁ t₂ lt ((δ , a) ∷ δas) | less p | less q | more r = absurd (<ℜ⁺-asym p r)
-- lemTDL-lookupTaken⁺ t₁ t₂ lt ((δ , a) ∷ δas) | less p | less q | less r = trans (cong (lookupTDL⁺ (takeIncl⁺ ((t₂ ⁺-⁺ δ) q) δas)) (lem-ℜ⁺-minus-proofirrelevence r p)) (lemTDL-lookupTaken⁺ ((t₁ ⁺-⁺ δ) p) ((t₂ ⁺-⁺ δ) q) (lem-ℜ⁺-minus-≤-cancellative p q lt) δas)

-- lemTDL-lookupTaken : {A : Set} → (t₁ t₂ : Time) → t₁ ≤ℜ₀ t₂ → (δas : TDList A) → lookupTDL (takeIncl t₂ δas) t₁ ≡ lookupTDL δas t₁
-- lemTDL-lookupTaken O t₂ lt δas = refl
-- lemTDL-lookupTaken (_ >0) O (inl ()) δas
-- lemTDL-lookupTaken (_ >0) O (inr ()) δas
-- lemTDL-lookupTaken (t₁ >0) (t₂ >0) lt δas = lemTDL-lookupTaken⁺ t₁ t₂ (lem-ℜ⁺-leq lt) δas

lemTDL-lookupTaken⁺ : {A : Set} → (t₁ t₂ : Time⁺) → t₁ ≤ℜ⁺ t₂ → (δas : TDList A) → lookupTDL⁺ (takeIncl (t₂ >0) δas) t₁ ≡ lookupTDL⁺ δas t₁
lemTDL-lookupTaken⁺ t₁ t₂ lt [] = refl
lemTDL-lookupTaken⁺ t₁ t₂ lt ((δ , a) ∷ δas) with compareℜ₀ (δ >0) (t₂ >0)
lemTDL-lookupTaken⁺ t₁ ._ lt ((δ , a) ∷ δas) | refl = lemTDL-lookupUptoHead-eq⁺ lt
lemTDL-lookupTaken⁺ t₁ t₂ lt ((δ , a) ∷ δas) | more p = sym (lemTDL-lookupBeforeHead⁺ (≤<ℜ⁺-trans lt p))
lemTDL-lookupTaken⁺ t₁ t₂ lt ((δ , a) ∷ δas) | less p with compareℜ⁺ δ t₁
lemTDL-lookupTaken⁺ ._ t₂ lt ((δ , a) ∷ δas) | less p | refl = refl
lemTDL-lookupTaken⁺ t₁ t₂ lt ((δ , a) ∷ δas) | less p | less q = lemTDL-lookupTaken⁺ (ℜ⁺-minus t₁ δ q) (ℜ⁺-minus t₂ δ p) (lem-ℜ⁺-minus-≤-cancellative q p lt) δas
lemTDL-lookupTaken⁺ t₁ t₂ lt ((δ , a) ∷ δas) | less p | more q = refl

lemTDL-lookupTaken : {A : Set} → (t₁ t₂ : Time) → t₁ ≤ℜ₀ t₂ → (δas : TDList A) → lookupTDL (takeIncl t₂ δas) t₁ ≡ lookupTDL δas t₁
lemTDL-lookupTaken O t₂ lt δas = refl
lemTDL-lookupTaken (t₁ >0) O (inl ()) δas
lemTDL-lookupTaken (t₁ >0) O (inr ()) δas
lemTDL-lookupTaken (t₁ >0) (t₂ >0) lt δas = lemTDL-lookupTaken⁺ t₁ t₂ (>0-inj-≤ lt) δas

----------------------------------------------------------------------------------------------------------

lemTDL-lookupAtSum : {A : Set} → (t : Time⁺) → (δas : TDList A) → sumTDL δas ≡ t >0 → IsJust (lookupTDL⁺ δas t)
lemTDL-lookupAtSum t [] ()
lemTDL-lookupAtSum t ((δ , a) ∷ δas) eq with compareℜ⁺ δ t
lemTDL-lookupAtSum t ((.t , a) ∷ δas) eq | refl = _
lemTDL-lookupAtSum t ((δ , a) ∷ δas) eq | less p = lemTDL-lookupAtSum (ℜ⁺-minus t δ p) δas (lem-ℜ₀-x⁺+₀y=z→y=z-x p eq)
lemTDL-lookupAtSum ._ ((δ , a) ∷ δas) refl | more p = absurd (lem-ℜ⁺₀-+-x+y≰x (sumTDL δas) p)

lemTDL-lookupBeyondSum⁺ : {A : Set} → (t : Time⁺) → (δas : TDList A) → sumTDL δas <ℜ₀ (t >0) → lookupTDL⁺ δas t ≡ nothing
lemTDL-lookupBeyondSum⁺ t [] _ = refl
lemTDL-lookupBeyondSum⁺ t ((δ , a) ∷ δas) lt with compareℜ⁺ δ t
lemTDL-lookupBeyondSum⁺ t ((.t , a) ∷ δas) lt | refl   = absurd (<≤ℜ⁺-asym lt (lem-ℜ⁺₀-⁺+-increasingR (sumTDL δas)))
lemTDL-lookupBeyondSum⁺ t ((δ , a) ∷ δas) lt  | less q = lemTDL-lookupBeyondSum⁺ (ℜ⁺-minus t δ q) δas (lem-ℜ₀-x⁺+₀y<z→y<z-x q lt)
lemTDL-lookupBeyondSum⁺ t ((δ , a) ∷ δas) lt  | more q = refl

lemTDL-lookupBeyondSum : {A : Set} → (t : Time) → (δas : TDList A) → sumTDL δas <ℜ₀ t → lookupTDL δas t ≡ nothing
lemTDL-lookupBeyondSum O δas ()
lemTDL-lookupBeyondSum (t >0) δas lt = lemTDL-lookupBeyondSum⁺ t δas lt

lemTDL-lookupBeyondSum' : {A : Set} → (t : Time) → (δas : TDList A) → sumTDL δas <ℜ₀ t → IsNothing (lookupTDL δas t)
lemTDL-lookupBeyondSum' t δas lt rewrite lemTDL-lookupBeyondSum t δas lt = id

lemTDL-lookupSumBeyond : {A : Set} → (t : Time⁺) → (δas : TDList A) → IsNothing (lookupTDL⁺ δas t) → sumTDL δas ≤ℜ₀ (t >0) → sumTDL δas <ℜ₀ (t >0)
lemTDL-lookupSumBeyond t δas p (inl q) = q
lemTDL-lookupSumBeyond t δas p (inr q) = absurd (p (lemTDL-lookupAtSum t δas q))

----------------------------------------------------------------------------------------------------------

lemTDL-sumTakeExcl⁺ : {A : Set} → (t : Time⁺) → (δas : TDList A) → sumTDL (takeExcl⁺ t δas) <ℜ₀ (t >0)
lemTDL-sumTakeExcl⁺ t [] = _
lemTDL-sumTakeExcl⁺ t ((δ , a) ∷ δas) with compareGeqℜ⁺ δ t
lemTDL-sumTakeExcl⁺ t ((δ , a) ∷ δas) | less p = lem-ℜ₀-y<z⁺-⁺x→x⁺+₀y<z p (lemTDL-sumTakeExcl⁺ ((t ⁺-⁺ δ) p) δas)
lemTDL-sumTakeExcl⁺ t ((δ , a) ∷ δas) | geq p  = _

lemTDL-sumTakeIncl⁺ : {A : Set} → (t : Time⁺) → (δas : TDList A) → sumTDL (takeIncl⁺ t δas) ≤ℜ₀ (t >0)
lemTDL-sumTakeIncl⁺ t [] = inl _
lemTDL-sumTakeIncl⁺ t ((δ , a) ∷ δas) with compareℜ⁺ δ t
lemTDL-sumTakeIncl⁺ ._ ((δ , a) ∷ δas) | refl = inr refl
lemTDL-sumTakeIncl⁺ t ((δ , a) ∷ δas) | less p = lem-ℜ₀-y≤z⁺-⁺x→x⁺+₀y≤z p (lemTDL-sumTakeIncl⁺ (ℜ⁺-minus t δ p) δas)
lemTDL-sumTakeIncl⁺ t ((δ , a) ∷ δas) | more p = inl _

-- lemTDL-sumTakeIncl : {A : Set} → (t : Time) → (δas : TDList A) → sumTDL (takeIncl t δas) ≤ℜ₀ t
-- lemTDL-sumTakeIncl O δas = inr refl
-- lemTDL-sumTakeIncl (t >0) δas = lemTDL-sumTakeIncl⁺ t δas

-- lemTDL-sumTakeExcl : {A : Set} → (t : Time) → (δas : TDList A) → sumTDL (takeIncl t δas) ≤ℜ₀ t
-- lemTDL-sumTakeExcl O δas = inr refl
-- lemTDL-sumTakeExcl (t >0) δas = lemTDL-sumTakeIncl⁺ t δas


lemTDL-sumTakeIncl : {A : Set} → (t : Time) → (δas : TDList A) → sumTDL (takeIncl t δas) ≤ℜ₀ t
lemTDL-sumTakeIncl t [] = ≤ℜ₀-min
lemTDL-sumTakeIncl t ((δ , a) ∷ δas) with compareLeqℜ₀ (δ >0) t
... | leq p  = fst (lem-ℜ₀-y≤z₀-⁺x→x⁺+₀y≤z p) (lemTDL-sumTakeIncl (ℜ₀⁺₀-minus t δ p) δas)
... | more p = ≤ℜ₀-min

----------------------------------------------------------------------------------------------------------

lemTDL-takeIncl→sum : {A : Set} → {t : Time} → (δas : TDList A) → takeIncl t δas ≡ δas → sumTDL δas ≤ℜ₀ t
lemTDL-takeIncl→sum δas eq = subst (cong2R _≤ℜ₀_ (cong sumTDL eq)) (lemTDL-sumTakeIncl _ δas)

-- lemTDL-sum→takeIncl⁺ : {A : Set} → (t : Time⁺) → (δas : TDList A) → sumTDL δas ≤ℜ₀ (t >0) → takeIncl⁺ t δas ≡ δas
-- lemTDL-sum→takeIncl⁺ t δas lt = {!!}

-- lemTDL-sum→takeIncl : {A : Set} → (t : Time) → (δas : TDList A) → sumTDL δas ≤ℜ₀ t → takeIncl t δas ≡ δas
-- lemTDL-sum→takeIncl O [] lt = refl
-- lemTDL-sum→takeIncl O (_ ∷ _) (inl ())
-- lemTDL-sum→takeIncl O (_ ∷ _) (inr ())
-- lemTDL-sum→takeIncl (t >0) δas lt = {!!} -- lemTDL-sum→takeIncl⁺ δas lt t

lemTDL-sum→takeIncl : {A : Set} → (t : Time) → (δas : TDList A) → sumTDL δas ≤ℜ₀ t → takeIncl t δas ≡ δas
lemTDL-sum→takeIncl t [] lt = refl
lemTDL-sum→takeIncl t ((δ , a) ∷ δas) lt with compareLeqℜ₀ (δ >0) t
lemTDL-sum→takeIncl t ((δ , a) ∷ δas) lt | leq p = ∷-congL (lemTDL-sum→takeIncl (ℜ₀⁺₀-minus t δ p) δas (snd (lem-ℜ₀-y≤z₀-⁺x→x⁺+₀y≤z p) lt))
lemTDL-sum→takeIncl t ((δ , a) ∷ δas) lt | more p = absurd (<≤ℜ₀-asym p (≤ℜ₀-trans (lem-ℜ⁺₀-⁺+₀-increasingR (sumℜ⁺ (map fst δas))) lt))

----------------------------------------------------------------------------------------------------------

lemTDL-lookupBeyondTake : {A : Set} → {t₁ t₂ : Time} → (δas : TDList A) → t₁ <ℜ₀ t₂ → takeIncl t₁ δas ≡ δas → IsNothing (lookupTDL δas t₂)
lemTDL-lookupBeyondTake δas lt eq = lemTDL-lookupBeyondSum' _ δas (≤<ℜ₀-trans (lemTDL-takeIncl→sum δas eq) lt)

----------------------------------------------------------------------------------------------------------

lemTDL-sum-head : {A : Set} → {t : Time} → (δ : Δt) → (a : A) → (δas : TDList A) → sumTDL ((δ , a) ∷ δas) ≤ℜ₀ t → (δ >0) ≤ℜ₀ t
lemTDL-sum-head δ a δas lt = ≤ℜ₀-trans (lem-ℜ⁺₀-⁺+₀-increasingR (sumTDL δas)) lt

----------------------------------------------------------------------------------------------------------

lemTDL-take0 : {A : Set} → (δas : TDList A) → takeIncl O δas ≡ []
lemTDL-take0 [] = refl
lemTDL-take0 ((_ , _) ∷ _) = refl

lemTDL-takeInclHead : {A : Set} → {δ : Δt} → {a : A} → (δas : TDList A) → takeIncl (δ >0) ((δ , a) ∷ δas) ≡ (δ , a) ∷ []
lemTDL-takeInclHead {_} {δ} δas with compareLeqℜ₀ (δ >0) (δ >0)
... | leq p  = ∷-congL (trans (cong2R takeIncl {_} {_} {δas} (lem-ℜ₀⁺₀-minus-O p)) (lemTDL-take0 δas))
... | more p = absurd (<ℜ⁺-irreflexive p)

----------------------------------------------------------------------------------------------------------

postulate lemTDL-isNothing-lookup-filter : {A : Set} → (δas : TDList A) → (p : A → Bool) → (t : Time⁺) → IsNothing (lookupTDL⁺ δas t) → IsNothing (lookupTDL⁺ (filterTDL p δas) t)
-- lemTDL-isNothing-filter [] p t isn = isn
-- lemTDL-isNothing-filter ((δ , a) ∷ δas) p t isn with p a
-- lemTDL-isNothing-filter ((δ , a) ∷ δas) p t isn | false with compareℜ⁺ δ t
-- lemTDL-isNothing-filter ((.t , a) ∷ δas) p t isn | false | refl = magic isn
-- lemTDL-isNothing-filter ((δ , a) ∷ δas) p t isn | false | less q = {!!}
-- lemTDL-isNothing-filter ((δ , a) ∷ δas) p t isn | false | more q = {!!}
-- lemTDL-isNothing-filter ((δ , a) ∷ δas) p t isn | true = {!!}

postulate lemTDL-lookup-filter-filter : {A : Set} → (δas : TDList A) → (p : A → Bool) → (t : Time⁺) → lookupTDL⁺ (filterTDL p δas) t ≡ maybeFilter p (lookupTDL⁺ δas t)
-- lemTDL-lookup-filter-filter [] p t = refl
-- lemTDL-lookup-filter-filter ((δ , a) ∷ δas) p t with compareℜ⁺ δ t
-- lemTDL-lookup-filter-filter ((.t , a) ∷ δas) p t | refl = {!!}
-- lemTDL-lookup-filter-filter ((δ , a) ∷ δas) p t  | less q = {!!}
-- lemTDL-lookup-filter-filter ((δ , a) ∷ δas) p t  | more q = {!!}

postulate lemTDL-lookup-filter : {A : Set} → (δas₁ δas₂ : TDList A) → (p : A → Bool) → (t₁ t₂ : Time⁺) → lookupTDL⁺ δas₁ t₁ ≡ lookupTDL⁺ δas₂ t₂ → lookupTDL⁺ (filterTDL p δas₁) t₁ ≡ lookupTDL⁺ (filterTDL p δas₂) t₂
-- lemTDL-lookup-filter [] [] p t₁ t₂ refl = refl
-- lemTDL-lookup-filter [] ((δ₂ , a₂) ∷ δas₂) p t₁ t₂ eq = {!!}
-- lemTDL-lookup-filter ((δ₁ , a₁) ∷ δas₁) [] p t₁ t₂ eq = {!!}
-- lemTDL-lookup-filter ((δ₁ , a₁) ∷ δas₁) ((δ₂ , a₂) ∷ δas₂) p t₁ t₂ eq with p a₁ | p a₂
-- ... | false | false = {!!}
-- ... | true  | false = {!!}
-- ... | false | true  = {!!}
-- ... | true  | true  = {!!}

lemTDL-isNothing-lookup-map : {A B : Set} → (δas : TDList A) → (f : A → B) → (t : Time⁺) → IsNothing (lookupTDL⁺ δas t) → IsNothing (lookupTDL⁺ (map (second f) δas) t) 
lemTDL-isNothing-lookup-map [] f t spell ()
lemTDL-isNothing-lookup-map ((δ , a) ∷ δas) f t spell p with compareℜ⁺ δ t
lemTDL-isNothing-lookup-map ((.t , a) ∷ δas) f t spell p | refl = spell p
lemTDL-isNothing-lookup-map ((δ , a) ∷ δas) f t spell p | less lt = lemTDL-isNothing-lookup-map δas f (ℜ⁺-minus t δ lt) spell p
lemTDL-isNothing-lookup-map ((δ , a) ∷ δas) f t spell () | more _

----------------------------------------------------------------------------------------------------------
