{-# OPTIONS --type-in-type  #-}

open import NeilPrelude
open import When
open import List
open import ListProps
open import RealTime
open import Maybe

module TimeDeltaWhenList where

-----------------------------------------------------

TimeDeltaWhenList : Set → Set
TimeDeltaWhenList A = List (Δt × When A)

WList = TimeDeltaWhenList

mapWL : {A B : Set} → (A → B) → WList A → WList B
mapWL = map ∘ second ∘ mapWhen

-----------------------------------------------------

-- delayWL increases the first time delta

delayWL : {A : Set} → Time⁺ → WList A → WList A
delayWL t [] = []
delayWL t ((d & a) ∷ das) = (t ⁺+⁺ d & a) ∷ das

-- advanceWL decreases the first time delta (the point of observation advances)
-- any time deltas we reach are discarded

advanceWL : {A : Set} → Time⁺ → WList A → WList A
advanceWL t [] = []
advanceWL t ((d & wa) ∷ das) with compareℜ⁺ d t
advanceWL t ((.t & wa) ∷ das) | refl   = das
advanceWL t ((d & wa)  ∷ das) | less p = advanceWL ((t ⁺-⁺ d) p) das
advanceWL t ((d & wa)  ∷ das) | more p = ((d ⁺-⁺ t) p & wa) ∷ das

sumTDs : {A : Set} → WList A → Time
sumTDs = sumℜ⁺ ∘ map fst

lookupTimePoint : {A : Set} → WList A → Time⁺ → Maybe (When A)
lookupTimePoint [] t = nothing
lookupTimePoint ((d & wa) ∷ as) t with compareℜ⁺ d t
lookupTimePoint ((d & wa) ∷ as) .d | refl   = just wa
lookupTimePoint ((d & wa) ∷ as) t  | more p = nothing
lookupTimePoint ((d & wa) ∷ as) t  | less p = lookupTimePoint as ((t ⁺-⁺ d) p)

-----------------------------------------------------

-- "takeIncl" takes all values upto and including the given time, and discards the rest
-- A "Now" change at the given time is included, a "Soon" change is not.

import OrdCompare
open OrdCompare _<ℜ⁺_ <ℜ⁺-irreflexive <ℜ⁺-asym <ℜ⁺-trans compareℜ⁺

takeIncl : {A : Set} → Time⁺ → WList A → WList A
takeIncl _ [] = []
takeIncl t  ((d & wa) ∷ das) with compareℜ⁺ d t
takeIncl t  ((d & wa) ∷ das) | more p = []
takeIncl t  ((d & wa) ∷ das) | less p = (d & wa) ∷ takeIncl ((t ⁺-⁺ d) p) das
takeIncl .d ((d & Now a)       ∷ das) | refl = (d & Now a) ∷ []
takeIncl .d ((d & Soon a)      ∷ das) | refl = []
takeIncl .d ((d & NowSoon a b) ∷ das) | refl = (d & Now a) ∷ []

takeExcl : {A : Set} → Time⁺ → WList A → WList A
takeExcl _ [] = []
takeExcl t  ((d & wa) ∷ das) with compareGeq d t
takeExcl t  ((d & wa) ∷ das) | geq  p = []
takeExcl t  ((d & wa) ∷ das) | less p = (d & wa) ∷ takeExcl ((t ⁺-⁺ d) p) das

dropIncl : {A : Set} → Time⁺ → WList A → WList A
dropIncl _ [] = []
dropIncl t  ((d & wa) ∷ das) with compareℜ⁺ d t
dropIncl t  ((d & wa) ∷ das) | more p = (d & wa) ∷ das
dropIncl t  ((d & wa) ∷ das) | less p = delayWL d (dropIncl ((t ⁺-⁺ d) p) das)
dropIncl .d ((d & Now a) ∷ das)       | refl = delayWL d das
dropIncl .d ((d & Soon a) ∷ das)      | refl = (d & Soon a) ∷ das
dropIncl .d ((d & NowSoon a b) ∷ das) | refl = (d & Soon b) ∷ das

dropExcl : {A : Set} → Time⁺ → WList A → WList A
dropExcl _ [] = []
dropExcl t  ((d & a) ∷ das) with compareGeq d t
dropExcl t  ((d & a) ∷ das) | geq  p = (d & a) ∷ das
dropExcl t  ((d & a) ∷ das) | less p = delayWL d (dropExcl ((t ⁺-⁺ d) p) das)

-----------------------------------------------------

lemWL-delayEmpty : {A : Set} → {t : Time⁺} → (das : WList A) → delayWL t das ≡ [] → das ≡ []
lemWL-delayEmpty [] p = p
lemWL-delayEmpty ((_ & _) ∷ _) ()

lemWL-delayEmptyResp : {A : Set} → {t : Time⁺} → (das : WList A) → das ≡ [] → delayWL t das ≡ []
lemWL-delayEmptyResp .[] refl = refl

lemWL-dropEmpty→takeAll : {A : Set} → (t : Time⁺) → (das : WList A) → dropIncl t das ≡ [] → takeIncl t das ≡ das
lemWL-dropEmpty→takeAll t [] p = refl
lemWL-dropEmpty→takeAll t ((d & a) ∷ das) p with compareℜ⁺ d t
lemWL-dropEmpty→takeAll t ((d & a) ∷ das) p | less lt = tail-resp (lemWL-dropEmpty→takeAll ((t ⁺-⁺ d) lt) das (lemWL-delayEmpty _ p))
lemWL-dropEmpty→takeAll t ((d & a) ∷ das) () | more _
lemWL-dropEmpty→takeAll t ((.t & Now a) ∷ das) p | refl = tail-resp (comm (lemWL-delayEmpty das p))
lemWL-dropEmpty→takeAll t ((.t & Soon a) ∷ das) () | refl
lemWL-dropEmpty→takeAll t ((.t & NowSoon a b) ∷ das) () | refl

lemWL-takeDropEmpty : {A : Set} → (t : Time⁺) → (das : WList A) → dropIncl t das ≡ [] → takeIncl t das ≡ [] → das ≡ []
lemWL-takeDropEmpty t das p = trans (takeIncl t das) (comm (lemWL-dropEmpty→takeAll t das p))

lemWL-safelastEqTakeAll : {A : Set} → (t : Time⁺) → (da da₀ : Δt × When A) → (das : WList A) → dropIncl t (da ∷ das) ≡ [] → safelast da das ≡ safelast da₀ (takeIncl t (da ∷ das))
lemWL-safelastEqTakeAll t da da₀ das p with lemWL-dropEmpty→takeAll t (da ∷ das) p
... | q = rewriteRHS (resp (safelast da₀) (comm q)) refl

lemWL-sndsafelastEqTakeAll : {A : Set} → (t : Time⁺) → (da da₀ : Δt × When A) → (das : WList A) → dropIncl t (da ∷ das) ≡ [] → snd (safelast da das) ≡ snd (safelast da₀ (takeIncl t (da ∷ das)))
lemWL-sndsafelastEqTakeAll t da da₀ das p = resp snd (lemWL-safelastEqTakeAll t da da₀ das p)

lemWL-takeDropSafeLast : {A : Set} → (t : Time⁺) → (da₁ da₂ : Δt × When A) → (das₁ das₂ : WList A) →
                         dropIncl t (da₂ ∷ das₂) ≡ [] → da₁ ∷ das₁ ≡ takeIncl t (da₂ ∷ das₂) → snd (safelast da₂ das₂) ≡ snd (safelast da₁ das₁)
lemWL-takeDropSafeLast t (d₁ & wa₁) (d₂ & wa₂) das₁ das₂ p q with compareℜ⁺ d₂ t
lemWL-takeDropSafeLast t (d₁ & wa₁) (d₂ & wa₂) das₁ das₂ () () | more _
lemWL-takeDropSafeLast t (.d₂ & .wa₂) (d₂ & wa₂) .[] [] p refl | less _ = refl
lemWL-takeDropSafeLast t (.d₂ & .wa₂) (d₂ & wa₂) ._ (da₂ ∷ das₂) p refl | less lt = lemWL-sndsafelastEqTakeAll ((t ⁺-⁺ d₂) lt) da₂ (d₂ & wa₂) das₂ (lemWL-delayEmpty _ p)
lemWL-takeDropSafeLast t (d₁ & wa₁) (.t & Soon a) das₁ das₂ () () | refl
lemWL-takeDropSafeLast t (d₁ & wa₁) (.t & NowSoon a b) das₁ das₂ () q | refl
lemWL-takeDropSafeLast t (.t & .(Now a)) (.t & Now a) .[] das₂ p refl | refl with lemWL-delayEmpty das₂ p
lemWL-takeDropSafeLast t (.t & .(Now a)) (.t & Now a) .[] .[] p refl | refl | refl = refl

-----------------------------------------------------

-- NOT TRUE WITH WHEN

-- lemWL-dropInclSum→Empty : {A : Set} → (t : Time⁺) → (das : WList A) → sumTDs das ≤ℜ (t >0) → dropIncl t das ≡ []
-- lemWL-dropInclSum→Empty t [] p = refl
-- lemWL-dropInclSum→Empty t ((d & wa) ∷ das) p with lem-leq-inj>0 p | compareℜ⁺ d t
-- lemWL-dropInclSum→Empty t ((d & wa) ∷ das) p | p' | more q = absurd (<≤ℜ⁺-antisym (≤<ℜ⁺-trans p' q) (lem-⁺+-increasing-comm (sumℜ⁺ (map fst das))))
-- lemWL-dropInclSum→Empty t ((d & wa) ∷ das) p | p' | less q = lemWL-delayEmptyResp _ (lemWL-dropInclSum→Empty ((t ⁺-⁺ d) q) das (lem-<ℜ⁺-plusminusinverse1 q p'))
-- lemWL-dropInclSum→Empty t ((.t & wa) ∷ da ∷ das) p | p' | refl = absurd (<≤ℜ⁺-antisym lem-⁺+⁺-increasing-comm p')
-- lemWL-dropInclSum→Empty t ((.t & Now a) ∷ []) p | p' | refl = refl
-- lemWL-dropInclSum→Empty t ((.t & Soon a) ∷ []) p | p' | refl = {!!}
-- lemWL-dropInclSum→Empty t ((.t & NowSoon a b) ∷ []) p | p' | refl = {!!}


lemWL-lookupFirstDelta : {A : Set} → {wa : When A} → {das : WList A} → (d : Δt) → just wa ≡ lookupTimePoint ((d & wa) ∷ das) d
lemWL-lookupFirstDelta d with compareℜ⁺ d d
lemWL-lookupFirstDelta d | refl   = refl
lemWL-lookupFirstDelta d | less p = absurd (<ℜ⁺-irreflexive p)
lemWL-lookupFirstDelta d | more p = absurd (<ℜ⁺-irreflexive p)

lemWL-lookupFirstDelta2 : {A : Set} → {wa : When A} → {das : WList A} → (d : Δt) → IsJust (lookupTimePoint ((d & wa) ∷ das) d)
lemWL-lookupFirstDelta2 d with compareℜ⁺ d d
lemWL-lookupFirstDelta2 d | refl = _
lemWL-lookupFirstDelta2 d | less p = absurd (<ℜ⁺-irreflexive p)
lemWL-lookupFirstDelta2 d | more p = absurd (<ℜ⁺-irreflexive p)

-- This is proveable but hard work
-- postulate lemWL-dropInclHeadmore : {A : Set} → (t : Time⁺) → (das : WList A) → (dropIncl t das ≡ [])  ⊎  Σ Δt (λ d → (t <ℜ⁺ d) × IsJust (lookupTimePoint das d))
-- lemWL-dropInclHeadmore t [] = inl refl
-- lemWL-dropInclHeadmore t ((d & a) ∷ das) with compareℜ⁺ d t
-- lemWL-dropInclHeadmore t ((.t & a) ∷ das) | refl   = {!!}
-- lemWL-dropInclHeadmore t ((d & a) ∷ das)  | less p = {!!}
-- lemWL-dropInclHeadmore t ((d & a) ∷ das)  | more p = inr (d & lemWL-lookupFirstDelta2 d & p)


-----------------------------------------------------

lemWL-lookupDelay : {A : Set} → (das : WList A) → (d t : Time⁺) → (q : d <ℜ⁺ t)→ lookupTimePoint (delayWL d das) t ≡ lookupTimePoint das ((t ⁺-⁺ d) q)
lemWL-lookupDelay [] d t q = refl
lemWL-lookupDelay ((d₂ & a) ∷ das) d₁ t q with compareℜ⁺ (d₁ ⁺+⁺ d₂) t
lemWL-lookupDelay ((d₂ & a) ∷ das) d₁ ._ q | refl = trans (lookupTimePoint ((d₂ & a) ∷ das) d₂) (lemWL-lookupFirstDelta d₂) (resp (lookupTimePoint ((d₂ & a) ∷ das)) (comm (lem-plusminusInverse2 q)))
lemWL-lookupDelay ((d₂ & a) ∷ das) d₁ t q | less r with compareℜ⁺ d₂ ((t ⁺-⁺ d₁) q)
lemWL-lookupDelay ((._ & a) ∷ das) d₁ t q | less r | refl   = absurd (<ℜ⁺-irreflexive (<ℜ⁺-substL (lem-plusminusInverse1 q) r))
lemWL-lookupDelay ((d₂ & a) ∷ das) d₁ t q | less r | less s = resp (lookupTimePoint das) (lem-plusminusdist r q s)
lemWL-lookupDelay ((d₂ & a) ∷ das) d₁ t q | less r | more s = absurd (<ℜ⁺-asym r (lem-<ℜ⁺-plusminusinverse2 q s))
lemWL-lookupDelay ((d₂ & a) ∷ das) d₁ t q | more r with compareℜ⁺ d₂ ((t ⁺-⁺ d₁) q)
lemWL-lookupDelay ((._ & a) ∷ das) d₁ t q | more r | refl   = absurd (<ℜ⁺-irreflexive (<ℜ⁺-substR (lem-plusminusInverse1 q) r))
lemWL-lookupDelay ((d₂ & a) ∷ das) d₁ t q | more r | less s = absurd (<ℜ⁺-asym r (lem-<ℜ⁺-plusminusinverse3 q s))
lemWL-lookupDelay ((d₂ & a) ∷ das) d₁ t q | more _ | more _ = refl

lemWL-lookupLessThanDelay : {A : Set} → (das : WList A) → (t₁ t₂ : Time⁺) → t₁ <ℜ⁺ t₂ → IsNothing (lookupTimePoint (delayWL t₂ das) t₁)
lemWL-lookupLessThanDelay [] t₁ t₂ lt p = p
lemWL-lookupLessThanDelay ((d & a) ∷ das) t₁ t₂ lt p with compareℜ⁺ (t₂ ⁺+⁺ d) t₁
lemWL-lookupLessThanDelay ((d & a) ∷ das) .(t₂ ⁺+⁺ d) t₂ lt p | refl = <ℜ⁺-asym lt lem-⁺+⁺-increasing-comm
lemWL-lookupLessThanDelay ((d & a) ∷ das) t₁ t₂ lt p | less q = <ℜ⁺-asym q (<ℜ⁺-trans lt lem-⁺+⁺-increasing-comm)
lemWL-lookupLessThanDelay ((d & a) ∷ das) t₁ t₂ lt p | more q = p

lemWL-lookupLessThenDelta : {A : Set} → {wa : When A} → {das : WList A} → (t d : Time⁺) → t <ℜ⁺ d → IsNothing (lookupTimePoint ((d & wa) ∷ das) t)
lemWL-lookupLessThenDelta t d lt p with compareℜ⁺ d t
lemWL-lookupLessThenDelta t .t lt p | refl   = <ℜ⁺-irreflexive lt
lemWL-lookupLessThenDelta t d lt p  | less gt = <ℜ⁺-asym lt gt
lemWL-lookupLessThenDelta t d lt p  | more gt = p

lemWL-lookupDelayed→Nothing : {A : Set} → (t : Time⁺) → (das : WList A) → IsNothing (lookupTimePoint (delayWL t das) t)
lemWL-lookupDelayed→Nothing t [] p = p
lemWL-lookupDelayed→Nothing t ((d & a) ∷ das) p = lemWL-lookupLessThenDelta t _ lem-⁺+⁺-increasing-comm p


-- NOT TRUE FOR WHEN

-- lemWL-lookupDropped→Nothing1 : {A : Set} → (t : Time⁺) → (das : WList A) → IsNothing (lookupTimePoint (dropIncl t das) t)
-- lemWL-lookupDropped→Nothing1 t [] () 
-- lemWL-lookupDropped→Nothing1 t ((d & a) ∷ das) p with compareℜ⁺ d t
-- lemWL-lookupDropped→Nothing1 t ((.t & a) ∷ das) p | refl   = lemWL-lookupDelayed→Nothing t das p
-- lemWL-lookupDropped→Nothing1 t ((d & a) ∷ das) p  | more q = lemWL-lookupLessThenDelta t d q p
-- lemWL-lookupDropped→Nothing1 t ((d & a) ∷ das) p  | less q = lemWL-lookupDropped→Nothing1 ((t ⁺-⁺ d) q) das (subst (resp IsJust (lemWL-lookupDelay (dropIncl ((t ⁺-⁺ d) q) das) d t q)) p)

lemWL-lookupDropped→Nothing2 : {A : Set} → (t₁ t₂ : Time⁺) → t₁ <ℜ⁺ t₂ → (das : WList A) → IsNothing (lookupTimePoint (dropIncl t₂ das) t₁)
lemWL-lookupDropped→Nothing2 t₁ t₂ lt [] p = p
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((d & a) ∷ das) p with compareℜ⁺ d t₂
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((d & a)   ∷ das) p | more q = lemWL-lookupLessThenDelta t₁ d (<ℜ⁺-trans lt q) p
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((d & a)   ∷ das) p | less q with compareℜ⁺ d t₁
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((.t₁ & a) ∷ das) p | less q | refl = lemWL-lookupDelayed→Nothing t₁ (dropIncl ((t₂ ⁺-⁺ t₁) q) das) p
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((d & a) ∷ das) p | less q | less r = lemWL-lookupDropped→Nothing2 ((t₁ ⁺-⁺ d) r) ((t₂ ⁺-⁺ d) q) (lem-<ℜ⁺minusCanc r q lt) das (subst (resp IsJust (lemWL-lookupDelay (dropIncl ((t₂ ⁺-⁺ d) q) das) d t₁ r)) p)
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((d & a) ∷ das) p | less q | more r = lemWL-lookupLessThanDelay (dropIncl ((t₂ ⁺-⁺ d) q) das) t₁ d r p
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((.t₂ & Now a) ∷ das) p | refl = lemWL-lookupLessThanDelay das t₁ t₂ lt p
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((.t₂ & Soon a) ∷ das) p | refl with compareℜ⁺ t₂ t₁
lemWL-lookupDropped→Nothing2 .t₂ t₂ lt ((.t₂ & Soon a) ∷ das) p | refl | refl = absurd (<ℜ⁺-irreflexive lt)
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((.t₂ & Soon a) ∷ das) p | refl | less q = absurd (<ℜ⁺-asym lt q)
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((.t₂ & Soon a) ∷ das) () | refl | more q
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((.t₂ & NowSoon a b) ∷ das) p | refl with compareℜ⁺ t₂ t₁
lemWL-lookupDropped→Nothing2 .t₂ t₂ lt ((.t₂ & NowSoon a b) ∷ das) p | refl | refl = absurd (<ℜ⁺-irreflexive lt)
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((.t₂ & NowSoon a b) ∷ das) p | refl | less q = absurd (<ℜ⁺-asym lt q)
lemWL-lookupDropped→Nothing2 t₁ t₂ lt ((.t₂ & NowSoon a b) ∷ das) () | refl | more q

{-
lemWL-lookupDropped→Nothing : {A : Set} → (t₁ t₂ : Time⁺) → t₁ ≤ℜ⁺ t₂ → (das : TDList A) → IsNothing (lookupTimePoint (dropIncl t₂ das) t₁)
lemWL-lookupDropped→Nothing t₁ t₂ (inl lt)    = lemWL-lookupDropped→Nothing2 t₁ t₂ lt
lemWL-lookupDropped→Nothing t₁ .t₁ (inr refl) = lemWL-lookupDropped→Nothing1 t₁

-----------------------------------------------------

lemWL-lookupTaken : {A : Set} → (t : Time⁺) → (das : WList A) → lookupTimePoint (takeIncl t das) t ≡ lookupTimePoint das t
lemWL-lookupTaken t [] = refl
lemWL-lookupTaken t ((d & a) ∷ das) with  (compareℜ⁺ d t)
lemWL-lookupTaken t ((.t & a) ∷ das) |  refl     = comm (lemWL-lookupFirstDelta t)
lemWL-lookupTaken t ((d & a) ∷ das)  |  (more p) = refl
lemWL-lookupTaken t ((d & a) ∷ das)  |  (less p) with compareℜ⁺ d t
lemWL-lookupTaken t ((.t & a) ∷ das) | less p | refl   = absurd (<ℜ⁺-irreflexive p)
lemWL-lookupTaken t ((d & a)  ∷ das) | less p | more q = absurd (<ℜ⁺-asym p q)
lemWL-lookupTaken t ((d & a)  ∷ das) | less p | less q = trans (lookupTimePoint (takeIncl ((t ⁺-⁺ d) p) das) ((t ⁺-⁺ d) p))
                                                                (resp (lookupTimePoint (takeIncl ((t ⁺-⁺ d) p) das)) (lem-minus-proofirrelevence q p))
                                                                (lemWL-lookupTaken ((t ⁺-⁺ d) p) das)

-----------------------------------------------------
-}