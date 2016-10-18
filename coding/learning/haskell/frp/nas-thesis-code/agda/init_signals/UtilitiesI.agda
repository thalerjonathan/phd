{-# OPTIONS --type-in-type --no-termination-check #-}

module UtilitiesI where

open import NeilPrelude
open import Maybe
open import List
open import RealTime
open import SigVecsI
open import TimeDeltaList
open import TimeDeltaListProps
open import StrictTotalOrder

------------------------------------------------------

lastChangeTime : {A : Set} → ChangeList A → Time
lastChangeTime = sumTDL

lookupCP : {A : Set} → ChangePrefix A → Time → Maybe A
lookupCP cp t = lookupTDL (cp t) t

occ : {A : Set} → SigVec (E A) → Time → Maybe A 
occ (ma , _)  O  = ma
occ (_  , cp) t  = lookupCP cp t

change : {A : Set} → SigVec (S A) → Time → Maybe A 
change (a , _)  O  = just a 
change (_ , cp) t  = lookupCP cp t

------------------------------------------------------

val : {A : Set} → SigVec (S A) → Time → A
val (a₀ , cp) t with reverse (cp t)
... | []             = a₀
... | (_ , a₁) ∷ _   = a₁

leftLimit : {A : Set} → SigVec (S A) → Time⁺ → A
leftLimit (a₀ , cp) t with reverse (takeExcl⁺ t (cp (t >0)))
... | []             = a₀
... | (_ , a₁) ∷ _   = a₁

------------------------------------------------------

mapCL : {A B : Set} → (A → B) → ChangeList A → ChangeList B
mapCL = mapTDL

mapCP : {A B : Set} → (A → B) → ChangePrefix A → ChangePrefix B
mapCP = result ∘ mapCL

mapCPtime : {A B : Set} → (Time → A → B) → ChangePrefix A → ChangePrefix B
mapCPtime = result ∘ mapTDLtime

mapCPtime⁺ : {A B : Set} → (Time⁺ → A → B) → ChangePrefix A → ChangePrefix B
mapCPtime⁺ = result ∘ mapTDLtime⁺

mapC : {i : Init} → {A B : Set} → (A → B) → SigVec (C i A) → SigVec (C i B)
mapC {ini} = result
mapC {uni} = result

mapE : {A B : Set} → (A → B) → SigVec (E A) → SigVec (E B)
mapE f (ma , cp) = (maybeMap f ma , mapCP f cp)

mapS : {A B : Set} → (A → B) → SigVec (S A) → SigVec (S B)
mapS f (a , cp) = (f a , mapCP f cp)

mapC2 : {i₁ i₂ : Init} → {A B Z : Set} → (A → B → Z) → SigVec (C i₁ A) → SigVec (C i₂ B) → SigVec (C (i₁ ⊓ i₂) Z)
mapC2 {ini} {ini} f s1 s2 = λ t → f (s1 t) (s2 t)
mapC2 {ini} {uni} f s1 s2 = λ t → f (s1 (t >0)) (s2 t)
mapC2 {uni} {ini} f s1 s2 = λ t → f (s1 t) (s2 (t >0))
mapC2 {uni} {uni} f s1 s2 = λ t → f (s1 t) (s2 t)

mapS2 : {A B Z : Set} → (A → B → Z) → SigVec (S A) → SigVec (S B) → SigVec (S Z)
mapS2 {A} {B} {Z} f (a , cpa) (b , cpb) = (f a b , λ t → mergeS a b (cpa t) (cpb t))
  where
        mergeS : A → B → ChangeList A → ChangeList B → ChangeList Z
        mergeS a₀ b₀ [] δbs = mapCL (f a₀) δbs
        mergeS a₀ b₀ δas [] = mapCL (flip f b₀) δas
        mergeS a₀ b₀ ((δ₁  , a₁) ∷ δas) ((δ₂ , b₁) ∷ δbs) with compareℜ⁺ δ₁ δ₂
        mergeS a₀ b₀ ((.δ₂ , a₁) ∷ δas) ((δ₂ , b₁) ∷ δbs) | refl   = (δ₂ , f a₁ b₁) ∷ mergeS a₁ b₁ δas δbs
        mergeS a₀ b₀ ((δ₁  , a₁) ∷ δas) ((δ₂ , b₁) ∷ δbs) | less p = (δ₁ , f a₁ b₀) ∷ mergeS a₁ b₀ δas (((δ₂ ⁺-⁺ δ₁) p , b₁) ∷ δbs)
        mergeS a₀ b₀ ((δ₁  , a₁) ∷ δas) ((δ₂ , b₁) ∷ δbs) | more p = (δ₂ , f a₀ b₁) ∷ mergeS a₀ b₁ (((δ₁ ⁺-⁺ δ₂) p , a₁) ∷ δas) δbs


mergeE2 : {A B Z : Set} → (A → Z) → (B → Z) → (A → B → Z) → SigVec (E A) → SigVec (E B) → SigVec (E Z)
mergeE2 {A} {B} {Z} fa fb fab (ma , cpa) (mb , cpb) = (maybeMerge fa fb fab ma mb , λ t → mergeCL (cpa t) (cpb t))
  where
        mergeCL : ChangeList A → ChangeList B → ChangeList Z
        mergeCL [] δbs = mapCL fb δbs
        mergeCL δas [] = mapCL fa δas
        mergeCL ((δ₁  , a) ∷ δas) ((δ₂ , b) ∷ δbs) with compareℜ⁺ δ₁ δ₂
        mergeCL ((.δ₂ , a) ∷ δas) ((δ₂ , b) ∷ δbs) | refl   = (δ₂ , fab a b) ∷ mergeCL δas δbs
        mergeCL ((δ₁  , a) ∷ δas) ((δ₂ , b) ∷ δbs) | less p = (δ₁ , fa a) ∷ mergeCL δas (((δ₂ ⁺-⁺ δ₁) p , b) ∷ δbs)
        mergeCL ((δ₁  , a) ∷ δas) ((δ₂ , b) ∷ δbs) | more p = (δ₂ , fb b) ∷ mergeCL (((δ₁ ⁺-⁺ δ₂) p , a) ∷ δas) δbs

joinE2 : {A B Z : Set} → (A → B → Z) → SigVec (E A) → SigVec (E B) → SigVec (E Z)
joinE2 {A} {B} {Z} f (ma , cpa) (mb , cpb) = (maybeMap2 f ma mb , λ t → joinCL O (cpa t) (cpb t))
  where
        joinCL : Time → ChangeList A → ChangeList B → ChangeList Z
        joinCL _ [] _  = []
        joinCL _ _  [] = []
        joinCL d ((δ₁ , a) ∷ δas) ((δ₂ , b) ∷ δbs) with compareℜ⁺ δ₁ δ₂
        joinCL d ((.δ₂ , a) ∷ δas) ((δ₂ , b) ∷ δbs) | refl   = (d ₀+⁺ δ₂ , f a b) ∷ joinCL O δas δbs
        joinCL d ((δ₁ , a) ∷ δas)  ((δ₂ , b) ∷ δbs) | less p = joinCL ((d ₀+⁺ δ₁) >0) δas (((δ₂ ⁺-⁺ δ₁) p , b) ∷ δbs)
        joinCL d ((δ₁ , a) ∷ δas)  ((δ₂ , b) ∷ δbs) | more p = joinCL ((d ₀+⁺ δ₂) >0) (((δ₁ ⁺-⁺ δ₂) p , a) ∷ δas) δbs

mapCE : {i : Init} → {A B Z : Set} → (A → B → Z) → SigVec (C i A) → SigVec (E B) → SigVec (E Z)
mapCE {ini} f s (mb , cp) = maybeMap (f (s O)) mb , mapCPtime (f ∘ s) cp
mapCE {uni} f s (mb , cp) = nothing , mapCPtime⁺ (f ∘ s) cp

mapSE : {A B Z : Set} → (A → B → Z) → SigVec (S A) → SigVec (E B) → SigVec (E Z)
mapSE f s (mb , cp) = (maybeMap (f (val s O)) mb , mapCPtime (f ∘ val s) cp)

------------------------------------------------------

-- When we delay a change prefix we may have an initial value to include

delayCP : {A : Set} → Maybe A → Time⁺ → ChangePrefix A → ChangePrefix A
delayCP ma d cp t with compareGeqℜ₀ t (d >0)
delayCP ma d cp t | less p = []
delayCP ma d cp t | geq  p = delayTDLinit ma d (cp (ℜ₀⁺₀-minus t d p))

advanceCP : {A : Set} → Time → ChangePrefix A → ChangePrefix A
advanceCP d cp t = advanceTDL d (cp (t ₀+₀ d))

advance : {as : SVDesc} → Time → SigVec as → SigVec as
advance {C ini _} d s = λ t → s (t ₀+₀ d)
advance {C uni _} d s = λ t → s (t ⁺+₀ d)
advance {S _}     d s = val s d , advanceCP d (snd s)
advance {E _}     d s = occ  s d , advanceCP d (snd s)
advance {as , bs} d (s₁ , s₂) = (advance {as} d s₁ , advance {bs} d s₂)


------------------------------------------------------

-- takeExclEnd ignores a change at the sample time, and also returns the remaining time after the last change (which thus has to be positive) 

lem-sumCLexcl : {A : Set} → (t : Time⁺) → (cp : ChangePrefix A) → sumTDL (takeExcl⁺ t (cp (t >0))) <ℜ₀ (t >0)
lem-sumCLexcl t cp = lemTDL-sumTakeExcl⁺ t (cp (t >0))

takeExclEnd : {A : Set} → ChangePrefix A → Time⁺ → ChangeList A × Δt
takeExclEnd cp t =  let δas = takeExcl⁺ t (cp (t >0))
                    in δas , ℜ⁺₀⁺-minus t (lastChangeTime δas) (lem-sumCLexcl t cp)

-- takeExclEnd takes a change list upto but excluding the specified time
-- it also returns a time delta which is the time remaining after the (resultant) final change and the specified time

-- firstOcc returns the first occurrence upto the specified time.  If the first occurrence is after this time it will return nothing

fstOcc : {A : Set} → SigVec (E A) → Time → Maybe (Time × A)
fstOcc (just a  , _) _ = just (O , a)
fstOcc (nothing , cp) t with cp t
... | []          = nothing
... | (δ , a) ∷ _ = just (δ >0 , a)

spliceC : {i : Init} → {A : Set} → SigVec (C i A) → SigVec (C ini A) → EventTime → SigVec (C i A)
spliceC {ini} s₁ s₂ te = λ t → ifℜ₀ t ≥ te thengeq (λ p → s₂ (ℜ₀-minus t te p)) elseless λ _ → s₁ t
spliceC {uni} s₁ s₂ te = λ t → ifℜ₀ t >0 ≥ te thengeq (λ p → s₂ (ℜ₀-minus (t >0) te p)) elseless λ _ → s₁ t

spliceS : {A : Set} → SigVec (S A) → SigVec (S A) → EventTime → SigVec (S A)
spliceS (a₁ , cp₁) (a₂ , cp₂) O = (a₂ , cp₂)
spliceS (a₁ , cp₁) (a₂ , cp₂) (te >0) with takeExclEnd cp₁ te
... | δas₁ , δ = a₁ , λ t → ifℜ₀ t ≥ te >0
                             thengeq (λ p → δas₁ ++ (δ , a₂) ∷ cp₂ (ℜ₀⁺₀-minus t te p))
                             elseless (λ _ → cp₁ t)

spliceE : {A : Set} → SigVec (E A) → SigVec (E A) → EventTime → SigVec (E A)
spliceE (ma₁ , cp₁) (ma₂ , cp₂) O       = (ma₂ , cp₂)
spliceE (ma₁ , cp₁) (ma₂ , cp₂) (te >0) with takeExclEnd cp₁ te
... | (δas₁ , δ) = ma₁ , λ t → ifℜ₀ t ≥ te >0
                                thengeq (λ p → δas₁ ++ delayTDLinit ma₂ δ (cp₂ (ℜ₀⁺₀-minus t te p)))
                                elseless (λ _ → cp₁ t)

splice : {as : SVDesc} → SigVec as → SigVec (iniSV as) → EventTime → SigVec as
splice {C i _}    s₁           s₂           te  = spliceC {i} s₁ s₂ te
splice {S _}      s₁           s₂           te  = spliceS s₁ s₂ te
splice {E _}      s₁           s₂           te  = spliceE s₁ s₂ te
splice {as , bs}  (sa₁ , sb₁)  (sa₂ , sb₂)  te  = splice {as} sa₁ sa₂ te , splice {bs} sb₁ sb₂ te

------------------------------------------------------

withTime : {as : SVDesc} → (SampleTime → SigVec as) → SigVec as
withTime {C ini _} f = λ t → f t t
withTime {C uni _} f = λ t → f (t >0) t
withTime {E _}     f = (fst (f O) , λ t → snd (f t) t)
withTime {S _}     f = (fst (f O) , λ t → snd (f t) t)
withTime {as , bs} f = (withTime {as} (fst ∘ f) , withTime {bs} (snd ∘ f))

------------------------------------------------------
