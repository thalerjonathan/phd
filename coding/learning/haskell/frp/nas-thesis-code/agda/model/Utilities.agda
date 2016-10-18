{-# OPTIONS --type-in-type --no-termination-check #-}

module Utilities where

open import NeilPrelude
open import Maybe
open import List
open import RealTime
open import SigVecs
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


mapC : {A B : Set} → (A → B) → SigVec (C A) → SigVec (C B)
mapC =  result

mapE : {A B : Set} → (A → B) → SigVec (E A) → SigVec (E B)
mapE f (ma , cp) = (maybeMap f ma , mapCP f cp)

mapS : {A B : Set} → (A → B) → SigVec (S A) → SigVec (S B)
mapS f (a , cp) = (f a , mapCP f cp)

mapC2 : {A B Z : Set} → (A → B → Z) → SigVec (C A) → SigVec (C B) → SigVec (C Z)
mapC2 f s1 s2 t = f (s1 t) (s2 t)

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

mapCE : {A B Z : Set} → (A → B → Z) → SigVec (C A) → SigVec (E B) → SigVec (E Z)
mapCE {A} {B} {Z} f s (mb , cp) = (maybeMap (f (s O)) mb , λ t → mergeCE O (cp t))
  where
        mergeCE : Time → ChangeList B → ChangeList Z
        mergeCE d []               = []
        mergeCE d ((δ , b) ∷ δbs)  = let d' = d ₀+⁺ δ in (d' , f (s (d' >0)) b) ∷ mergeCE (d' >0) δbs 

mapSE : {A B Z : Set} → (A → B → Z) → SigVec (S A) → SigVec (E B) → SigVec (E Z)
mapSE {A} {B} {Z} f s (mb , cp) = (maybeMap (f (val s O)) mb , λ t → mergeSE O (cp t))
  where
        mergeSE : Time → ChangeList B → ChangeList Z
        mergeSE d []               = []
        mergeSE d ((δ , b) ∷ δbs)  = let d' = d ₀+⁺ δ in (d' , f (val s (d' >0)) b) ∷ mergeSE (d' >0) δbs 

------------------------------------------------------

-- When we delay a change prefix we may have an initial value to include

delayCP : {A : Set} → Maybe A → Time⁺ → ChangePrefix A → ChangePrefix A
delayCP ma d cp t with compareGeqℜ₀ t (d >0)
delayCP ma d cp t | less p = []
delayCP ma d cp t | geq  p = delayTDLinit ma d (cp (ℜ₀⁺₀-minus t d p))

advanceCP : {A : Set} → Time → ChangePrefix A → ChangePrefix A
advanceCP d cp t = advanceTDL d (cp (t ₀+₀ d))

advance : {as : SVDesc} → Time → SigVec as → SigVec as
advance {C _}     d s = λ t → s (t ₀+₀ d)
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

lem-sumCLincl : {A : Set} → (t : Time) → (cp : ChangePrefix A) → sumTDL (takeIncl t (cp t)) ≤ℜ₀ t
lem-sumCLincl t cp = lemTDL-sumTakeIncl t (cp t)

takeInclEnd : {A : Set} → ChangePrefix A → Time → ChangeList A × Time
takeInclEnd cp t =  let δas = takeIncl t (cp t)
                    in δas , ℜ₀-minus t (lastChangeTime δas) (lem-sumCLincl t cp)


-- firstOcc returns the first occurrence upto the specified time.  If the first occurrence is after this time it will return nothing

fstOcc : {A : Set} → SigVec (E A) → Time → Maybe (Time × A)
fstOcc (just a  , _) _ = just (O , a)
fstOcc (nothing , cp) t with cp t
... | []          = nothing
... | (δ , a) ∷ _ = just (δ >0 , a)

------------------------------------------------------

-- splice s₁ s₂ tx t takes s₁ over the interval [0,tx⟩ and appends it to s₂ over the interval [0,t-tx]

spliceC : {A : Set} → SigVec (C A) → SigVec (C A) → Time → SigVec (C A)
spliceC s₁ s₂ tx t = ifℜ₀ t ≥ tx
                      thengeq (λ p → s₂ (ℜ₀-minus t tx p))
                      elseless (λ _ → s₁ t)

spliceS : {A : Set} → SigVec (S A) → SigVec (S A) → Time → SigVec (S A)
spliceS (a₁ , cp₁) (a₂ , cp₂) O = (a₂ , cp₂)
spliceS (a₁ , cp₁) (a₂ , cp₂) (tx >0) with takeExclEnd cp₁ tx
... | δas , δ = a₁ , λ t → ifℜ₀ t ≥ tx >0
                            thengeq (λ p → δas ++ (δ , a₂) ∷ cp₂ (ℜ₀⁺₀-minus t tx p))
                            elseless (λ _ → cp₁ t)

spliceE : {A : Set} → SigVec (E A) → SigVec (E A) → Time → SigVec (E A)
spliceE (ma₁ , cp₁) (ma₂ , cp₂) O       = (ma₂ , cp₂)
spliceE (ma₁ , cp₁) (ma₂ , cp₂) (tx >0) with takeExclEnd cp₁ tx
... | (δas , δ) = ma₁ , λ t → ifℜ₀ t ≥ tx >0
                               thengeq (λ p → δas ++ delayTDLinit ma₂ δ (cp₂ (ℜ₀⁺₀-minus t tx p)))
                               elseless (λ _ → cp₁ t)

splice : {as : SVDesc} → SigVec as → SigVec as → Time → SigVec as
splice {C _}      s₁           s₂           t  = spliceC s₁ s₂ t
splice {S _}      s₁           s₂           t  = spliceS s₁ s₂ t
splice {E _}      s₁           s₂           t  = spliceE s₁ s₂ t
splice {as , bs}  (sa₁ , sb₁)  (sa₂ , sb₂)  t  = (splice {as} sa₁ sa₂ t , splice {bs} sb₁ sb₂ t)

------------------------------------------------------

-- spliceʳ s₁ s₂ tx t takes s₁ over the interval [0,tx] and appends it to s₂ over the interval ⟨0,t-tx]
-- It cannot be defined in the step signal case

-- spliceCʳ : {A : Set} → SigVec (C A) → SigVec (C A) → Time → SigVec (C A)
-- spliceCʳ s₁ s₂ tx t = ifℜ₀ t > tx
--                         thenmore (λ p → s₂ (ℜ₀-minus t tx (inl p)))
--                         elseleq (λ _ → s₁ t)

-- spliceSʳ : {A : Set} → SigVec (S A) → SigVec (S A) → Time → SigVec (S A)
-- spliceSʳ (a₁ , cp₁) (a₂ , cp₂) tx with takeInclEnd cp₁ tx
-- ... | (δas , d) = a₁ , λ t → ifℜ₀ t > tx 
--                                thenmore (λ p → δas ++ {!cannot define without "soon" changes!}) -- delayTDL d (cp₂ (ℜ₀-minus t tx (inl p)))
--                                elseleq (λ _ → cp₁ t)

-- spliceEʳ : {A : Set} → SigVec (E A) → SigVec (E A) → Time → SigVec (E A)
-- spliceEʳ (ma₁ , cp₁) (_ , cp₂) tx with takeInclEnd cp₁ tx
-- ... | (δas , d) = ma₁ , λ t → ifℜ₀ t > tx
--                                 thenmore (λ p → δas ++ delayTDL d (cp₂ (ℜ₀-minus t tx (inl p))))
--                                 elseleq (λ _ → cp₁ t)

-- spliceʳ : {as : SVDesc} → SigVec as → SigVec as → Time → SigVec as
-- spliceʳ {C _}      s₁           s₂           t  = spliceCʳ s₁ s₂ t
-- spliceʳ {S _}      s₁           s₂           t  = spliceSʳ s₁ s₂ t
-- spliceʳ {E _}      s₁           s₂           t  = spliceEʳ s₁ s₂ t
-- spliceʳ {as , bs}  (sa₁ , sb₁)  (sa₂ , sb₂)  t  = (spliceʳ {as} sa₁ sa₂ t , spliceʳ {bs} sb₁ sb₂ t)

------------------------------------------------------

withTime : {as : SVDesc} → (SampleTime → SigVec as) → SigVec as
withTime {C _}     f = λ t → f t t
withTime {E _}     f = (fst (f O) , λ t → snd (f t) t)
withTime {S _}     f = (fst (f O) , λ t → snd (f t) t)
withTime {as , bs} f = (withTime {as} (fst ∘ f) , withTime {bs} (snd ∘ f))

------------------------------------------------------

sample : {as : SVDesc} → SigVec as → SampleTime → Sample as
sample {C _}      s          t  =  s t
sample {S _}      s          t  =  val s t
sample {E _}      s          t  =  occ s t
sample {as , bs}  (sa , sb)  t  =  (sample {as} sa t , sample {bs} sb t)

------------------------------------------------------

Content : SVDesc → Set
Content (C A) = A
Content (E A) = Maybe A
Content (S A) = Maybe A
Content (as , bs) = Content as × Content bs

content : (as : SVDesc) → SigVec as → Time → Content as
content (C _) s t             = s t
content (E _) s t             = occ s t
content (S _) s t             = change s t
content (as , bs) (sa , sb) t = (content as sa t , content bs sb t)

SVAt : SVDesc → Set
SVAt = Content

at : (as : SVDesc) → SigVec as → Time → Content as
at = content

------------------------------------------------------
