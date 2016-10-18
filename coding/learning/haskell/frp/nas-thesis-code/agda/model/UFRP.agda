{-# OPTIONS --type-in-type --no-termination-check #-}

module UFRP where

open import NeilPrelude
open import List
open import RealTime renaming (_₀>=₀_ to _>=_ ; _₀>₀_ to _>_ ; _₀<₀_ to _<_)
open import Real hiding (_-_ ; _>=_)
open import StrictTotalOrder hiding (_>_ ; _<_)
open import Bool renaming (_∧_ to _&&_ ; _∨_ to _||_)

------------------------------------------

Signal : Set → Set
Signal A = Time → A

SF : Set → Set → Set
SF A B = Signal A → Signal B

Event : Set → Set
Event A = List (Time × A)

------------------------------------------

constant : {A B : Set} → B → SF A B
constant b = λ s t → b  

lift : {A B : Set} → (A → B) → SF A B
lift f = λ s → f ∘ s

notYet : {A : Set} → SF (Event A) (Event A)
notYet = λ s → dropWhile (_>=_ O ∘ fst) ∘ s

---------------------------------------------

postulate when : {A : Set} → (A → Bool) → SF A (Event A)
postulate integral : SF ℜ ℜ

---------------------------------------------

infixr 90 _>>>_
infixr 91 _&&&_

_>>>_ : {A B C : Set} → SF A B → SF B C → SF A C
sf₁ >>> sf₂ = sf₂ ∘ sf₁

_&&&_ : {A B C : Set} → SF A B → SF A C → SF A (B × C)
sf₁ &&& sf₂ = λ s t → (sf₁ s t , sf₂ s t)

---------------------------------------------

-- We know the sample time is at least the event time,
-- but inserting a proof into the definition would unnecessarily complicate it
private  
         postulate ≤-trustme : {te : EventTime} → {t : SampleTime} → te ≤ℜ₀ t

         _-_ : (t : SampleTime) → (te : EventTime) → Time
         t - te = ℜ₀-minus t te ≤-trustme

         advance : {A : Set} → Time → Signal A → Signal A
         advance d s t = s (t ₀+₀ d)

         splice : {A : Set} → Signal A → Signal A → Time → Signal A
         splice s₁ s₂ tx t with compareGeqℜ₀ t tx
         ... | less p = s₁ t
         ... | geq  p = s₂ (ℜ₀-minus t tx p)

switch : {A B E : Set} → SF A (B × Event E) → (E → SF A B) → SF A B
switch {A} sf f s t with sf s t
... | (b , ev) with dropWhile (λ tee → fst tee < O) ev
...    | []            = b
...    | (te , e) ∷ _  = (f e) (advance te s) (t - te)

dswitch : {A B E : Set} → SF A (B × Event E) → (E → SF A B) → SF A B
dswitch {A} sf f s t with sf s t
... | (b , ev) with dropWhile (λ tee → fst tee < O || fst tee >= t) ev
...    | []            = b
...    | (te , e) ∷ _  = (f e) (advance te s) (t - te)

freeze : {A B : Set} → SF A B → SF A (B × SF A B)
freeze sf = λ s t → (sf s t , λ s' → advance t (sf (splice s s' t)))

------------------------------------------

iIntegral : ℜ → SF ℜ ℜ
iIntegral x = integral >>> lift (_+_ x)

------------------------------------------

-- fanoutFirst : {A B : Set} → SF A B → SF A (B × A)
-- fanoutFirst sf = sf &&& lift id

-- fanoutSecond : {A B : Set} → SF A B → SF A (A × B)
-- fanoutSecond sf = lift id &&& sf

-- sfFirst : {A B C : Set} → SF A B → SF (A × C) (B × C)
-- sfFirst sf = (lift fst >>> sf) &&& lift snd

-- sfSecond : {A B C : Set} → SF B C → SF (A × B) (A × C)
-- sfSecond sf = lift fst &&& (lift snd >>> sf)

identity : {A : Set} → SF A A
identity = lift id

sfFst : {A B : Set} → SF (A × B) A
sfFst = lift fst

sfSnd : {A B : Set} → SF (A × B) B
sfSnd = lift snd

sfSwap : {A B : Set} → SF (A × B) (B × A)
sfSwap = lift swap

sfFork : {A : Set} → SF A (A × A)
sfFork = lift fork

toFst : {A B C : Set} → SF A C → SF (A × B) C
toFst sf = sfFst >>> sf

toSnd : {A B C : Set} → SF B C → SF (A × B) C
toSnd sf = sfSnd >>> sf

_***_ : {A B C D : Set} → SF A C → SF B D → SF (A × B) (C × D)
sf₁ *** sf₂ = toFst sf₁ &&& toSnd sf₂

sfFirst : {A B C : Set} → SF A B → SF (A × C) (B × C)
sfFirst sf = sf *** identity

sfSecond : {A B C : Set} → SF B C → SF (A × B) (A × C)
sfSecond sf = identity *** sf

forkFirst : {A B : Set} → SF A B → SF A (B × A)
forkFirst sf = sfFork >>> sfFirst sf

forkSecond : {A B : Set} → SF A B → SF A (A × B)
forkSecond sf = sfFork >>> sfSecond sf

------------------------------------------

switchWhen : {A B E : Set} → SF A B → SF B (Event E) → (E → SF A B) → SF A B
switchWhen sf sfe = switch (sf >>> forkSecond sfe)

rswitch : {A B E : Set} → SF A (B × Event E) → (E → SF A (B × Event E)) → SF A B
rswitch sf f = switch sf (λ e → rswitch (f e >>> sfSecond notYet) f)

rswitchWhen : {A B E : Set} → SF A B → SF B (Event E) → (E → SF A B) → SF A B
rswitchWhen sf sfe f = rswitch (sf >>> forkSecond sfe) (λ e → f e >>> forkSecond sfe) 

replace : {A B E : Set} → SF A B → (E → SF A B) → SF (A × Event E) B
replace sf f = rswitch (sfFirst sf) (λ e → sfFirst (f e))

------------------------------------------

open import BouncingBall

fallingBall : {A : Set} → Ball → SF A Ball
fallingBall (h , v) = constant (negate g) >>> iIntegral v >>> forkFirst (iIntegral h)

detectBounce : SF Ball (Event Ball)
detectBounce = when detectImpact

elasticBall : {A : Set} → Ball → SF A Ball
elasticBall b = rswitchWhen (fallingBall b) detectBounce (fallingBall ∘ negateVel)

inelasticBall : {A : Set} → Ball → SF A Ball
inelasticBall b = switchWhen (fallingBall b) detectBounce (λ _ → constant (O , O))

resetBall : {A : Set} → (Ball → SF A Ball) → Ball → SF (A × Event Ball) Ball
resetBall f b = replace (f b) f

------------------------------------------

initialise : {A : Set} → A → Signal A → Signal A
initialise a s O       = a
initialise a s (t >0)  = s (t >0)

------------------------------------------
