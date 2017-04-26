{-# OPTIONS --type-in-type  #-}

module LibraryD where

open import NeilPrelude
open import Real
open import CoreD
open import PrimitivesD
open import Bool using (not)
open import List

-------------------------------------------------------------------

sfSwap : ∀ {as bs} → SF (as , bs) (bs , as) cau
sfSwap = sfSnd &&& sfFst

toFst : ∀ {d as bs cs} → SF as cs d → SF (as , bs) cs d
toFst sf = sfFst >>> sf

toSnd : ∀ {d as bs cs} → SF bs cs d → SF (as , bs) cs d
toSnd sf = sfSnd >>> sf

_***_ : ∀ {d₁ d₂ as bs cs ds} → SF as cs d₁ → SF bs ds d₂ → SF (as , bs) (cs , ds) (d₁ ∧ d₂)
sf1 *** sf2 = toFst sf1 &&& toSnd sf2

sfFirst : ∀ {d as bs cs} → SF as bs d → SF (as , cs) (bs , cs) cau
sfFirst {cau} sf = sf *** identity
sfFirst {dec} sf = sf *** identity

sfSecond : ∀ {d as bs cs} → SF bs cs d → SF (as , bs) (as , cs) cau
sfSecond sf = identity *** sf

sfFork : ∀ {as} → SF as (as , as) cau
sfFork = identity &&& identity

forkFirst : ∀ {d as bs} → SF as bs d → SF as (bs , as) cau
forkFirst {cau} sf = sf &&& identity
forkFirst {dec} sf = sf &&& identity

forkSecond : ∀ {d as bs} → SF as bs d → SF as (as , bs) cau
forkSecond sf = identity &&& sf

sfAssocR : ∀ {as bs cs} → SF ((as , bs) , cs) (as , (bs , cs)) cau
sfAssocR = toFst sfFst &&& sfFirst sfSnd

sfAssocL : ∀ {as bs cs} → SF (as , (bs , cs)) ((as , bs) , cs) cau
sfAssocL = sfSecond sfFst &&& toSnd sfSnd

-------------------------------------------------------------------

switch : ∀ {d₁ d₂ as bs A} → SF as (bs , E A) d₁ → (A → SF as bs d₂) → SF as bs (d₁ ∧ d₂)
switch {cau}       sf f = rswitch sf (λ e → f e &&& never)
switch {dec} {dec} sf f = rswitch sf (λ e → f e &&& never)
switch {dec} {cau} sf f = rswitch sf (λ e → f e &&& never)

switchWhen : ∀ {d₁ d₂ d₃ as bs A} → SF as bs d₁ → SF bs (E A) d₂ → (A → SF as bs d₃) → SF as bs (d₁ ∧ d₃)
switchWhen {cau} sf sfe = switch (sf >>> forkSecond sfe)
switchWhen {dec} sf sfe = switch (sf >>> forkSecond sfe)

rswitchWhen : ∀ {d₁ d₂ d₃ as bs A} → SF as bs d₁ → SF bs (E A) d₂ → (A → SF as bs d₃) → SF as bs (d₁ ∧ d₃)
rswitchWhen {cau}           sf sfe f = rswitch (sf >>> forkSecond sfe) (λ e → f e >>> forkSecond sfe) 
rswitchWhen {dec} {_} {cau} sf sfe f = rswitch (sf >>> forkSecond sfe) (λ e → f e >>> forkSecond sfe)
rswitchWhen {dec} {_} {dec} sf sfe f = rswitch (sf >>> forkSecond sfe) (λ e → f e >>> forkSecond sfe)

replace : ∀ {d₁ d₂ as bs A} → SF as bs d₁ → (A → SF as bs d₂) → SF (as , E A) bs cau
replace sf f = rswitch (sfFirst sf) (λ e → sfFirst (f e))

-------------------------------------------------------------------

constantC : ∀ {as A} → A → SF as (C A) dec
constantC a = constantS a >>> fromS

dhold : ∀ {A} → A → SF (E A) (C A) dec
dhold a = hold a >>> dfromS a

iIntegralS : ℜ → SF (S ℜ) (C ℜ) dec
iIntegralS x = integralS >>> liftC (_+_ x)

iIntegralC : ℜ → SF (C ℜ) (C ℜ) cau
iIntegralC x = integralC >>> liftC (_+_ x)

sampleC : ∀ {A B} → SF (C A , E B) (E A) cau
sampleC = sampleWithC const

sampleS : ∀ {A B} → SF (S A , E B) (E A) cau
sampleS = sampleWithS const

localTime : ∀ {as} → SF as (C ℜ) dec
localTime = constantS ı >>> integralS

after : ∀ {as} → Time⁺ → SF as (E Unit) dec
after t = now >>> delayE t

repeatedly : ∀ {as} → Time⁺ → SF as (E Unit) dec
repeatedly t = rswitchWhen never (after t) (λ _ → now)

tag : ∀ {A B} → A → SF (E B) (E A) cau
tag a = liftE (const a)

nowTag : ∀ {as A} → A → SF as (E A) dec
nowTag a = now >>> tag a

afterTag : ∀ {as A} → Time⁺ → A → SF as (E A) dec
afterTag t a = after t >>> tag a

once : ∀ {A} → SF (E A) (E A) cau
once = switch sfFork nowTag

afterEach : ∀ {as} → List Δt → SF as (E Unit) dec
afterEach [] = never
afterEach {as} (δ ∷ δs) = switch (never &&& after δ) (λ _ → afterEachAux δs)
  where
    afterEachAux : List Δt → SF as (E Unit) dec
    afterEachAux [] = now
    afterEachAux (δ ∷ δs) = switch (now &&& after δ) (λ _ → afterEachAux δs)

afterEachTag : ∀ {as A} → List (Δt × A) → SF as (E A) dec
afterEachTag [] = never
afterEachTag {as} {A} ((δ , e) ∷ δes) = switch (never &&& afterTag δ e) (afterEachTagAux δes)
  where
    afterEachTagAux : List (Δt × A) → A → SF as (E A) dec
    afterEachTagAux [] e0 = nowTag e0
    afterEachTagAux ((δ , e1) ∷ δes) e0 = switch (nowTag e0 &&& afterTag δ e1) (afterEachTagAux δes) 

-------------------------------------------------------------------

symLoop : ∀ {d as bs cs ds} → SF (as , cs) (bs , ds) d → SF ds cs dec → SF as bs d
symLoop {cau} sff sfb = loop sff (sfSnd >>> sfb) >>> sfFst
symLoop {dec} sff sfb = loop sff (sfSnd >>> sfb) >>> sfFst

loopInterdefineable : ∀ {d as bs cs} → SF (as , cs) bs d → SF bs cs dec → SF as bs d
loopInterdefineable {dec} sff sfb = symLoop (sff >>> sfFork) sfb
loopInterdefineable {cau} sff sfb = symLoop (sff >>> sfFork) sfb

postulate generalLoop : ∀ {d₁ d₂ as bs cs} → SF (as , cs) bs d₁ → SF bs cs d₂ → d₁ ∨ d₂ ≡ dec → SF as bs d₁

-- This should be dec overall, need decoupled matrices
loop' : ∀ {d as bs cs} → SF (as , cs) bs dec → SF bs cs d → SF as bs cau
loop' sff sfb = loop (sfSecond (forkFirst sfb) >>> sfAssocL) (sfFst >>> sff) >>> sfSnd

-------------------------------------------------------------------

save : ∀ {d as bs A} → SF as bs d → SF (as , E A) (bs , E (SF as bs d)) cau
save sf = sfFirst (freeze sf) >>> sfAssocR >>> sfSecond sampleC

saveReplace : ∀ {d as bs A} → SF as bs d → SF ((as , E A) , E (SF as bs d)) (bs , E (SF as bs d)) cau
saveReplace sf = replace (save sf) save

saveResume : ∀ {d as bs A B} → SF as bs d → SF ((as , E A) , E B) bs cau
saveResume sf = symLoop (sfAssocR >>> sfSecond (sfSwap >>> sampleC) >>> saveReplace sf) (dhold sf)

-------------------------------------------------------------------

open import BouncingBall

fallingBall : ∀ {as} → Ball → SF as (C Ball) dec
fallingBall (h , v) = constantS (negate g) >>> iIntegralS v >>> forkFirst (iIntegralC h) >>> liftC2 (_,_)

detectBounce : SF (C Ball) (E Ball) cau
detectBounce = when detectImpact

elasticBall : ∀ {as} → Ball → SF as (C Ball) dec
elasticBall b = rswitchWhen (fallingBall b) detectBounce (fallingBall ∘ negateVel)

inelasticBall : ∀ {as} → Ball → SF as (C Ball) dec
inelasticBall b = switchWhen (fallingBall b) detectBounce (λ _ → constantC (O , O))

resetBall : ∀ {d as} → (Ball → SF as (C Ball) d) → Ball → SF (as , E Ball) (C Ball) cau
resetBall f b = replace (f b) f

oneInelasticReset : Ball → SF (E Ball) (C Ball) cau
oneInelasticReset b = once >>> sfFork >>> resetBall inelasticBall b

-------------------------------------------------------------------

-- Thomson's Lamp
lamp : ∀ {as} → Time⁺ → Bool → SF as (S Bool) dec
lamp {as} t b = rswitch (lampAux (t , b)) lampAux
    where
          lampAux : (Time⁺ × Bool) → SF as (S Bool , E (Time⁺ × Bool)) dec
          lampAux (t' , b') = constantS b' &&& afterTag t' (halfℜ⁺ t' , not b')

-------------------------------------------------------------------

sampleTime : ∀ {A} → SF (E A) (E ℜ) cau
sampleTime = forkFirst localTime >>> sampleC

-------------------------------------------------------------------

inaccurate : ∀ {A} → SF (E A) (E A , E Unit) cau
inaccurate = (identity &&& now) >>> (delayE ı⁺ *** identity)

accurate : ∀ {A} → SF (E A) (E A , E Unit) dec
accurate = (identity >>> delayE ı⁺) &&& (now >>> identity)

-------------------------------------------------------------------
