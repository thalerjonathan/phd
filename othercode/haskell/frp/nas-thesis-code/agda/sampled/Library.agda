{-# OPTIONS --type-in-type  #-}

module Library where

open import NeilPrelude
open import Real
open import Core
open import Primitives
open import Bool using (not)
open import List

--------------------------------------------------------------------

sfSwap : ∀ {as bs} → SF (as , bs) (bs , as)
sfSwap = sfSnd &&& sfFst

toFst : ∀ {as bs cs} → SF as cs → SF (as , bs) cs
toFst sf = sfFst >>> sf

toSnd : ∀ {as bs cs} → SF bs cs → SF (as , bs) cs
toSnd sf = sfSnd >>> sf

_***_ : ∀ {as bs cs ds} → SF as cs → SF bs ds → SF (as , bs) (cs , ds)
sf1 *** sf2 = toFst sf1 &&& toSnd sf2

sfFirst : ∀ {as bs cs} → SF as bs → SF (as , cs) (bs , cs)
sfFirst sf = sf *** identity

sfSecond : ∀ {as bs cs} → SF bs cs → SF (as , bs) (as , cs)
sfSecond sf = identity *** sf

sfFork : ∀ {as} → SF as (as , as)
sfFork = identity &&& identity

forkFirst : ∀ {as bs} → SF as bs → SF as (bs , as)
forkFirst sf = sf &&& identity

forkSecond : ∀ {as bs} → SF as bs → SF as (as , bs)
forkSecond sf = identity &&& sf

sfAssocR : ∀ {as bs cs} → SF ((as , bs) , cs) (as , (bs , cs))
sfAssocR = toFst sfFst &&& sfFirst sfSnd

sfAssocL : ∀ {as bs cs} → SF (as , (bs , cs)) ((as , bs) , cs)
sfAssocL = sfSecond sfFst &&& toSnd sfSnd

-------------------------------------------------------------------

switch : ∀ {as bs A} → SF as (bs , E A) → (A → SF as bs) → SF as bs
switch sf f = rswitch sf (λ e → f e &&& never)

switchWhen : ∀ {as bs A} → SF as bs → SF bs (E A) → (A → SF as bs) → SF as bs
switchWhen sf sfe = switch (sf >>> forkSecond sfe)

rswitchWhen : ∀ {as bs A} → SF as bs → SF bs (E A) → (A → SF as bs) → SF as bs
rswitchWhen sf sfe f = rswitch (sf >>> forkSecond sfe) (λ e → f e >>> forkSecond sfe) 

replace : ∀ {as bs A} → SF as bs → (A → SF as bs) → SF (as , E A) bs
replace sf f = rswitch (sfFirst sf) (λ e → sfFirst (f e))

-------------------------------------------------------------------

constantC : ∀ {as A} → A → SF as (C A)
constantC a = constantS a >>> fromS

dhold : ∀ {A} → A → SF (E A) (C A)
dhold a = hold a >>> dfromS a

iIntegralS : ℜ → SF (S ℜ) (C ℜ)
iIntegralS x = integralS >>> liftC (_+_ x)

iIntegralC : ℜ → SF (C ℜ) (C ℜ)
iIntegralC x = integralC >>> liftC (_+_ x)

sampleC : ∀ {A B} → SF (C A , E B) (E A)
sampleC = sampleWithC const

sampleS : ∀ {A B} → SF (S A , E B) (E A)
sampleS = sampleWithS const

localTime : ∀ {as} → SF as (C ℜ)
localTime = constantS ı >>> integralS

after : ∀ {as} → Time⁺ → SF as (E Unit)
after t = now >>> delayE t

repeatedly : ∀ {as} → Time⁺ → SF as (E Unit)
repeatedly t = rswitchWhen never (after t) (λ _ → now)

tag : ∀ {A B} → A → SF (E B) (E A)
tag a = liftE (const a)

nowTag : ∀ {as A} → A → SF as (E A)
nowTag a = now >>> tag a

afterTag : ∀ {as A} → Time⁺ → A → SF as (E A)
afterTag t a = after t >>> tag a

once : ∀ {A} → SF (E A) (E A)
once = switch sfFork nowTag

afterEach : ∀ {as} → List Δt → SF as (E Unit)
afterEach [] = never
afterEach {as} (δ ∷ δs) = switch (never &&& after δ) (λ _ → afterEachAux δs)
  where
    afterEachAux : List Δt → SF as (E Unit)
    afterEachAux [] = now
    afterEachAux (δ ∷ δs) = switch (now &&& after δ) (λ _ → afterEachAux δs)

afterEachTag : ∀ {as A} → List (Δt × A) → SF as (E A)
afterEachTag [] = never
afterEachTag {as} {A} ((δ , e) ∷ δes) = switch (never &&& afterTag δ e) (afterEachTagAux δes)
  where
    afterEachTagAux : List (Δt × A) → A → SF as (E A)
    afterEachTagAux [] e0 = nowTag e0
    afterEachTagAux ((δ , e1) ∷ δes) e0 = switch (nowTag e0 &&& afterTag δ e1) (afterEachTagAux δes) 

-------------------------------------------------------------------

save : ∀ {as bs A} → SF as bs → SF (as , E A) (bs , E (SF as bs))
save sf = sfFirst (freeze sf) >>> sfAssocR >>> sfSecond sampleC

saveReplace : ∀ {as bs A} → SF as bs → SF ((as , E A) , E (SF as bs)) (bs , E (SF as bs))
saveReplace sf = replace (save sf) save

-------------------------------------------------------------------

open import BouncingBall

fallingBall : ∀ {as} → Ball → SF as (C Ball)
fallingBall (h , v) = constantS (negate g) >>> iIntegralS v >>> forkFirst (iIntegralC h) >>> liftC2 (_,_)

detectBounce : SF (C Ball) (E Ball)
detectBounce = when detectImpact

elasticBall : ∀ {as} → Ball → SF as (C Ball)
elasticBall b = rswitchWhen (fallingBall b) detectBounce (fallingBall ∘ negateVel)

inelasticBall : ∀ {as} → Ball → SF as (C Ball)
inelasticBall b = switchWhen (fallingBall b) detectBounce (λ _ → constantC (O , O))

resetBall : ∀ {as} → (Ball → SF as (C Ball)) → Ball → SF (as , E Ball) (C Ball)
resetBall f b = replace (f b) f

oneInelasticReset : Ball → SF (E Ball) (C Ball)
oneInelasticReset b = once >>> sfFork >>> resetBall inelasticBall b

-------------------------------------------------------------------

-- Thomson's Lamp
lamp : ∀ {as} → Time⁺ → Bool → SF as (S Bool)
lamp {as} t b = rswitch (lampAux (t , b)) lampAux
    where
          lampAux : (Time⁺ × Bool) → SF as (S Bool , E (Time⁺ × Bool))
          lampAux (t' , b') = constantS b' &&& afterTag t' (halfℜ⁺ t' , not b')

-------------------------------------------------------------------

sampleTime : ∀ {A} → SF (E A) (E ℜ)
sampleTime = forkFirst localTime >>> sampleC

-------------------------------------------------------------------
