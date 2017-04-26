{-# OPTIONS --type-in-type  #-}

module Library2 where

open import NeilPrelude
open import RealTime
open import Real
open import SigVecs
open import NaryFRP
open import Library1
open import RSwitch
open import Bool using (not)

------------------------------------------------------

-- For human-readable versions of this library code
-- (i.e. without the implicit arguments)
-- see the Agda embeddings 

-------------------------------------------------------------------

switchWhen : {as bs : SVDesc} → {A : Set} → SF as bs → SF bs (E A) → (A → SF as bs) → SF as bs
switchWhen {as} {bs} {A} sf sfe = switch {as} {bs} (_>>>_ {as} {bs} {bs , E A} sf (forkSecond {bs} {E A} sfe))

rswitchWhen : {as bs : SVDesc} → {A : Set} → SF as bs → SF bs (E A) → (A → SF as bs) → SF as bs
rswitchWhen {as} {bs} {A} sf sfe f = rswitch {as} {bs} (_>>>_ {as} {bs} {bs , E A} sf (forkSecond {bs} {E A} sfe)) (λ e → _>>>_ {as} {bs} {bs , E A} (f e) (forkSecond {bs} {E A} sfe)) 

replace : {as bs : SVDesc} → {A : Set} → SF as bs → (A → SF as bs) → SF (as , E A) bs
replace {as} {bs} {A} sf f = rswitch {as , E A} {bs} (sfFirst {as} {bs} {E A} sf) (λ e → sfFirst {as} {bs} {E A} (f e))

-------------------------------------------------------------------

constantC : {as : SVDesc} → {A : Set} → A → SF as (C A)
constantC {as} {A} a = _>>>_ {as} {S A} {C A} (constantS {as} a) fromS

dhold : {A : Set} → A → SF (E A) (C A)
dhold {A} a = _>>>_ {E A} {S A} {C A} (hold a) (dfromS a)

iIntegralS : ℜ → SF (S ℜ) (C ℜ)
iIntegralS x = _>>>_ {S ℜ} {C ℜ} {C ℜ} integralS (liftC (_+_ x))

iIntegralC : ℜ → SF (C ℜ) (C ℜ)
iIntegralC x = _>>>_ {C ℜ} {C ℜ} {C ℜ} integralC (liftC (_+_ x))

sampleC : {A B : Set} → SF (C A , E B) (E A)
sampleC = sampleWithC const

sampleS : {A B : Set} → SF (S A , E B) (E A)
sampleS = sampleWithS const

localTime : {as : SVDesc} → SF as (C ℜ)
localTime {as} = _>>>_ {as} {S ℜ} {C ℜ} (constantS {as} ı) integralS

after : {as : SVDesc} → Time⁺ → SF as (E Unit)
after {as} t = _>>>_ {as} {E Unit} {E Unit} (now {as}) (delayE t)

repeatedly : {as : SVDesc} → Time⁺ → SF as (E Unit)
repeatedly {as} t = rswitchWhen {as} {E Unit} (never {as}) (after {E Unit} t) (λ _ → now {as})

tag : {A B : Set} → A → SF (E B) (E A)
tag a = liftE (const a)

nowTag : {as : SVDesc} → {A : Set} → A → SF as (E A)
nowTag {as} {A} a = _>>>_ {as} {E Unit} {E A} (now {as}) (tag a)

afterTag : {as : SVDesc} → {A : Set} → Time⁺ → A → SF as (E A)
afterTag {as} {A} t a = _>>>_ {as} {E Unit} {E A} (after {as} t) (tag a)

once : {A : Set} → SF (E A) (E A)
once {A} = switch {E A} {E A} (sfFork {E A}) (nowTag {E A})

-------------------------------------------------------------------

symLoop : {as bs cs ds : SVDesc} → SF (as , cs) (bs , ds) → SF ds cs → SF as bs
symLoop {as} {bs} {cs} {ds} sff sfb = _>>>_ {as} {bs , ds} {bs} (loop {as} {bs , ds} {cs} sff (_>>>_ {bs , ds} {ds} {cs} (sfSnd {bs} {ds}) sfb)) (sfFst {bs} {ds})

-------------------------------------------------------------------

save : {as bs : SVDesc} → {A : Set} → SF as bs → SF (as , E A) (bs , E (SF as bs))
save {as} {bs} {A} sf = _>>>_ {as , E A} {bs , (C _ , E A)} {bs , E _} (_>>>_ {as , E A} {(bs , C _) , E A} {bs , (C _ , E A)} (sfFirst {as} {bs , C _} {E A} (freeze {as} {bs} sf)) (sfAssocR {bs} {C _} {E A})) (sfSecond {bs} {C _ , E A} {E _} sampleC)

saveReplace : {as bs : SVDesc} → {A : Set} → SF as bs → SF ((as , E A) , E (SF as bs)) (bs , E (SF as bs))
saveReplace {as} {bs} {A} sf = replace {as , E A} {bs , E _} (save {as} {bs} sf) (save {as} {bs})

saveResume : {as bs : SVDesc} → {A B : Set} → SF as bs → SF ((as , E A) , E B) bs
saveResume {as} {bs} {A} {B} sf = symLoop {(as , E A) , E B} {bs} {C _} {E _} (_>>>_ {((as , E A) , E B) , C _} {(as , E A) , E _} {bs , E _} (_>>>_ {((as , E A) , E B) , C _} {(as , E A) , E B , C _} {(as , E A) , E _} (sfAssocR {as , E A} {E B} {C _}) (sfSecond {as , E A} {E B , C _} {E _} (_>>>_ {E B , C _} {C _ , E B} {E _} (sfSwap {E B} {C _}) sampleC))) (saveReplace {as} {bs} sf)) (dhold sf)

-------------------------------------------------------------------

open import BouncingBall

fallingBall : {as : SVDesc} → Ball → SF as (C Ball)
fallingBall {as} (h , v) = _>>>_ {as} {S _} {C _} (constantS {as} (negate g)) (_>>>_ {S _} {C _} {C _} (iIntegralS v) (_>>>_ {C _} {C _ , C _} {C _} (forkFirst {C _} {C _} (iIntegralC h)) (liftC2 (_,_))))

detectBounce : SF (C Ball) (E Ball)
detectBounce = when detectImpact

elasticBall : {as : SVDesc} → Ball → SF as (C Ball)
elasticBall {as} b = rswitchWhen {as} {C _} (fallingBall {as} b) detectBounce (fallingBall {as} ∘ negateVel)

inelasticBall : {as : SVDesc} → Ball → SF as (C Ball)
inelasticBall {as} b = switchWhen {as} {C _} (fallingBall {as} b) detectBounce (λ _ → constantC {as} (O , O))

resetBall : {as : SVDesc} → (Ball → SF as (C Ball)) → Ball → SF (as , E Ball) (C Ball)
resetBall {as} f b = replace {as} {C _} (f b) f

oneInelasticReset : Ball → SF (E Ball) (C Ball)
oneInelasticReset b = _>>>_ {E _} {E _} {C _} once (_>>>_ {E _} {E _ , E _} {C _} (sfFork {E _}) (resetBall {E _} (inelasticBall {E _}) b))

-------------------------------------------------------------------

-- Thomson's Lamp
lamp : {as : SVDesc} → Time⁺ → Bool → SF as (S Bool)
lamp {as} t b = rswitch {as} {S _} (lampAux (t , b)) lampAux
    where
          lampAux : (Time⁺ × Bool) → SF as (S Bool , E (Time⁺ × Bool))
          lampAux (t' , b') = _&&&_ {as} {S _} {E _} (constantS {as} b') (afterTag {as} t' (halfℜ⁺ t' , not b'))

-------------------------------------------------------------------

sampleTime : {A : Set} → SF (E A) (E ℜ)
sampleTime = _>>>_ {E _} {C _ , E _} {E _} (forkFirst {E _} {C _} (localTime {E _})) sampleC

-------------------------------------------------------------------
