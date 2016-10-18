{-# OPTIONS --type-in-type --no-termination-check #-}

module CFRP where

open import NeilPrelude
open import List
open import RealTime renaming (_₀>=₀_ to _>=_ ; _₀>₀_ to _>_)
open import Real hiding (_>=_)

------------------------------------------

Behaviour : Set → Set
Behaviour A = StartTime → SampleTime → A

Event : Set → Set
Event A = StartTime → SampleTime → List (Time × A)

------------------------------------------

constant : {A : Set} → A → Behaviour A
constant a = λ t₀ t₁ → a  

-- liftE : {A B : Set} → (A → B) → Event A → Event B
-- liftE f ev = (result2 ∘ map ∘ second) f ev 

liftB : {A B : Set} → (A → B) → Behaviour A → Behaviour B
liftB f beh = λ t₀ t₁ → f (beh t₀ t₁)

liftB2 : {A B C : Set} → (A → B → C) → Behaviour A → Behaviour B → Behaviour C
liftB2 f beh₁ beh₂ = λ t₀ t₁ → f (beh₁ t₀ t₁) (beh₂ t₀ t₁)

notYet : {A : Set} → Event A → Event A
notYet ev = λ t₀ t₁ → dropWhile (_>=_ t₀ ∘ fst) (ev t₀ t₁)

------------------------------------------

postulate when : {A : Set} → (A → Bool) → Behaviour A → Event A
postulate integral : Behaviour ℜ → Behaviour ℜ

iIntegral : ℜ → Behaviour ℜ → Behaviour ℜ
iIntegral x = liftB (_+_ x) ∘ integral

------------------------------------------

untilB : {A E : Set} → Behaviour A → Event E → (E → Behaviour A) → Behaviour A
untilB beh ev f t₀ t₁ with ev t₀ t₁
... | []            = beh t₀ t₁
... | (te , e) ∷ _  = (f e) te t₁

untilB' : {A E : Set} → Behaviour A → Event E → (E → Behaviour A) → Behaviour A
untilB' beh ev f t₀ t₁ with ev t₀ t₁
... | []            = beh t₀ t₁
... | (te , e) ∷ _  = (f e) t₀ t₁

------------------------------------------

replaceBeh₀ : {A E : Set} → Event E → Behaviour A → (E → Behaviour A) → Behaviour A
replaceBeh₀ ev beh f = untilB beh ev (λ e → replaceBeh₀ ev (f e) f)

------------------------------------------

runningInBB : {A B : Set} → Behaviour A → (Behaviour A → Behaviour B) → Behaviour B
runningInBB beh f = λ t₀ → f (λ _ → beh t₀) t₀

runningInEB : {A B : Set} → Event A → (Event A → Behaviour B) → Behaviour B
runningInEB ev f = λ t₀ → f (λ te → dropWhile ((_>_ te) ∘ fst) ∘ ev t₀) t₀

------------------------------------------

replaceBeh : {A E : Set} → Event E → Behaviour A → (E → Behaviour A) → Behaviour A
replaceBeh {A} {E} ev beh f = runningInEB ev (λ rev → replaceBehAux rev beh)
                              where
                                   replaceBehAux : Event E → Behaviour A → Behaviour A
                                   replaceBehAux rev beh' = untilB beh' rev (λ e → replaceBehAux (notYet rev) (f e))

------------------------------------------

open import BouncingBall

fallingBall : Ball → Behaviour Ball
fallingBall (h₀ , v₀) =  let  a = constant (negate g)
                              v = iIntegral v₀ a
                              h = iIntegral h₀ v
                         in liftB2 (_,_) h v

detectBounce : Behaviour Ball → Event Ball
detectBounce = when detectImpact

elasticBall : Ball → Behaviour Ball
elasticBall b =  let beh = fallingBall b
                 in untilB beh (detectBounce beh) (elasticBall ∘ negateVel)

inelasticBall : Ball → Behaviour Ball
inelasticBall b =  let beh = fallingBall b
                   in untilB beh (detectBounce beh) (λ _ → constant (O , O))

resetBall₀ : Event Ball → (Ball → Behaviour Ball) → Ball → Behaviour Ball
resetBall₀ ev f b = untilB (f b) ev (resetBall₀ ev f)

resetBall : Event Ball → (Ball → Behaviour Ball) → Ball → Behaviour Ball
resetBall ev f b = replaceBeh ev (f b) f

------------------------------------------
