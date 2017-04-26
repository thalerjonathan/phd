{-# OPTIONS --type-in-type #-}

module Primitives where

open import NeilPrelude
open import Real hiding (_*_) renaming (_⁺*_ to _*_)
open import Core
open import Maybe
open import Bool renaming (_∧_ to _&&_; _∨_ to _||_)
open import Queue

---------------------------------------------------------

private

  -- utility constructor functions, hidden from FRP programmer

  mkSF : ∀ {as bs Q} → (Δt → Q → Sample as → Q × Sample bs) → (Sample as → Q × Sample bs) → SF as bs
  mkSF f g = prim (first (node f) ∘ g)

  mkSFsource : ∀ {as bs Q} → (Δt → Q → Q × Sample bs) → Q → Sample bs → SF as bs
  mkSFsource f q sb = mkSF (λ δ q' _ → f δ q') (const (q , sb))

  mkSFtimeless : ∀ {as bs Q} → (Q → Sample as → Q × Sample bs) → Q → SF as bs
  mkSFtimeless f q = mkSF (const f) (f q)

  mkSFstateless : ∀ {as bs} → (Sample as → Sample bs) → SF as bs
  mkSFstateless f = mkSFtimeless (λ _ sa → (unit , f sa)) unit

  mkSFchangeless : ∀ {as bs} → Sample bs → SF as bs
  mkSFchangeless sb = mkSFstateless (const sb)

  -- utility synonyms

  noEvent : ∀ {A} → Sample (E A)
  noEvent = nothing

  event : ∀ {A} → A → Sample (E A)
  event = just

---------------------------------------------------------

identity : ∀ {as} → SF as as
identity = arouter sfId

sfFst : ∀ {as bs} → SF (as , bs) as
sfFst = arouter fstProj

sfSnd : ∀ {as bs} → SF (as , bs) bs
sfSnd = arouter sndProj

infixr 90 _>>>_
infixr 91 _&&&_

_>>>_ :  ∀ {as bs cs} → SF as bs → SF bs cs → SF as cs
_>>>_ = seq

_&&&_ :  ∀ {as bs cs} → SF as bs → SF as cs → SF as (bs , cs)
_&&&_ = fan

rswitch : ∀ {as bs A} → SF as (bs , E A) → (A → SF as (bs , E A)) → SF as bs
rswitch = rswitcher

freeze : ∀ {as bs} → SF as bs → SF as (bs , C (SF as bs))
freeze = freezer

---------------------------------------------------------

constantS : ∀ {as A} → A → SF as (S A)
constantS a = mkSFchangeless a

never : ∀ {as A} → SF as (E A)
never = mkSFchangeless noEvent

now : ∀ {as} → SF as (E Unit)
now = mkSFsource (λ _ _ → (unit , noEvent)) unit (event unit)

notYet : ∀ {A} → SF (E A) (E A)
notYet = mkSF (λ _ → curry id) (const (unit , noEvent))

filterE : ∀ {A} → (A → Bool) → SF (E A) (E A)
filterE p = mkSFstateless (maybeFilter p)

hold : ∀ {A} → A → SF (E A) (S A)
hold = mkSFtimeless (λ q → fork ∘ fromMaybe q)

edge : SF (S Bool) (E Unit)
edge = mkSFtimeless (λ q i → (i , (if i && not q then event unit else noEvent))) true

when : ∀ {A} → (A → Bool) → SF (C A) (E A)
when p = mkSFtimeless (λ q i → (p i , (if p i && not q then event i else noEvent))) true


private

  -- integral has a state consisting of: Total × Previous Sample

  IntegralState = ℜ × ℜ

  integrateRectangle : Δt → IntegralState → ℜ → IntegralState × ℜ
  integrateRectangle δ (tot , x₁) x₂ =  let tot' = tot + (δ * x₁)
                                        in ((tot' , x₂) , tot')

  integrateTrapezium : Δt → IntegralState → ℜ → IntegralState × ℜ
  integrateTrapezium δ (tot , x₁) x₂ =  let tot' = tot + halfℜ (δ * (x₁ + x₂))
                                        in ((tot' , x₂) , tot')

integralS : SF (S ℜ) (C ℜ)
integralS = mkSF integrateRectangle (λ x₀ → ((O , x₀) , O))

integralC : SF (C ℜ) (C ℜ)
integralC = mkSF integrateTrapezium (λ x₀ → ((O , x₀) , O))

--------------------------------------------------------------------

liftC : ∀ {A B} → (A → B) → SF (C A) (C B)
liftC = mkSFstateless

liftS : ∀ {A B} → (A → B) → SF (S A) (S B)
liftS = mkSFstateless

liftE : ∀ {A B} → (A → B) → SF (E A) (E B)
liftE = mkSFstateless ∘ maybeMap

liftC2 : ∀ {A B Z} → (A → B → Z) → SF (C A , C B) (C Z)
liftC2 = mkSFstateless ∘ uncurry

liftS2 : ∀ {A B Z} → (A → B → Z) → SF (S A , S B) (S Z)
liftS2 = mkSFstateless ∘ uncurry

merge : ∀ {A B Z} → (A → Z) → (B → Z) → (A → B → Z) → SF (E A , E B) (E Z)
merge fa fb fab = mkSFstateless (uncurry (maybeMerge fa fb fab))

join : ∀ {A B Z} → (A → B → Z) → SF (E A , E B) (E Z)
join = mkSFstateless ∘ uncurry ∘ maybeMap2

sampleWithC : ∀ {A B Z} → (A → B → Z) → SF (C A , E B) (E Z)
sampleWithC f = mkSFstateless (uncurry (maybeMap ∘ f))

sampleWithS : ∀ {A B Z} → (A → B → Z) → SF (S A , E B) (E Z)
sampleWithS f = mkSFstateless (uncurry (maybeMap ∘ f))

-------------------------------------------------------------------

fromS : ∀ {A} → SF (S A) (C A)
fromS = mkSFstateless id

dfromS : ∀ {A} → A → SF (S A) (C A)
dfromS = mkSFtimeless (flip _,_)

-------------------------------------------------------------------

private DelayQueue : Set → Set
        DelayQueue A = Queue (ReleaseTime⁺ × A)

        ready : {A : Set} → CurrentTime → ReleaseTime⁺ × A → Bool
        ready ct (rt , _) = ct ₀>=⁺ rt

delayC : ∀ {A} → Time⁺ → (Time → A) → SF (C A) (C A)
delayC {A} d f = mkSF delayAuxC (λ a₀ → ((O , nothing , enQueueC O a₀ emptyQueue) , f O))
  where
         -- The "Maybe A" tells us whether we are still in the delay period (nothing) or not (just a, where a is the most recent output sample)
         
         DelayStateC = CurrentTime × Maybe A × DelayQueue A

         enQueueC : CurrentTime → A → DelayQueue A → DelayQueue A
         enQueueC t a = enQueue (t ₀+⁺ d , a)

         deQueueC : CurrentTime → DelayQueue A → Maybe (DelayQueue A × ReleaseTime⁺ × A)
         deQueueC t = deQueueWhileLast (ready t)

         deQueueCstate : CurrentTime → Maybe A → DelayQueue A → Maybe A × DelayQueue A × A
         deQueueCstate t st q with deQueueC t q
         deQueueCstate t st q        | just (q' , (_ , a₂)) = (just a₂ , q' , a₂)
         deQueueCstate t nothing   q | nothing = (nothing , q , f t)
         deQueueCstate t (just a₁) q | nothing = (just a₁ , q , a₁)

         delayAuxTimeC : CurrentTime → Maybe A → DelayQueue A → A → DelayStateC × A
         delayAuxTimeC t ma₁ q a with deQueueCstate t ma₁ q
         ... | (ma₂ , q' , a₂) = ((t , ma₂ , enQueueC t a q') , a₂)

         delayAuxC : Δt → DelayStateC → A → DelayStateC × A
         delayAuxC δ (t₁ , ma₁ , q) = delayAuxTimeC ((δ ⁺+₀ t₁) >0) ma₁ q


delayS : ∀ {A} → Time⁺ → A → SF (S A) (S A)
delayS {A} d a₀ = mkSF delayAuxS (λ a₁ → ((O , a₀ , enQueueS O a₁ emptyQueue) , a₀))
   where
         DelayStateS = CurrentTime × A × DelayQueue A

         enQueueS : CurrentTime → A → DelayQueue A → DelayQueue A
         enQueueS t a = enQueue (t ₀+⁺ d , a)

         deQueueS : CurrentTime → A → DelayQueue A → DelayQueue A × A
         deQueueS t a₁ q with deQueueWhileLast (ready t) q
         ... | nothing              = (q  , a₁)
         ... | just (q' , (_ , a₂)) = (q' , a₂)

         delayAuxTimeS : CurrentTime → A → DelayQueue A → A → DelayStateS × A
         delayAuxTimeS t a₁ q a with deQueueS t a₁ q
         ... | (q' , a₂) = ((t , a₂ , enQueueS t a q') , a₂)

         delayAuxS : Δt → DelayStateS → A → DelayStateS × A
         delayAuxS δ (t₁ , a₁ , q) = delayAuxTimeS ((δ ⁺+₀ t₁) >0) a₁ q


delayE : ∀ {A} → Time⁺ → SF (E A) (E A)
delayE {A} d = mkSF delayAuxE (λ e₀ → ((O , enQueueE O e₀ emptyQueue) , noEvent))
   where
         DelayStateE = CurrentTime × DelayQueue A

         Event = Maybe

         enQueueE : CurrentTime → Event A → DelayQueue A → DelayQueue A
         enQueueE t nothing   = id
         enQueueE t (just a)  = enQueue (t ₀+⁺ d , a)

         deQueueE : CurrentTime → DelayQueue A → DelayQueue A × Event A
         deQueueE t q with deQueueIf (ready t) q
         ... | nothing              = (q  , noEvent)
         ... | just (q' , (_ , a))  = (q' , event a)

         delayAuxTimeE : CurrentTime → DelayQueue A → Event A → DelayStateE × Event A
         delayAuxTimeE t q e with deQueueE t q
         ... | (q' , e₂) = ((t , enQueueE t e q') , e₂)

         delayAuxE : Δt → DelayStateE → Event A → DelayStateE × Event A
         delayAuxE δ (t₁ , q) = delayAuxTimeE ((δ ⁺+₀ t₁) >0) q 

--------------------------------------------------------------------
