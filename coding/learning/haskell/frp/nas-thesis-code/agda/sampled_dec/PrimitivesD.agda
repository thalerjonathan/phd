{-# OPTIONS --type-in-type #-}

module PrimitivesD where

open import NeilPrelude
open import Real hiding (_*_) renaming (_⁺*_ to _*_)
open import CoreD
open import Maybe
open import Queue
open import Bool renaming (_∧_ to _&&_; _∨_ to _||_)

---------------------------------------------------------

private

  -- utility constructor functions, hidden from FRP programmer

  mkSFcau : ∀ {as bs Q} → (Δt → Q → Sample as → Q × Sample bs) → (Sample as → Q × Sample bs) → SF as bs cau
  mkSFcau f g = cprim (first (cnode f) ∘ g)

  mkSFdec : ∀ {as bs Q} → (Δt → Q → (Sample as → Q) × Sample bs) → (Sample as → Q) → Sample bs → SF as bs dec
  mkSFdec f g = dprim (dnode f ∘ g)

  mkSFsource : ∀ {as bs Q} → (Δt → Q → Q × Sample bs) → Q → Sample bs → SF as bs dec
  mkSFsource f q = mkSFdec ((result2 ∘ first) const f) (const q)

  mkSFtimeless : ∀ {as bs Q} → (Q → Sample as → Q × Sample bs) → Q → SF as bs cau
  mkSFtimeless f q = mkSFcau (const f) (f q)

  mkSFstateless : ∀ {as bs} → (Sample as → Sample bs) → SF as bs cau
  mkSFstateless f = mkSFtimeless (curry (second f)) unit

  mkSFchangeless : ∀ {as bs} → Sample bs → SF as bs dec
  mkSFchangeless sb = mkSFsource (λ _ _ → (unit , sb)) unit sb

  -- utility synonyms

  noEvent : ∀ {A} → Sample (E A)
  noEvent = nothing

  event : ∀ {A} → A → Sample (E A)
  event = just

---------------------------------------------------------

identity : ∀ {as} → SF as as cau
identity = arouter sfId

sfFst : ∀ {as bs} → SF (as , bs) as cau
sfFst = arouter fstProj

sfSnd : ∀ {as bs} → SF (as , bs) bs cau
sfSnd = arouter sndProj

infixr 90 _>>>_
infixr 91 _&&&_

_>>>_ : ∀ {d₁ d₂ as bs cs} → SF as bs d₁ → SF bs cs d₂ → SF as cs (d₁ ∨ d₂)
_>>>_ = seq

_&&&_ : ∀ {d₁ d₂ as bs cs} → SF as bs d₁ → SF as cs d₂ → SF as (bs , cs) (d₁ ∧ d₂)
_&&&_ = fan

rswitch : ∀ {d₁ d₂ as bs A} → SF as (bs , E A) d₁ → (A → SF as (bs , E A) d₂) → SF as bs (d₁ ∧ d₂)
rswitch = rswitcher

freeze : ∀ {d as bs} → SF as bs d → SF as (bs , C (SF as bs d)) d
freeze = freezer

loop : ∀ {d as bs cs} → SF (as , cs) bs d → SF bs cs dec → SF as bs d
loop = looper

weaken : ∀ {d as bs} → SF as bs d → SF as bs cau
weaken = weakener

---------------------------------------------------------

constantS : ∀ {as A} → A → SF as (S A) dec
constantS = mkSFchangeless

never : ∀ {as A} → SF as (E A) dec
never = mkSFchangeless noEvent

now : ∀ {as} → SF as (E Unit) dec
now = mkSFsource (λ _ _ → (unit , noEvent)) unit (event unit)

notYet : ∀ {A} → SF (E A) (E A) cau
notYet = mkSFcau (λ _ → curry id) (const (unit , noEvent))

filterE : ∀ {A} → (A → Bool) → SF (E A) (E A) cau
filterE p = mkSFstateless (maybeFilter p)

hold : ∀ {A} → A → SF (E A) (S A) cau
hold = mkSFtimeless (λ q → fork ∘ fromMaybe q)

edge : SF (S Bool) (E Unit) cau
edge = mkSFtimeless (λ q i → (i , (if i && not q then event unit else noEvent))) true

when : ∀ {A} → (A → Bool) → SF (C A) (E A) cau
when p = mkSFtimeless (λ q i → (p i , (if p i && not q then event i else noEvent))) true


private

  -- integral has a state consisting of: Total × Previous Sample

  IntegralState = ℜ × ℜ

  integrateRectangle : Δt → IntegralState → (ℜ → IntegralState) × ℜ
  integrateRectangle δ (tot , x₁) =  let tot' = tot + (δ * x₁)
                                     in ((λ x₂ → (tot' , x₂)) , tot')

  integrateTrapezium : Δt → IntegralState → ℜ → IntegralState × ℜ
  integrateTrapezium δ (tot , x₁) x₂ =  let tot' = tot + halfℜ (δ * (x₁ + x₂))
                                        in ((tot' , x₂) , tot')

integralS : SF (S ℜ) (C ℜ) dec
integralS = mkSFdec integrateRectangle (λ x₀ → (O , x₀)) O

integralC : SF (C ℜ) (C ℜ) cau
integralC = mkSFcau integrateTrapezium (λ x₀ → ((O , x₀) , O))

--------------------------------------------------------------------

liftC : ∀ {A B} → (A → B) → SF (C A) (C B) cau
liftC = mkSFstateless

liftS : ∀ {A B} → (A → B) → SF (S A) (S B) cau
liftS = mkSFstateless

liftE : ∀ {A B} → (A → B) → SF (E A) (E B) cau
liftE = mkSFstateless ∘ maybeMap

liftC2 : ∀ {A B Z} → (A → B → Z) → SF (C A , C B) (C Z) cau
liftC2 = mkSFstateless ∘ uncurry

liftS2 : ∀ {A B Z} → (A → B → Z) → SF (S A , S B) (S Z) cau
liftS2 = mkSFstateless ∘ uncurry

merge : ∀ {A B Z} → (A → Z) → (B → Z) → (A → B → Z) → SF (E A , E B) (E Z) cau
merge fa fb fab = mkSFstateless (uncurry (maybeMerge fa fb fab))

join : ∀ {A B Z} → (A → B → Z) → SF (E A , E B) (E Z) cau
join = mkSFstateless ∘ uncurry ∘ maybeMap2

sampleWithC : ∀ {A B Z} → (A → B → Z) → SF (C A , E B) (E Z) cau
sampleWithC f  = mkSFstateless (uncurry (maybeMap ∘ f))

sampleWithS : ∀ {A B Z} → (A → B → Z) → SF (S A , E B) (E Z) cau
sampleWithS f = mkSFstateless (uncurry (maybeMap ∘ f))

-------------------------------------------------------------------

fromS : ∀ {A} → SF (S A) (C A) cau
fromS = mkSFstateless id

dfromS : ∀ {A} → A → SF (S A) (C A) dec
dfromS = mkSFdec (λ _ q → (id , q)) id

-------------------------------------------------------------------

private DelayQueue : Set → Set
        DelayQueue A = Queue (ReleaseTime⁺ × A)

        ready : {A : Set} → CurrentTime → ReleaseTime⁺ × A → Bool
        ready ct (rt , _) = ct ₀>=⁺ rt

delayC : ∀ {A} → Time⁺ → (Time → A) → SF (C A) (C A) dec
delayC {A} d f = mkSFdec delayAuxC (λ a₀ → (O , nothing , enQueueC O a₀ emptyQueue)) (f O) 
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

         delayAuxTimeC : CurrentTime → Maybe A → DelayQueue A → (A → DelayStateC) × A
         delayAuxTimeC t ma₁ q with deQueueCstate t ma₁ q
         ... | (ma₂ , q' , a₂) = ((λ a → (t , ma₂ , enQueueC t a q')) , a₂)

         delayAuxC : Δt → DelayStateC → (A → DelayStateC) × A
         delayAuxC δ (t₁ , ma₁ , q) = delayAuxTimeC ((δ ⁺+₀ t₁) >0) ma₁ q


delayS : ∀ {A} → Time⁺ → A → SF (S A) (S A) dec
delayS {A} d a₀ = mkSFdec delayAuxS (λ a₁ → (O , a₀ , enQueueS O a₁ emptyQueue)) a₀
   where
         DelayStateS = CurrentTime × A × DelayQueue A

         enQueueS : CurrentTime → A → DelayQueue A → DelayQueue A
         enQueueS t a = enQueue (t ₀+⁺ d , a)

         deQueueS : CurrentTime → A → DelayQueue A → DelayQueue A × A
         deQueueS t a₁ q with deQueueWhileLast (ready t) q
         ... | nothing              = (q  , a₁)
         ... | just (q' , (_ , a₂)) = (q' , a₂)

         delayAuxTimeS : CurrentTime → A → DelayQueue A → (A → DelayStateS) × A
         delayAuxTimeS t a₁ q with deQueueS t a₁ q
         ... | (q' , a₂) = ((λ a → (t , a₂ , enQueueS t a q')) , a₂)

         delayAuxS : Δt → DelayStateS → (A → DelayStateS) × A
         delayAuxS δ (t₁ , a₁ , q) = delayAuxTimeS ((δ ⁺+₀ t₁) >0) a₁ q


delayE : ∀ {A} → Time⁺ → SF (E A) (E A) dec
delayE {A} d = mkSFdec delayAuxE (λ e₀ → (O , enQueueE O e₀ emptyQueue)) noEvent
   where
         DelayStateE = CurrentTime × DelayQueue A

         Event = Maybe

         enQueueE : CurrentTime → Event A → DelayQueue A → DelayQueue A
         enQueueE t nothing  = id
         enQueueE t (just a) = enQueue (t ₀+⁺ d , a)

         deQueueE : CurrentTime → DelayQueue A → DelayQueue A × Event A
         deQueueE t q with deQueueIf (ready t) q
         ... | nothing             = (q  , noEvent)
         ... | just (q' , (_ , a)) = (q' , event a)

         delayAuxTimeE : CurrentTime → DelayQueue A → (Event A → DelayStateE) × Event A
         delayAuxTimeE t q with deQueueE t q
         ... | (q' , e₂) = ((λ e → (t , enQueueE t e q')) , e₂)

         delayAuxE : Δt → DelayStateE → (Event A → DelayStateE) × Event A
         delayAuxE δ (t₁ , q) = delayAuxTimeE ((δ ⁺+₀ t₁) >0) q 

--------------------------------------------------------------------
