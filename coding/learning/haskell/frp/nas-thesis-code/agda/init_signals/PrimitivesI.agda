{-# OPTIONS --type-in-type #-}

module PrimitivesI where

open import NeilPrelude hiding (Total)
open import Real hiding (_*_) renaming (_⁺*_ to _*_)
open import CoreI
open import InitDesc
open import Sample
open import Maybe
open import Bool renaming (_∧_ to _&&_; _∨_ to _||_)
open import Queue

---------------------------------------------------------

private

  -- utility constructor functions, hidden from FRP programmer

  mkSF : ∀ {as bs Q} → (Δt → Q → Sample' (svdTosvd' as) → Q × Sample' (svdTosvd' bs)) → (Sample₀ as → Q × Sample₀ bs) → SF as bs
  mkSF f g = prim (first (node f) ∘ g)

  mkSFtimeless₀ : ∀ {as bs Q} → (Q → Sample' (svdTosvd' as) → Q × Sample' (svdTosvd' bs)) → (Sample₀ as → Q × Sample₀ bs) → SF as bs
  mkSFtimeless₀ f g = mkSF (λ _ → f) g

  mkSFstateless₀ : ∀ {as bs} → (Sample' (svdTosvd' as) → Sample' (svdTosvd' bs)) → (Sample₀ as → Sample₀ bs) → SF as bs
  mkSFstateless₀ f g = mkSFtimeless₀ (λ _ sa → unit , f sa) (λ sa → (unit , g sa))

  mkSFsource : ∀ {as bs Q} → (Δt → Q → Q × Sample' (svdTosvd' bs)) → Q → Sample₀ bs → SF as bs
  mkSFsource f q sb = mkSF (λ δ q' _ → f δ q') (const (q , sb))

  mkSFtimeless : ∀ {as bs Q} → (Q → Sample' (svdTosvd' as) → Q × Sample' (svdTosvd' bs)) → Q → SF (iniSV as) (iniSV bs)
  mkSFtimeless {as} {bs} f q = mkSF (λ _ q' sa → second (subst-Sample' {bs}) (f q' (subst-Sample'R {as} sa))) (argResult (sample₀ToSample'2 {as}) (second (sample'ToSample₀2 {bs})) (f q))

  mkSFstateless : ∀ {as bs} → (Sample' (svdTosvd' as) → Sample' (svdTosvd' bs)) → SF (iniSV as) (iniSV bs)
  mkSFstateless f = mkSFtimeless (λ _ sa → (unit , f sa)) unit

  mkSFchangeless : ∀ {as bs} → Sample' (svdTosvd' bs) → SF as bs
  mkSFchangeless {as} {bs} sb = mkSFsource (λ _ _ → unit , sb) unit (weakenSample'ToSample₀ {bs} sb)

  -- utility synonyms

  noEvent : ∀ {A} → Sample' (E A)
  noEvent = nothing

  event : ∀ {A} → A → Sample' (E A)
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

rswitch : ∀ {as bs A} → (SF as (bs , E A)) → (A → SF as (iniSV bs , E A)) → SF as bs
rswitch = rswitcher

freeze : ∀ {as bs} → SF as bs → SF as (bs , C ini (SF (iniSV as) bs))
freeze = freezer

---------------------------------------------------------

initialise : ∀ {A} → A → SF (C uni A) (C ini A)
initialise a = mkSFstateless₀ id (const a)

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
hold a = mkSFtimeless (λ q → fork ∘ fromMaybe q) a

edge : SF (S Bool) (E Unit)
edge = mkSFtimeless (λ q i → i , (if i && not q then event unit else noEvent)) true

whenAux : ∀ {A} → (A → Bool) → Bool → A → Bool × Sample' (E A)
whenAux p q a = (p a , (if p a && not q then event a else noEvent))

when : ∀ {i A} → (A → Bool) → SF (C i A) (E A)
when {ini} p = mkSFtimeless₀ (whenAux p) (λ a → p a , noEvent)
when {uni} p = mkSFtimeless₀ (whenAux p) (λ _ → (true , noEvent))


private

  -- integral has a state consisting of: Total × Previous Sample

  IntegralState = ℜ × ℜ

  integrateRectangle : Δt → IntegralState → ℜ → IntegralState × ℜ
  integrateRectangle δ (tot , x₁) x₂ =  let tot' = tot + (δ * x₁)
                                        in ((tot' , x₂) , tot')

  integrateTrapezium : Δt → IntegralState → ℜ → IntegralState × ℜ
  integrateTrapezium δ (tot , x₁) x₂ =  let tot' = tot + halfℜ (δ * (x₁ + x₂))
                                        in ((tot' , x₂) , tot')

integralS : SF (S ℜ) (C ini ℜ)
integralS = mkSF integrateRectangle (λ x₀ → ((O , x₀) , O))

-- uninitialised signals are dealt with by assuming the initial value is 0
-- a better approach might be to assume the initial value is the same as that of succeeding sample

integralC : ∀ {i} → SF (C i ℜ) (C ini ℜ)
integralC {ini} = mkSF integrateTrapezium (λ x₀ → ((O , x₀) , O))
integralC {uni} = mkSF integrateTrapezium (λ _  → ((O , O) , O))


--------------------------------------------------------------------

liftC : ∀ {i A B} → (A → B) → SF (C i A) (C i B)
liftC {ini} f = mkSFstateless {C ini _} {C ini _} f
liftC {uni} f = mkSFstateless₀ f id

liftS : ∀ {A B} → (A → B) → SF (S A) (S B)
liftS f = mkSFstateless f

liftE : ∀ {A B} → (A → B) → SF (E A) (E B)
liftE f = mkSFstateless (maybeMap f)

liftC2 : ∀ {i₁ i₂ A B Z} → (A → B → Z) → SF (C i₁ A , C i₂ B) (C (i₁ ⊓ i₂) Z)
liftC2 {ini} {ini} f = mkSFstateless {C ini _ , C ini _} {C ini _} (uncurry f)
liftC2 {ini} {uni} f = mkSFstateless₀ (uncurry f) (const unit)
liftC2 {uni}       f = mkSFstateless₀ (uncurry f) (const unit)

liftS2 : ∀ {A B Z} → (A → B → Z) → SF (S A , S B) (S Z)
liftS2 f = mkSFstateless (uncurry f)

merge : ∀ {A B Z} → (A → Z) → (B → Z) → (A → B → Z) → SF (E A , E B) (E Z)
merge fa fb fab = mkSFstateless (uncurry (maybeMerge fa fb fab))

join : ∀ {A B Z} → (A → B → Z) → SF (E A , E B) (E Z)
join f = mkSFstateless (uncurry (maybeMap2 f))

sampleWithC : ∀ {i A B Z} → (A → B → Z) → SF (C i A , E B) (E Z)
sampleWithC {ini} f = mkSFstateless {C ini _ , E _} {E _} (uncurry (maybeMap ∘ f))
sampleWithC {uni} f = mkSFstateless₀ (uncurry (maybeMap ∘ f)) (const nothing)

sampleWithS : ∀ {A B Z} → (A → B → Z) → SF (S A , E B) (E Z)
sampleWithS f = mkSFstateless (uncurry (maybeMap ∘ f))

-------------------------------------------------------------------

fromS : ∀ {A} → SF (S A) (C ini A)
fromS = mkSFstateless {S _} {C ini _} id

dfromS : ∀ {A} → SF (S A) (C uni A)
dfromS = mkSF (λ _ q a → a , q) (λ a → a , unit) -- mkSFtimeless (flip _,_)

-------------------------------------------------------------------

private DelayQueue : Set → Set
        DelayQueue A = Queue (ReleaseTime⁺ × A)

delayC : ∀ {i A} → Time⁺ → (Time → A) → SF (C i A) (C ini A)
delayC {i} {A} d f = mkSF delayAuxC (λ sa₀ → ((O , nothing , caseSample {i} emptyQueue (λ a₀ → enQueueC O a₀ emptyQueue) sa₀) , f O))
  where
         -- The "Maybe A" tells us whether we are still in the delay period (nothing) or not (just a, where a is the most recent output sample)
         
         DelayStateC = CurrentTime × Maybe A × DelayQueue A

         enQueueC : CurrentTime → A → DelayQueue A → DelayQueue A
         enQueueC t a = enQueue (t ₀+⁺ d , a)

         deQueueC : CurrentTime → DelayQueue A → Maybe (DelayQueue A × ReleaseTime⁺ × A)
         deQueueC t = deQueueWhileLast (λ ta → t ₀>=⁺ fst ta)

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
         deQueueS t a₁ q with deQueueWhileLast (λ ta → t ₀>=⁺ fst ta) q
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
         enQueueE t nothing  = id
         enQueueE t (just a) = enQueue (t ₀+⁺ d , a)

         deQueueE : CurrentTime → DelayQueue A → DelayQueue A × Event A
         deQueueE t q with deQueueIf (λ ta → t ₀>=⁺ fst ta) q
         ... | nothing             = (q  , noEvent)
         ... | just (q' , (_ , a)) = (q' , event a)

         delayAuxTimeE : CurrentTime → DelayQueue A → Event A → DelayStateE × Event A
         delayAuxTimeE t q e with deQueueE t q
         ... | (q' , e₂) = ((t , enQueueE t e q') , e₂)

         delayAuxE : Δt → DelayStateE → Event A → DelayStateE × Event A
         delayAuxE δ (t₁ , q) = delayAuxTimeE ((δ ⁺+₀ t₁) >0) q 

--------------------------------------------------------------------
