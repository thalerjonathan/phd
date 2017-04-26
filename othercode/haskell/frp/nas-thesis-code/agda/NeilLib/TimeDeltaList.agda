{-# OPTIONS --type-in-type  #-}

open import NeilPrelude
open import List
open import Bool
open import RealTime
open import StrictTotalOrder
open import Maybe

module TimeDeltaList where

-----------------------------------------------------

TimeDeltaList : Set → Set
TimeDeltaList A = List (Δt × A)

TDList = TimeDeltaList

-----------------------------------------------------

mapTDL : {A B : Set} → (A → B) → TDList A → TDList B
mapTDL = map ∘ second

mapTDLtime : {A B : Set} → (Time → A → B) → TDList A → TDList B
mapTDLtime {A} {B} f = mapTDLtimeAux O
  where    
    mapTDLtimeAux : Time → TDList A → TDList B
    mapTDLtimeAux d [] = []
    mapTDLtimeAux d ((δ , a) ∷ δas) = let d' = (d ₀+⁺ δ) >0 in (δ , f d' a) ∷ mapTDLtimeAux d' δas

mapTDLtime⁺ : {A B : Set} → (Time⁺ → A → B) → TDList A → TDList B
mapTDLtime⁺ {A} {B} f = mapTDLtimeAux⁺ O
  where    
    mapTDLtimeAux⁺ : Time → TDList A → TDList B
    mapTDLtimeAux⁺ d [] = []
    mapTDLtimeAux⁺ d ((δ , a) ∷ δas) = let d' = (d ₀+⁺ δ) in (δ , f d' a) ∷ mapTDLtimeAux⁺ (d' >0) δas

-----------------------------------------------------

-- delayTDL increases the first time delta

delayTDL⁺ : {A : Set} → Time⁺ → TDList A → TDList A
delayTDL⁺ d [] = []
delayTDL⁺ d ((δ , a) ∷ δas) = (d ⁺+⁺ δ , a) ∷ δas

delayTDL : {A : Set} → Time → TDList A → TDList A
delayTDL O      δas = δas 
delayTDL (d >0) δas = delayTDL⁺ d δas

delayTDLinit : {A : Set} → Maybe A → Time⁺ → TDList A → TDList A
delayTDLinit (just a) d δas  = (d , a) ∷ δas
delayTDLinit nothing  d δas  = delayTDL⁺ d δas

-- advanceTDL decreases the first time delta (the point of observation advances)
-- any time deltas we reach are discarded

advanceTDL⁺ : {A : Set} → Time⁺ → TDList A → TDList A
advanceTDL⁺ d [] = []
advanceTDL⁺ d ((δ , a) ∷ δas) with compareℜ⁺ δ d
advanceTDL⁺ d ((.d , a) ∷ δas) | refl   = δas
advanceTDL⁺ d ((δ , a)  ∷ δas) | less p = advanceTDL⁺ ((d ⁺-⁺ δ) p) δas
advanceTDL⁺ d ((δ , a)  ∷ δas) | more p = ((δ ⁺-⁺ d) p , a) ∷ δas

advanceTDL : {A : Set} → Time → TDList A → TDList A
advanceTDL d [] = []
advanceTDL d ((δ , a) ∷ δas) with compareLeqℜ₀ (δ >0) d
... | leq  p = advanceTDL (ℜ₀⁺₀-minus d δ p) δas
... | more p = (ℜ⁺₀⁺-minus δ d p , a) ∷ δas

sumTDL : {A : Set} → TDList A → Time
sumTDL = sumℜ⁺ ∘ map fst

lookupTDL⁺ : {A : Set} → TDList A → Time⁺ → Maybe A
lookupTDL⁺ [] t = nothing
lookupTDL⁺ ((δ , a) ∷ δas) t with compareℜ⁺ δ t
lookupTDL⁺ ((δ , a) ∷ δas) .δ | refl   = just a
lookupTDL⁺ ((δ , a) ∷ δas) t  | more p = nothing
lookupTDL⁺ ((δ , a) ∷ δas) t  | less p = lookupTDL⁺ δas ((t ⁺-⁺ δ) p)

lookupTDL : {A : Set} → TDList A → Time → Maybe A
lookupTDL δas O      = nothing
lookupTDL δas (t >0) = lookupTDL⁺ δas t

-- safelookupTDL⁺ : {A : Set} → A → TDList A → Time⁺ → A
-- safelookupTDL⁺ a₀ [] t = a₀
-- safelookupTDL⁺ a₀ ((δ , a₁) ∷ δas) t with compareℜ⁺ δ t
-- safelookupTDL⁺ a₀ ((δ , a₁) ∷ δas) .δ | refl   = a₁
-- safelookupTDL⁺ a₀ ((δ , a₁) ∷ δas) t  | more p = a₀
-- safelookupTDL⁺ a₀ ((δ , a₁) ∷ δas) t  | less p = safelookupTDL⁺ a₁ δas ((t ⁺-⁺ δ) p)

safelookupTDL : {A : Set} → A → TDList A → Time → A
safelookupTDL a₀ [] t = a₀
safelookupTDL a₀ ((δ , a₁) ∷ δas) t with compareLeqℜ₀ (δ >0) t
... | leq p  = safelookupTDL a₁ δas (ℜ₀⁺₀-minus t δ p)
... | more p = a₀

-----------------------------------------------------

-- "takeIncl" takes all changes upto and including the given time, and discards the rest
-- This is one of several places where using an Interval datatype rather than a Time would be neater
-- (as the only difference is whether the interval is closed or open)

takeIncl⁺ : {A : Set} → Time⁺ → TDList A → TDList A
takeIncl⁺ _ [] = []
takeIncl⁺ t  ((δ , a) ∷ δas) with compareℜ⁺ δ t
takeIncl⁺ t  ((δ , a) ∷ δas) | more p = []
takeIncl⁺ .δ ((δ , a) ∷ δas) | refl   = (δ , a) ∷ []
takeIncl⁺ t  ((δ , a) ∷ δas) | less p = (δ , a) ∷ takeIncl⁺ ((t ⁺-⁺ δ) p) δas

-- takeIncl : {A : Set} → Time → TDList A → TDList A
-- takeIncl O      δas = []
-- takeIncl (t >0) δas = takeIncl⁺ t δas 

takeIncl : {A : Set} → Time → TDList A → TDList A
takeIncl t [] = []
takeIncl t ((δ , a) ∷ δas) with compareLeqℜ₀ (δ >0) t
... | leq p  = (δ , a) ∷ takeIncl (ℜ₀⁺₀-minus t δ p) δas
... | more p = []

takeExcl : {A : Set} → Time → TDList A → TDList A
takeExcl t [] = []
takeExcl t ((δ , a) ∷ δas) with compareGeqℜ₀ (δ >0) t
... | less p  = (δ , a) ∷ takeExcl (ℜ₀⁺₀-minus t δ (inl p)) δas
... | geq p = []

takeExcl⁺ : {A : Set} → Time⁺ → TDList A → TDList A
takeExcl⁺ _ [] = []
takeExcl⁺ t  ((δ , a) ∷ δas) with compareGeqℜ⁺ δ t
takeExcl⁺ t  ((δ , a) ∷ δas) | geq  p = []
takeExcl⁺ t  ((δ , a) ∷ δas) | less p = (δ , a) ∷ takeExcl⁺ ((t ⁺-⁺ δ) p) δas

-- takeExcl : {A : Set} → Time → TDList A → TDList A
-- takeExcl O      δas = []
-- takeExcl (t >0) δas = takeExcl⁺ t δas 

dropIncl⁺ : {A : Set} → Time⁺ → TDList A → TDList A
dropIncl⁺ _ [] = []
dropIncl⁺ t  ((δ , a) ∷ δas) with compareℜ⁺ δ t
dropIncl⁺ t  ((δ , a) ∷ δas) | more p = (δ , a) ∷ δas
dropIncl⁺ .δ ((δ , a) ∷ δas) | refl   = delayTDL⁺ δ δas
dropIncl⁺ t  ((δ , a) ∷ δas) | less p = delayTDL⁺ δ (dropIncl⁺ ((t ⁺-⁺ δ) p) δas)

dropIncl : {A : Set} → Time → TDList A → TDList A
dropIncl O      δas = δas
dropIncl (t >0) δas = dropIncl⁺ t δas

dropExcl⁺ : {A : Set} → Time⁺ → TDList A → TDList A
dropExcl⁺ _ [] = []
dropExcl⁺ t  ((δ , a) ∷ δas) with compareGeqℜ⁺ δ t
dropExcl⁺ t  ((δ , a) ∷ δas) | geq  p = (δ , a) ∷ δas
dropExcl⁺ t  ((δ , a) ∷ δas) | less p = delayTDL⁺ δ (dropExcl⁺ ((t ⁺-⁺ δ) p) δas)

dropExcl : {A : Set} → Time → TDList A → TDList A
dropExcl O      δas = δas
dropExcl (t >0) δas = dropExcl⁺ t δas

-----------------------------------------------------

-- exclEnd⁺ : {A : Set} → (Time → A × TDList A) → Time⁺ → A × TDList A
-- exclEnd⁺ f t = second (takeExcl⁺ t) (f (t >0))

-- exclEnd : {A : Set} → (Time → A × TDList A) → Time → Maybe A × TDList A
-- exclEnd f O      = (nothing , [])
-- exclEnd f (t >0) = first just (exclEnd⁺ f t)

exclEnd : {A : Set} → (Time → A × TDList A) → Time → Maybe A × TDList A
exclEnd f O = (nothing , [])
exclEnd f t = (just ∥ takeExcl t) (f t)

-----------------------------------------------------

filterTDL : {A : Set} → (A → Bool) → TDList A → TDList A
filterTDL p [] = []
filterTDL p ((δ , a) ∷ δas) = if p a
                               then (δ , a) ∷ filterTDL p δas
                               else delayTDL (δ >0) (filterTDL p δas)

-----------------------------------------------------
