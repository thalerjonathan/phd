{-# OPTIONS --type-in-type #-}

module NaryFRP where

open import NeilPrelude
open import Maybe
open import List
open import RealTime
open import Real hiding (_-_)
open import TimeDeltaList
open import SigVecs
open import Utilities
open import StrictTotalOrder

------------------------------------------------------

identity : {as : SVDesc} → SF as as
identity = id

sfFst : {as bs : SVDesc} → SF (as , bs) as
sfFst = fst

sfSnd : {as bs : SVDesc} → SF (as , bs) bs
sfSnd = snd

infixr 90 _>>>_
infixr 91 _&&&_

_>>>_ : {as bs cs : SVDesc} → SF as bs → SF bs cs → SF as cs
sf₁ >>> sf₂ = sf₂ ∘ sf₁

_&&&_ : {as bs cs : SVDesc} → SF as bs → SF as cs → SF as (bs , cs)
sf₁ &&& sf₂ = λ s → (sf₁ s , sf₂ s)

--------------------------------------------------------------------

switch : {as bs : SVDesc} → {A : Set} → SF as (bs , E A) → (A → SF as bs) → SF as bs
switch {as} {bs} sf f sa with sf sa
... | sb , se = withTime {bs} (λ t → maybe sb
                                           (uncurry (λ te e → (splice {bs} sb ((f e) (advance {as} te sa)) te)))
                                           (fstOcc se t))


-- spliceʳ, and thus dswitch, is not defineable in the N-ary FRP model

-- dswitch : {as bs : SVDesc} → {A : Set} → SF as (bs , E A) → (A → SF as bs) → SF as bs
-- dswitch {as} {bs} sf f sa with sf sa
-- ... | sb , se = withTime {bs} (λ t → maybe sb
--                                            (uncurry (λ te e → (spliceʳ {bs} sb ((f e) (advance {as} te sa)) te)))
--                                            (fstOcc se t))

--------------------------------------------------------------------

-- one could also imagine a "dfreeze" that uses a "dsplice"

freeze : {as bs : SVDesc} → SF as bs → SF as (bs , C (SF as bs))
freeze {as} {bs} sf = λ s₁ → (sf s₁ , λ t → (λ s₂ → advance {bs} t (sf (splice {as} s₁ s₂ t))))

--------------------------------------------------------------------

constantS : {as : SVDesc} → {A : Set} → A → SF as (S A)
constantS a = const (a , const [])

never : {as : SVDesc} → {A : Set} → SF as (E A)
never = const (nothing , const [])

now : {as : SVDesc} → SF as (E Unit)
now = const (just unit , const []) 

notYet : {A : Set} → SF (E A) (E A)
notYet = first (const nothing)

filterE : {A : Set} → (A → Bool) → SF (E A) (E A)
filterE p (ma , cp) = (maybeFilter p ma , result (filterTDL p) cp)

hold : {A : Set} → A → SF (E A) (S A)
hold a = first (fromMaybe a)


private edgeAux : Time → Bool → ChangeList Bool → ChangeList Unit
        edgeAux d _      []                   = []
        edgeAux d true   ((δ , b) ∷ δbs)      = edgeAux ((d ₀+⁺ δ) >0) b δbs
        edgeAux d false  ((δ , false) ∷ δbs)  = edgeAux ((d ₀+⁺ δ) >0) false δbs
        edgeAux d false  ((δ , true) ∷ δbs)   = (d ₀+⁺ δ , unit) ∷ edgeAux O true δbs

edge : SF (S Bool) (E Unit)
edge (b , cp) = (nothing , edgeAux O b ∘ cp)

integralS : SF (S ℜ) (C ℜ)
integralS (x₀ , cp) t =   let 
                               δas   =  cp t
                               ds    =  map (_>0 ∘ fst) δas ++ t ₀-₀ lastChangeTime δas ∷ []
                               xs    =  x₀ ∷ map snd δas 
                          in
                               sumℜ (zipWith _*_ ds xs)

postulate integralC : SF (C ℜ) (C ℜ)

--------------------------------------------------------------------

delayC : {A : Set} → Time⁺ → (Time → A) → SF (C A) (C A)
delayC d f s t with compareGeqℜ₀ t (d >0)
... | less p = f t
... | geq p  = s (ℜ₀⁺₀-minus t d p)

delayS : {A : Set} → Time⁺ → A → SF (S A) (S A)
delayS d a₀ (a₁ , cp) = (a₀ , delayCP (just a₁) d cp)

delayE : {A : Set} → Time⁺ → SF (E A) (E A)
delayE d (ma , cp) = (nothing , delayCP ma d cp)

--------------------------------------------------------------------

liftC : {A B : Set} → (A → B) → SF (C A) (C B)
liftC = mapC

liftS : {A B : Set} → (A → B) → SF (S A) (S B)
liftS = mapS

liftE : {A B : Set} → (A → B) → SF (E A) (E B)
liftE = mapE

liftC2 : {A B Z : Set} → (A → B → Z) → SF (C A , C B) (C Z)
liftC2 f = uncurry (mapC2 f)

liftS2 : {A B Z : Set} → (A → B → Z) → SF (S A , S B) (S Z)
liftS2 f = uncurry (mapS2 f)

merge : {A B Z : Set} → (A → Z) → (B → Z) → (A → B → Z) → SF (E A , E B) (E Z)
merge fa fb fab = uncurry (mergeE2 fa fb fab)

join : {A B Z : Set} → (A → B → Z) → SF (E A , E B) (E Z)
join f = uncurry (joinE2 f)

sampleWithC : {A B Z : Set} → (A → B → Z) → SF (C A , E B) (E Z)
sampleWithC f = uncurry (mapCE f)

sampleWithS : {A B Z : Set} → (A → B → Z) → SF (S A , E B) (E Z)
sampleWithS f = uncurry (mapSE f)

--------------------------------------------------------------------

fromS : {A : Set} → SF (S A) (C A)
fromS = val

dfromS : {A : Set} → A → SF (S A) (C A)
dfromS a₀ s O       = a₀
dfromS a₀ s (t >0)  = leftLimit s t

postulate fix : {A : Set} → (A → A) → A
--        fix f = f (fix f)

loop : {as bs cs : SVDesc} → SF (as , cs) bs → SF bs cs → SF as bs
loop {as} {bs} sff sfb sa = fix (sfb ⋙ (λ sc → sff (sa , sc)))

-------------------------------------------------------------------

module When where

  open import Properties
  open import Logic

  PIvl : TPred → TPred
  PIvl φ t = P (λ t₀ → Over φ ⟨ t₀ , t ⟩) t

  FIvl : TPred → TPred
  FIvl φ t = F (λ t₁ → Over φ ⟨ t , t₁ ⟩) t

  Neighbourhood : TPred → TPred
  Neighbourhood φ = PIvl φ ∧ φ ∧ FIvl φ

  -- PosTrans : TPred → TPred
  -- PosTrans φ = PIvl (¬ φ) ∧ FIvl φ

  PosTrans : TPred → TPred
  PosTrans φ = PIvl (¬ φ) ∧ φ ∧ FIvl φ

  NegTrans : TPred → TPred
  NegTrans φ = PIvl φ ∧ FIvl (¬ φ)

  NoTrans : TPred → TPred
  NoTrans φ = Neighbourhood φ ∨ Neighbourhood (¬ φ)

  PosTransL : TPred → TPred
  PosTransL φ = PIvl (¬ φ) ∧ φ

  postulate finite : Set → Set

  WellBehaved : TPred → Time → Time → Set
  WellBehaved φ t₀ t₁ = finite (Σ Time (λ τ → τ ∈ ⟨ t₀ , t₁ ⟩ × PosTrans φ τ)) × Over (PosTrans φ ∨ NegTrans φ ∨ NoTrans φ) ⟨ t₀ , t₁ ⟩
--  WellBehaved φ t₀ t₁ = finite { τ | τ ∈ ⟨ t₀ , t₁ ⟩ , PosTrans φ τ } × Over (PosTrans φ ∨ NegTrans φ ∨ NoTrans φ) ⟨ t₀ , t₁ ⟩

  postulate poccs : TPred → Time → List Time
  --        poccs φ t | WellBehaved φ 0 t = [ τ | τ ∈ ⟨ O , t ⟩ , PosTrans φ τ ] ++ [ t | PosTransL φ t ]

  private postulate _-_ : Time → Time → Δt

  when : {A : Set} → (A → Bool) → SF (C A) (E A)
  when {A} p = λ s → (nothing , whenAux s)
     where
       whenAux : (Time → A) → ChangePrefix A
       whenAux s t =  let ts = poccs (isTrue ∘ p ∘ s) t
                      in zipWith (λ t₁ t₀ → (t₁ - t₀ , s t₁)) ts (O ∷ ts)
--                    in [ (t₁ - t₀, s t₁) | (t₁, t₀) ← zip ts (0 ∷ ts) ]

open When public

-------------------------------------------------------------------

-- A utility for use in expressing properties

frozenSample : {as bs : SVDesc} → SF as bs → SigVec as → Time → SF as bs
frozenSample {as} {bs} sf s t = sample {C _} (snd (freeze {as} {bs} sf s)) t

-------------------------------------------------------------------
