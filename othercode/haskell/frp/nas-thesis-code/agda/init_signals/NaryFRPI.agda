{-# OPTIONS --type-in-type #-}

module NaryFRPI where

open import NeilPrelude
open import Maybe
open import List
open import RealTime
open import Real hiding (_-_)
open import TimeDeltaList
open import SigVecsI
open import UtilitiesI
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
sf1 >>> sf2 = sf2 ∘ sf1

_&&&_ : {as bs cs : SVDesc} → SF as bs → SF as cs → SF as (bs , cs)
sf1 &&& sf2 = λ as → (sf1 as , sf2 as)

--------------------------------------------------------------------

switch  : {as bs : SVDesc} → {A : Set} → SF as (bs , E A) → (A → SF as (iniSV bs)) → SF as bs
switch {as} {bs} sf f sa with sf sa
... | sb , se = withTime {bs} (λ t → maybe sb
                                           (uncurry (λ te e → (splice {bs} sb ((f e) (advance {as} te sa)) te)))
                                           (fstOcc se t))

postulate dswitch : {as bs : SVDesc} → {A : Set} → SF as (bs , E A) → (A → SF as (uniSV bs)) → SF as bs

--------------------------------------------------------------------

-- one could also imagine a "dfreeze" that uses a "dsplice"

freeze : {as bs : SVDesc} → SF as bs → SF as (bs , C ini (SF (iniSV as) bs))
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
filterE p = maybeFilter p ∥ result (filterTDL p)

hold : {A : Set} → A → SF (E A) (S A)
hold a = first (fromMaybe a)

private edgeAux : Time → Bool → ChangeList Bool → ChangeList Unit
        edgeAux d _      []                   = []
        edgeAux d true   ((δ , b) ∷ δbs)      = edgeAux ((d ₀+⁺ δ) >0) b δbs
        edgeAux d false  ((δ , false) ∷ δbs)  = edgeAux ((d ₀+⁺ δ) >0) false δbs
        edgeAux d false  ((δ , true) ∷ δbs)   = (d ₀+⁺ δ , unit) ∷ edgeAux O true δbs

edge : SF (S Bool) (E Unit)
edge (b , cp) = (nothing , edgeAux O b ∘ cp)

integralS : SF (S ℜ) (C ini ℜ)
integralS (x₀ , cp) t =   let 
                               δas   =  cp t
                               ds    =  map (_>0 ∘ fst) δas ++ t ₀-₀ lastChangeTime δas ∷ []
                               xs    =  x₀ ∷ map snd δas 
                          in
                               sumℜ (zipWith _*_ ds xs)

postulate integralC : {i : Init} → SF (C i ℜ) (C ini ℜ)

--------------------------------------------------------------------

delayC : {i : Init} → {A : Set} → Time⁺ → (Time → A) → SF (C i A) (C ini A)
delayC {ini} d f s t with compareGeqℜ₀ t (d >0)
... | less p = f t
... | geq p  = s (ℜ₀⁺₀-minus t d p)
delayC {uni} d f s t with compareLeqℜ₀ t (d >0)
... | leq p  = f t
... | more p = s (ℜ₀⁺⁺-minus t d p)

delayS : {A : Set} → Time⁺ → A → SF (S A) (S A)
delayS d a₀ (a₁ , cp) = (a₀ , delayCP (just a₁) d cp)

delayE : {A : Set} → Time⁺ → SF (E A) (E A)
delayE d (ma , cp) = (nothing , delayCP ma d cp)

--------------------------------------------------------------------

liftC : {i : Init} → {A B : Set} → (A → B) → SF (C i A) (C i B)
liftC {ini} f s = result f s
liftC {uni} f s = result f s

liftS : {A B : Set} → (A → B) → SF (S A) (S B)
liftS = mapS

liftE : {A B : Set} → (A → B) → SF (E A) (E B)
liftE = mapE

liftC2 : {i₁ i₂ : Init} → {A B Z : Set} → (A → B → Z) → SF (C i₁ A , C i₂ B) (C (i₁ ⊓ i₂) Z)
liftC2 {ini} {ini} f (s₁ , s₂) = λ t → f (s₁ t) (s₂ t)
liftC2 {ini} {uni} f (s₁ , s₂) = λ t → f (s₁ (t >0)) (s₂ t)
liftC2 {uni} {ini} f (s₁ , s₂) = λ t → f (s₁ t) (s₂ (t >0))
liftC2 {uni} {uni} f (s₁ , s₂) = λ t → f (s₁ t) (s₂ t)

liftS2 : {A B Z : Set} → (A → B → Z) → SF (S A , S B) (S Z)
liftS2 f = uncurry (mapS2 f)

merge : {A B Z : Set} → (A → Z) → (B → Z) → (A → B → Z) → SF (E A , E B) (E Z)
merge fa fb fab = uncurry (mergeE2 fa fb fab)

join : {A B Z : Set} → (A → B → Z) → SF (E A , E B) (E Z)
join f = uncurry (joinE2 f)

sampleWithC : {i : Init} → {A B Z : Set} → (A → B → Z) → SF (C i A , E B) (E Z)
sampleWithC {i} f = uncurry (mapCE {i} f)

sampleWithS : {A B Z : Set} → (A → B → Z) → SF (S A , E B) (E Z)
sampleWithS f = uncurry (mapSE f)

--------------------------------------------------------------------

weaken : {as as' bs bs' : SVDesc} → as' <: as → bs <: bs' → SF as bs → SF as' bs'
weaken {as} {as'} {bs} {bs'} p q = argResult (weakenSV {as'} {as} p) (weakenSV {bs} {bs'} q)

fromS : {A : Set} → SF (S A) (C ini A)
fromS = val

dfromS : {A : Set} → SF (S A) (C uni A)
dfromS = leftLimit

initialise : {A : Set} → A → SF (C uni A) (C ini A)
initialise a s O       = a
initialise a s (t >0)  = s t

-------------------------------------------------------------------

postulate fix : {A : Set} → (A → A) → A
--        fix f = f (fix f)

loop : {as bs cs : SVDesc} → SF (as , cs) bs → SF bs cs → SF as bs
loop {as} {bs} sff sfb sa = fix (sfb ⋙ (λ sc → sff (sa , sc)))

-------------------------------------------------------------------

module When where

  open import Logic

  import TemporalLogic renaming (_B_ to _`Back-To`_ ; _S_ to _`Since`_)
  open TemporalLogic Time _<ℜ₀_ <ℜ₀-trans <ℜ₀-trich public

  import TemporalFunction
  open TemporalFunction Time _<ℜ₀_ <ℜ₀-trans <ℜ₀-trich public

  PIvl : TPred → TPred
  PIvl φ t = P (λ t₀ → Over φ ⟨ t₀ , t ⟩) t

  FIvl : TPred → TPred
  FIvl φ t = F (λ t₁ → Over φ ⟨ t , t₁ ⟩) t

  Neighbourhood : TPred → TPred
  Neighbourhood φ = PIvl φ ∧ φ ∧ FIvl φ

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

  postulate poccs : TPred → Time → List Time⁺
  --        poccs φ t | WellBehaved φ 0 t = [ τ | τ ∈ ⟨ O , t ⟩ , PosTrans φ τ ] ++ [ t | PosTransL φ t ]


  postulate _-_ : Time⁺ → Time → Δt

  whenIni : {A : Set} → (A → Bool) → SF (C ini A) (E A)
  whenIni {A} p s = (nothing , whenAux)
     where
       whenAux : ChangePrefix A
       whenAux t =  let ts = poccs (isTrue ∘ p ∘ s) t
                    in zipWith (λ t₁ t₀ → (t₁ - t₀ , s (t₁ >0))) ts (O ∷ map _>0 ts)
--                  in [ (t₁ - t₀, s t₁) | (t₁, t₀) ← zip ts (0 ∷ ts) ]

  whenUni : {A : Set} → (A → Bool) → SF (C uni A) (E A)
  whenUni {A} p s = (nothing , whenAux)
     where
       whenAux : ChangePrefix A
       whenAux t =  let ts = poccs (isTrue ∘ ℜ₀-elim true (p ∘ s)) t
                    in zipWith (λ t₁ t₀ → (t₁ - t₀ , s t₁)) ts (O ∷ map _>0 ts)
--                  in [ (t₁ - t₀, s t₁) | (t₁, t₀) ← zip ts (0 ∷ ts) ]

  when : {i : Init} → {A : Set} → (A → Bool) → SF (C i A) (E A)
  when {ini} = whenIni
  when {uni} = whenUni

-------------------------------------------------------------------
