{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude
open import List
open import ListProps
open import Nat
open import Vector

module HetroVector where

infixr 16 _∷_

---------------------------------------

data HetVec {A : Set} (eType : A → Set) : (List A) → Set where
  ⟨⟩   : HetVec eType []
  _∷_  : {a : A} → {as : List A} → (eType a) → HetVec eType as → HetVec eType (a ∷ as)

data HetVec2 {A B : Set} (eType : A → B → Set) : List A → List B → Set where
  ⟨⟩   : HetVec2 eType [] []
  _∷_  : {a : A} → {b : B} → {as : List A} → {bs : List B} → eType a b → HetVec2 eType as bs → HetVec2 eType (a ∷ as) (b ∷ bs)

---------------------------------------

module HetroVectorAux {T : Set} {eType : T → Set} where

  infixr 15 _+++_ 

  HetVec' = HetVec eType

  ⟨_⟩ : {A : T} → eType A → HetVec' [ A ]
  ⟨ a ⟩ = a ∷ ⟨⟩

  substhv : {AS BS : List T} → AS ≡ BS → HetVec' AS → HetVec' BS
  substhv refl as = as

  hvec : {AS : List T} → ({A : T} → eType A) → HetVec' AS
  hvec {[]} a = ⟨⟩
  hvec {_ ∷ _} a = a ∷ hvec a

  hvrev : {AS BS : List T} → HetVec' AS → HetVec' BS → HetVec' (reverse BS ++ AS)
  hvrev acc ⟨⟩ = acc
  hvrev {_} {B ∷ BS} acc (b ∷ bs) = substhv (++reverseAuxR {_} {BS}) (hvrev (b ∷ acc) bs)

  hvreverse : {AS : List T} → HetVec' AS → HetVec' (reverse AS)
  hvreverse v = substhv ⊕unit (hvrev ⟨⟩ v)

  _+++_ : {AS BS : List T} → HetVec' AS → HetVec' BS → HetVec' (AS ++ BS)
  ⟨⟩     +++ bs = bs
  a ∷ as +++ bs = a ∷ (as +++ bs)

  hvhead : {A : T} → {AS : List T} → HetVec' (A ∷ AS) → eType A
  hvhead (a ∷ _) = a

  hvtail : {A : T} → {AS : List T} → HetVec' (A ∷ AS) → HetVec' AS
  hvtail (_ ∷ as) = as

  hvinit : {A : T} → {AS : List T} → HetVec' (A ∷ AS) → HetVec' (safeinit A AS)
  hvinit (a ∷ ⟨⟩) = ⟨⟩
  hvinit (a ∷ b ∷ as) = a ∷ hvinit (b ∷ as)

  hvlast : {A : T} → {AS : List T} → HetVec' (A ∷ AS) → (eType (safelast A AS))
  hvlast (a ∷ ⟨⟩) = a
  hvlast (_ ∷ b ∷ as) = hvlast (b ∷ as)

  hvheadtail : {A : T} → {AS : List T} → HetVec' (A ∷ AS) → eType A × HetVec' AS
  hvheadtail (a ∷ as) = a , as

  hvunwrap : {A : T} → HetVec' [ A ] → eType A
  hvunwrap = hvhead

  hvunwrap2 : {A B : T} → HetVec' (A ∷ B ∷ []) → eType A × eType B
  hvunwrap2 (a ∷ b ∷ ⟨⟩) = a , b

  hvwrap : {A : T} → eType A → HetVec' [ A ]
  hvwrap = ⟨_⟩

  hvwrap2 : {A B : T} → eType A → eType B → HetVec' (A ∷ B ∷ [])
  hvwrap2 a b = a ∷ b ∷ ⟨⟩

  hvsplit : {AS BS : List T} → HetVec' (AS ++ BS) → HetVec' AS × HetVec' BS
  hvsplit {[]} bs = ⟨⟩ , bs
  hvsplit {A ∷ AS} (a ∷ asbs) = first (_∷_ a) (hvsplit {AS} asbs)

  hvtake : {AS BS : List T} → HetVec' (AS ++ BS) → HetVec' AS
  hvtake = fst ∘ hvsplit 

  hvdrop : {BS : List T} → (AS : List T) → HetVec' (AS ++ BS) → HetVec' BS
  hvdrop AS = snd ∘ hvsplit {AS}

  hvproject : {BS CS : List T} → (AS : List T) → HetVec' (AS ++ BS ++ CS) → HetVec' BS
  hvproject AS = hvtake ∘ hvdrop AS

  hvmaptoV : {AS : List T} → {B : Set} → ((a : T) → eType a → B) → HetVec' AS → Vec B (length AS)
  hvmaptoV {[]} f ⟨⟩ = []
  hvmaptoV {A ∷ AS} f (a ∷ as) = f A a ∷ hvmaptoV f as

  hvmaptoL : {AS : List T} → {B : Set} → ((a : T) → eType a → B) → HetVec' AS → List B
  hvmaptoL f = vecToList ∘ hvmaptoV f

  hvany : {as : List T} → ((t : T) → eType t → Bool) → HetVec' as → Bool
  hvany f = ⋁ ∘ hvmaptoL f

--------------------------------------

open HetroVectorAux public

--------------------------------------

NTuple : List Set → Set
NTuple = HetVec {Set} id

hvecMap : {A B : Set} → {eType : B → Set} → {f : A → B} → ((a : A) → eType (f a)) → (as : List A) → HetVec eType (map f as)
hvecMap {_} {_} g []       = ⟨⟩
hvecMap {_} {_} g (a ∷ as) = g a ∷ hvecMap g as

hvmapB' : {A B Y : Set} → {F : Y → A} → {G : Y → B} → {eTypeA : A → Set} → {eTypeB : B → Set}
         → (YS : List Y) → ((y : Y) → eTypeA (F y) → eTypeB (G y)) → HetVec eTypeA (map F YS) → HetVec eTypeB (map G YS)
hvmapB' [] f ⟨⟩ = ⟨⟩
hvmapB' (A ∷ AS) f (a ∷ as) = f A a ∷ hvmapB' AS f as

hvmapB : {A B Y : Set} → {F : Y → A} → {G : Y → B} → {eTypeA : A → Set} → {eTypeB : B → Set}
        → (YS : List Y) → ({y : Y} → eTypeA (F y) → eTypeB (G y)) → HetVec eTypeA (map F YS) → HetVec eTypeB (map G YS)
hvmapB AS f = hvmapB' AS (λ A → f {A})

hvmapR : {A B : Set} → {AS : List A} → {F : A → B} → {eTypeA : A → Set} → {eTypeB : B → Set}
        → ({a : A} → eTypeA a → eTypeB (F a)) → HetVec eTypeA AS → HetVec eTypeB (map F AS)
hvmapR {_} {_} {AS} f = hvmapB AS f ∘ substhv (sym mapId)

hvmapL' : {A B : Set} → {BS : List B} → {F : B → A} → {eTypeA : A → Set} → {eTypeB : B → Set}
         → ((b : B) → eTypeA (F b) → eTypeB b) → HetVec eTypeA (map F BS) → HetVec eTypeB BS
hvmapL' {_} {_} {BS} f = substhv mapId ∘ hvmapB' BS f

hvmapL : {A B : Set} → {BS : List B} → {F : B → A} → {eTypeA : A → Set} → {eTypeB : B → Set}
        → ({b : B} → eTypeA (F b) → eTypeB b) → HetVec eTypeA (map F BS) → HetVec eTypeB BS
hvmapL {_} {_} {BS} f = hvmapL' (λ B → f {B})

hvmap : {A : Set} → {AS : List A} → {eType₁ : A → Set} → {eType₂ : A → Set} → ({a : A} → eType₁ a → eType₂ a) → HetVec eType₁ AS → HetVec eType₂ AS
hvmap f = substhv mapId ∘ hvmapR f


hvzipWith' : {A B : Set} → {F : A → B} → {AS : List A} → {eType₁ : A → Set} → {eType₂ : A → Set} → {eTypeB : B → Set}
             → ({a : A} → eType₁ a → eType₂ a → eTypeB (F a)) → HetVec eType₁ AS → HetVec eType₂ AS → HetVec eTypeB (map F AS) 
hvzipWith' f ⟨⟩ ⟨⟩ = ⟨⟩
hvzipWith' f (a ∷ as) (b ∷ bs) = f a b ∷ hvzipWith' f as bs


hvzipWith : {A : Set} → {AS : List A} → {eType₁ : A → Set} → {eType₂ : A → Set} → {eType₃ : A → Set}
             → ({a : A} → eType₁ a → eType₂ a → eType₃ a) → HetVec eType₁ AS → HetVec eType₂ AS → HetVec eType₃ AS
hvzipWith f as bs = substhv mapId (hvzipWith' f as bs)



-- HetVec2 = Hetrogeneous Vectors parameterised by two lists (of equal length) [ maybe we should use vectors? ] -------------------------
-- well, lists of pairs is the obvious solution, but two lists seems to be what's desired in practice

hv2map : {A B : Set} → {as : List A} → {bs : List B} → {P Q : A → B → Set} → ({a : A} → {b : B} → P a b → Q a b) → HetVec2 P as bs → HetVec2 Q as bs
hv2map f ⟨⟩ = ⟨⟩
hv2map f (a ∷ as) = f a ∷ hv2map f as

hvec2 : {A : Set} → {as : List A} → {P : A → A → Set} → ({a : A} → P a a) → HetVec2 P as as
hvec2 {_} {[]}    p = ⟨⟩
hvec2 {_} {_ ∷ _} p = p ∷ hvec2 p

hv2wrap : {A B : Set} -> {P : A -> B -> Set} -> {a : A} -> {b : B} -> P a b -> HetVec2 P [ a ] [ b ]
hv2wrap a = a ∷ ⟨⟩ 

-- I need to rewrite these in terms of the general case at some point
-- some hvrec would be nice too

-- hvecMap2 : {A B : Set} → {as : List A} → {P : A → B → Set} → {f : A → B} → ({a : A} → P a (f a)) → HetVec2 P as (map f as)
-- hvecMap2 {_} {_} {[]}    p = ⟨⟩
-- hvecMap2 {_} {_} {_ ∷ _} p = p ∷ hvecMap2 p

hvec2R : {A B : Set} → {as : List A} → {P : A → B → Set} → {f : A → B} → ({a : A} → P a (f a)) → HetVec2 P as (map f as)
hvec2R {_} {_} {[]}    p = ⟨⟩
hvec2R {_} {_} {_ ∷ _} p = p ∷ hvec2R p

hvec2L : {A B : Set} → {as : List A} → {P : B → A → Set} → {f : A → B} → ({a : A} → P (f a) a) → HetVec2 P (map f as) as
hvec2L {_} {_} {[]}    p = ⟨⟩
hvec2L {_} {_} {_ ∷ _} p = p ∷ hvec2L p

hvec2B : {A B C : Set} → {P : B → C → Set} → {f : A → B} → {g : A → C} → ({a : A} → P (f a) (g a)) → (as : List A) → HetVec2 P (map f as) (map g as)
hvec2B p [] = ⟨⟩
hvec2B p (_ ∷ as) = p ∷ hvec2B p as

hv2zipWithhv1 : {A B : Set} → {as : List A} → {bs : List B} → {P : A → B → Set} → {Q : A → Set} → {R : B → Set} → ({a : A} → {b : B} → P a b → Q a → R b) → HetVec2 P as bs → HetVec Q as → HetVec R bs
hv2zipWithhv1 f ⟨⟩ ⟨⟩ = ⟨⟩
hv2zipWithhv1 f (p ∷ ps) (q ∷ qs) = f p q ∷ hv2zipWithhv1 f ps qs


-- Properties of "All" list elements ------------------------------------

All : {A : Set} → (P : A → Set) → List A → Set
All = HetVec

-- This is a bit awkward, but I'm not sure how to define a general list of truths

⟨T⟩ : {A : Set} → {a : A} → {P : A → Set} → {t : P a} → HetVec P [ a ]
⟨T⟩ {_} {_} {_} {t} = ⟨ t ⟩

⟨TT⟩ : {A : Set} → {a b : A} → {P : A → Set} → {t₁ : P a} → {t₂ : P b} → HetVec P (a ∷ b ∷ [])
⟨TT⟩ {_} {_} {_} {_} {t₁} {t₂} = t₁ ∷ t₂ ∷ ⟨⟩

mapWithPred : {A B : Set} → {P : A → Set} → ((a : A) → P a → B) → (as : List A) → All P as → List B
mapWithPred f [] ⟨⟩ = []
mapWithPred f (a ∷ as) (p ∷ ps) = f a p ∷ mapWithPred f as ps


All2 : {A B : Set} → (P : A → B → Set) → List A → List B → Set
All2 = HetVec2

zipWithPred : {A B C : Set} → {P : A → B → Set} → ((a : A) → (b : B) → P a b → C) → (as : List A) → (bs : List B) → All2 P as bs → List C
zipWithPred f .[] .[] ⟨⟩ = []
zipWithPred f (a ∷ as) (b ∷ bs) (p ∷ ps) = f a b p ∷ zipWithPred f as bs ps

fequivPred : {A B : Set} → {P : A → A → Set} → {F : A → B} → {as₁ as₂ : List A} → ({a₁ a₂ : A} → P a₁ a₂ → F a₁ ≡ F a₂) → All2 P as₁ as₂ → map F as₁ ≡ map F as₂
fequivPred f ⟨⟩ = refl
fequivPred f (eq ∷ eqs) = cong2 _∷_ (f eq) (fequivPred f eqs)

all2comm : {A : Set} → {P : A → A → Set} → {as bs : List A} → ({a b : A} → P a b → P b a) → All2 P as bs → All2 P bs as
all2comm _ ⟨⟩ = ⟨⟩
all2comm f (a ∷ as) = f a ∷ all2comm f as

all2idem : {A : Set} → {as : List A} → {P : A → A → Set} → {ps : All2 P as as} → {f : (a₁ a₂ : A) → P a₁ a₂ → A} → ({a : A} → {p : P a a} → f a a p ≡ a) → zipWithPred f as as ps ≡ as
all2idem {_} {[]} {_} {⟨⟩} eq = refl
all2idem {_} {_ ∷ _} {_} {_ ∷ _} eq = cong2 _∷_ eq (all2idem eq)

all2trans : {A : Set} → {as bs cs : List A} → {P : A → A → Set} → ({a b c : A} → P a b → P b c → P a c) → HetVec2 P as bs → HetVec2 P bs cs → HetVec2 P as cs
all2trans p ⟨⟩ ⟨⟩ = ⟨⟩
all2trans p (ab ∷ abs) (bc ∷ bcs) = p ab bc ∷ all2trans p abs bcs


-- all2refl = hvec2


