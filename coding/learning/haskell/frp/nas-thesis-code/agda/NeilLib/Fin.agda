{-# OPTIONS --type-in-type #-}

open import NeilPrelude 
open import Nat

module Fin where

open import Vector
open import Recursion


-- Finite Sets --

-- Defined in NeilPrelude

finToℕ : {n : ℕ} → Fin n → ℕ
finToℕ fO = O
finToℕ (fS fn) = S (finToℕ fn)

ℕtoFin : (n : ℕ) → Fin (S n)
ℕtoFin = natrec fO fS

fmax : {n : ℕ} → Fin (S n)
fmax {n} = ℕtoFin n

ftweak : {n : ℕ} → Fin n → Fin (S n)
ftweak fO = fO
ftweak (fS i) = fS (ftweak i)

ℕFinLem' : (n : ℕ) → finToℕ (ℕtoFin n) ≡ n
ℕFinLem' = natrec refl (cong S)

ℕFinLem : {n : ℕ} → finToℕ (ℕtoFin n) ≡ n
ℕFinLem {n} = ℕFinLem' n

fextendFunc : {n : ℕ} → {A : Set} → A → (Fin n → A) → Fin (S n) → A
fextendFunc {O} a f fO = a
fextendFunc {S n} a f fO = f fO
fextendFunc {O} a f (fS ())
fextendFunc {S n} a f (fS fn) = fextendFunc a (f ∘ fS) fn

fS-inj : {n : ℕ} → Injective (fS {n})
fS-inj refl = refl

data _<f_ : {n : ℕ} → Fin n → Fin n → Set where
  <O : {n : ℕ} → {m : Fin (S n)} → fO <f fS m 
  <S : {n : ℕ} → {l m : Fin n} → l <f m → fS l <f fS m

<O' : {n : ℕ} → {m : Fin n} → (fO <f fS m)
<O' {._} {fO} = <O
<O' {._} {fS _} = <O

<f-trans : {n : ℕ} → Transitive (_<f_ {n})
<f-trans <O     (<S _) = <O
<f-trans (<S p) (<S q) = <S (<f-trans p q)

≮O : {n : ℕ} → {m : Fin (S n)} → Not (m <f fO)
≮O ()

S≮O : {n : ℕ} → {m : Fin n} → Not (fS m <f fO)
S≮O ()

O≠S : {n : ℕ} → {m : Fin n} → fO ≢ fS m
O≠S ()

S≠O : {n : ℕ} → {m : Fin n} → fS m ≢ fO
S≠O ()

<f-injS : {n : ℕ} → {l m : Fin n} → fS l <f fS m → l <f m
<f-injS (<S p) = p

<f-trich : {n : ℕ} → Trichotomous {Fin n} _<f_
<f-trich .{_} {fO} {fO} = inl (refl , ≮O , ≮O)
<f-trich .{_} {fO} {fS m} = inrl (O≠S , <O' , S≮O)
<f-trich .{_} {fS l} {fO} = inrr (S≠O , ≮O , <O')
<f-trich .{_} {fS l} {fS m} with <f-trich {_} {l} {m}
... | inl (eq , mn , nm) = inl (cong fS eq , mn ∘ <f-injS , nm ∘ <f-injS)
... | inr (inl (eq , mn , nm)) = inrl (eq ∘ fS-inj , <S mn , nm ∘ <f-injS)
... | inr (inr (eq , mn , nm)) = inrr (eq ∘ fS-inj , mn ∘ <f-injS , <S nm)

module FinSTO {n : ℕ} where

  import StrictTotalOrder
  open StrictTotalOrder (_<f_ {S n}) <f-trans <f-trich public

open FinSTO public

{-

-- I think this is all dead code now, as its all in StrictTotalOrder

data _≤₂_ : {n : ℕ} → Fin n → Fin n → Set where
  ≤O : {n : ℕ} → {fn : Fin (S n)} → fO ≤₂ fn 
  ≤S : {n : ℕ} → {fm fn : Fin n} → fm ≤₂ fn → fS fm ≤₂ fS fn

_<₂_ : {n : ℕ} → Fin n → Fin n → Set
fm <₂ fn = fS fm ≤₂ ftweak fn

CompareFin : {n : ℕ} → (fn₁ fn₂ : Fin n) → Set
CompareFin = OrdCompare _<₂_

comparefS : {n : ℕ} → {fm fn : Fin n} → CompareFin fm fn → CompareFin (fS fm) (fS fn)
comparefS refl      = refl
comparefS (less mn) = less (≤S mn)
comparefS (more mn) = more (≤S mn)

compareFin : {n : ℕ} → (fn₁ fn₂ : Fin n) → OrdCompare _<₂_ fn₁ fn₂
compareFin fO fO = refl
compareFin fO (fS fn) = less (≤S ≤O)
compareFin (fS fm) fO = more (≤S ≤O)
compareFin (fS fm) (fS fn) with compareFin fm fn
compareFin (fS .fn) (fS fn) | refl    = refl
compareFin (fS fm) (fS fn)  | less lt = comparefS (less lt)
compareFin (fS fm) (fS fn)  | more lt = comparefS (more lt)
-}

-- we untweak fm

funtweak : {n : ℕ} → (m : Fin (S n)) → m < fmax → Fin n
funtweak {O} m ()
funtweak {S n} fO p = fO
funtweak {S n} (fS m) (<S p) = fS (funtweak m p)


-- vector manipulation --------------

vproj : {A : Set} → {n : ℕ} → Vec A n → Fin n → A
vproj [] ()
vproj (a ∷ as) fO = a
vproj (a ∷ as) (fS i) = vproj as i

vdelete : {A : Set} → {n : ℕ} → Fin (S n) → Vec A (S n) → Vec A n
vdelete fO (_ ∷ as) = as
vdelete {_} {O} (fS ()) _
vdelete {_} {S _} (fS fn) (a ∷ as) = a ∷ vdelete fn as 

vinsert : {A : Set} → {n : ℕ} → A → Fin (S n) → Vec A n → Vec A (S n)
vinsert a fO bs = a ∷ bs
vinsert a (fS ()) []
vinsert a (fS fn) (b ∷ bs) = b ∷ vinsert a fn bs

fenum : {n : ℕ} → Vec (Fin n) n
fenum {O} = []
fenum {S n} = fmax ∷ vmap ftweak fenum

vtab : {A : Set} → {n : ℕ} → (Fin n → A) → Vec A n
vtab f = vreverse (vmap f fenum)

minusfn : (n : ℕ) → Fin n → ℕ
minusfn O ()
minusfn (S n) fO = S n
minusfn (S n) (fS fn) = minusfn n fn

vtake : {A : Set} → {n : ℕ} → (fn : Fin n) → Vec A n → Vec A (finToℕ fn)
vtake fO as = []
vtake (fS fn) (a ∷ as) = a ∷ vtake fn as

vdrop : {A : Set} → {n : ℕ} → (fn : Fin n) → Vec A n → Vec A (minusfn n fn)
vdrop fO as = as
vdrop (fS fn) (a ∷ as) = vdrop fn as


vprojinsert : {n : ℕ} → {A : Set} → {a : A} → {as : Vec A n} → (fn : Fin (S n)) → vproj (vinsert a fn as) fn ≡ a
vprojinsert fO = refl
vprojinsert {O} (fS ())
vprojinsert {S _} {_} {_} {_ ∷ _} (fS fn) = vprojinsert fn

