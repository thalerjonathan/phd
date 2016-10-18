{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import NatProps
open import List
open import Bool
open import Nat

module Vector where

infixr 14 _+m+_
infixr 15 _+v+_
infixr 16 _∷_

data Vec (A : Set) : ℕ → Set where
  [] : Vec A O
  _∷_  : {n : ℕ} → A → Vec A n → Vec A (S n)

data Fin : ℕ → Set where
  fO : {n : ℕ} → Fin (S n)
  fS : {n : ℕ} → Fin n → Fin (S n)


vecrec : {A : Set} → {P : {m : ℕ} → Vec A m → Set} → {n : ℕ} → P [] → ({m : ℕ} → {a : A} → {as : Vec A m} → P as → P (a ∷ as)) → Π (Vec A n) P
vecrec base step [] = base
vecrec {_} {P} base step (a ∷ as) = step (vecrec {_} {P} base step as) 

vecrec' : {A : Set} → {P : {m : ℕ} → Vec A m → Set} → {n : ℕ} → P [] → ({m : ℕ} → (a : A) → (as : Vec A m) → P as → P (a ∷ as)) → Π (Vec A n) P
vecrec' base step = vecrec base (λ {_} {a} {as} → step a as)

veccases : {A : Set} → {P : {m : ℕ} → Vec A m → Set} → {n : ℕ} → P [] → ({m : ℕ} → (a : A) → (as : Vec A m) → P (a ∷ as)) → Π (Vec A n) P
veccases {_} {P} b f = vecrec' {_} {P} b (λ a as _ → f a as)

vec : {n : ℕ} → {A : Set} → A → Vec A n
vec {O} a = []
vec {S n} a = a ∷ vec a

vapply : {A B : Set} → {n : ℕ} → Vec (A → B) n → Vec A n → Vec B n
vapply [] [] = []
vapply (f ∷ fs) (a ∷ as) = f a ∷ vapply fs as

vmap : {A B : Set} → {n : ℕ} → (A → B) → Vec A n → Vec B n
vmap = vapply ∘ vec

vzipWith : {A B C : Set} → {n : ℕ} → (A → B → C) → Vec A n → Vec B n → Vec C n
vzipWith f as = vapply (vmap f as)

vzip    : {A B : Set} → {n : ℕ} → Vec A n → Vec B n → Vec (A × B) n
vzip    = vzipWith _,_

vunzipWith : {A B C : Set} → {n : ℕ} → (A → B × C) → Vec A n → Vec B n × Vec C n
vunzipWith f [] = [] , []
vunzipWith f (a ∷ as) = ×-map2 _∷_ _∷_ (f a) (vunzipWith f as)

vunzip : {A B : Set} → {n : ℕ} → Vec (A × B) n → Vec A n × Vec B n
vunzip = vunzipWith id

substvlength : {A : Set} → {m n : ℕ} → m ≡ n → Vec A m → Vec A n
substvlength refl as = as

open import Nat

vrev : {m n : ℕ} → {A : Set} → Vec A m → Vec A n → Vec A (n + m)
vrev acc [] = acc
vrev {m} {S n} acc (x ∷ xs) = substvlength (trans (+comm n) (cong S (+comm m))) (vrev (x ∷ acc) xs)

vreverse : {A : Set} → {n : ℕ} → Vec A n → Vec A n
vreverse v = substvlength +O (vrev [] v)

vecToList : {n : ℕ} → {A : Set} → Vec A n → List A
vecToList [] = []
vecToList (a ∷ as) = a ∷ vecToList as

listToVec : {A : Set} → (l : List A) → Vec A (length l)
listToVec [] = []
listToVec (a ∷ as) = a ∷ listToVec as

vfoldr : {n : ℕ} → {A B : Set} → (A → B → B) → B → Vec A n → B
vfoldr f b = foldr f b ∘ vecToList

vfoldl   : {n : ℕ} → {A B : Set} → (A → B → A) → A → Vec B n → A
vfoldl f a = foldl f a ∘ vecToList

vfoldr1 : {n : ℕ} → {A : Set} → (A → A → A) → Vec A (S n) → A
vfoldr1 f (a ∷ as) = vfoldr f a as

vfoldl1 : {n : ℕ} → {A : Set} → (A → A → A) → Vec A (S n) → A
vfoldl1 f (a ∷ as) = vfoldl f a as

_+v+_ : {A : Set} → {m n : ℕ} → Vec A m → Vec A n → Vec A (m + n)
[] +v+ bs     = bs
a ∷ as +v+ bs = a ∷ (as +v+ bs)

vhead : {n : ℕ} → {A : Set} → Vec A (S n) → A
vhead (a ∷ as) = a

vtail : {n : ℕ} → {A : Set} → Vec A (S n) → Vec A n
vtail (a ∷ as) = as

vlast : {n : ℕ} → {A : Set} → Vec A (S n) → A
vlast {O} (a ∷ []) = a
vlast {S n} (a ∷ as) = vlast as

vinit : {n : ℕ} → {A : Set} → Vec A (S n) → Vec A n
vinit {O} (a ∷ []) = []
vinit {S n} (a ∷ as) = a ∷ vinit as

vheadtail : {n : ℕ} → {A : Set} → Vec A (S n) → A × Vec A n
vheadtail (a ∷ as) = a , as

vcount : {A : Set} → {n : ℕ} → (A → Bool) → Vec A n → ℕ
vcount p = count p ∘ vecToList 

vpartition : {A : Set} → {n : ℕ} → (A → Bool) → Vec A n → Σ (ℕ × ℕ) (uncurry (λ l r → l + r ≡ n × Vec A l × Vec A r))
vpartition p [] = (O , O) , refl , [] , []
vpartition p (a ∷ as) with vpartition p as
... | (l , r) , eq , pass , fail = if p a
                                    then (S l , r) , cong S eq , a ∷ pass , fail
                                    else (l , S r) , trans (sym (+Scomm l)) (cong S eq) , pass , a ∷ fail

vfilterAux : {A : Set} → {n : ℕ} → (A → Bool) → Vec A n → Σ ℕ (Vec A)
vfilterAux p as with vpartition p as
... | (l , _) , _ , pass , _ = l , pass

vfilter : {A : Set} → {n : ℕ} → (p : A → Bool) → (v : Vec A n) → Vec A (fst (vfilterAux p v))
vfilter p v = snd (vfilterAux p v)

vsplit : {m n : ℕ} → {A : Set} → Vec A (m + n) → Vec A m × Vec A n
vsplit {O} v = [] , v
vsplit {S m} (a ∷ as) = first (_∷_ a) (vsplit as)

-- vmapAccumL starts from the left (the head of the vector)

vmapAccumL : {n : ℕ} → {B C Acc : Set} → (Acc → B → Acc × C) → Acc → Vec B n → Acc × Vec C n
vmapAccumL f acc [] =  acc , []
vmapAccumL f acc (b ∷ bs) with f acc b
... | acc' , c  =  second (_∷_ c) (vmapAccumL f acc' bs)

-- vmapAccumR accumulates starting at the right (the end of the vector)

vmapAccumR : {n : ℕ} → {B C Acc : Set} → (Acc → B → Acc × C) → Acc → Vec B n → Acc × Vec C n
vmapAccumR f acc [] = acc , []
vmapAccumR f acc (b ∷ bs) with vmapAccumR f acc bs
... | acc' , cs = second (λ c → c ∷ cs) (f acc' b)

---------- Matrices ----------

-- An m x n matrix has m rows and n columns
-- Thus co-ordinates are given (down,right) {i.e. (y,x)}

-- A matrix built of vectors is a vector of columns
--  _ _
-- |1|4|
-- |2|5|
-- |3|6|
--  - -


Matrix : Set → ℕ → ℕ → Set
Matrix A rows cols = Vec (Vec A rows) cols

transpose : {A : Set} → {m n : ℕ} → Matrix A m n → Matrix A n m
transpose [] = vec []
transpose (col ∷ cols) = vzipWith _∷_ col (transpose cols)

substmdim : {A : Set} → {m n m' n' : ℕ} → m ≡ m' → n ≡ n' → Matrix A m n → Matrix A m' n'
substmdim refl refl = id

substmrows : {A : Set} → {m m' n : ℕ} → m ≡ m' → Matrix A m n → Matrix A m' n
substmrows refl = id

substmcols : {A : Set} → {m n n' : ℕ} → n ≡ n' → Matrix A m n → Matrix A m n'
substmcols refl = id

mat : {m n : ℕ} → {A : Set} → A → Matrix A m n  
mat = vec ∘ vec

mmap : {A B : Set} → {m n : ℕ} → (A → B) → Matrix A m n → Matrix B m n
mmap f = vmap (vmap f)

mzipWith : {A B C : Set} → {m n : ℕ} → (A → B → C) → Matrix A m n → Matrix B m n → Matrix C m n
mzipWith f = vzipWith (vzipWith f)

-- +m+ is concatenation of matrices vertically
-- (you can concatenate horizontally using vector concatenation)

_+m+_ : {A : Set} {l m n : ℕ} → Matrix A l n → Matrix A m n → Matrix A (l + m) n
m1 +m+ m2 = transpose (transpose m1 +v+ transpose m2)

msplit : {l m n : ℕ} → {A : Set} → Matrix A (l + m) n → Matrix A l n × Matrix A m n
msplit = vunzip ∘ vmap vsplit

1sthalf : {l m n : ℕ} → {A : Set} → Matrix A (l + m) n → Matrix A l n
1sthalf = fst ∘ msplit

2ndhalf : {l m n : ℕ} → {A : Set} → Matrix A (l + m) n → Matrix A m n
2ndhalf {l} = snd ∘ msplit {l}


--     n₁    n₂
-- m₁  1st | 2nd
-- m₂  3rd | 4th

-- : (1st × 3rd) × (2nd × 4th)

mquarter : {m₁ n₁ m₂ n₂ : ℕ} → {A : Set} → Matrix A (m₁ + m₂) (n₁ + n₂) → (Matrix A m₁ n₁ ×
                                                                           Matrix A m₂ n₁) ×
                                                                          (Matrix A m₁ n₂ ×
                                                                           Matrix A m₂ n₂)
mquarter = msplit ∥ msplit ∘ vsplit

1stquadrant : {m₁ n₁ m₂ n₂ : ℕ} → {A : Set} → Matrix A (m₁ + m₂) (n₁ + n₂) → Matrix A m₁ n₁
1stquadrant = fst ∘ fst ∘ mquarter

2ndquadrant : {m₁ n₁ m₂ n₂ : ℕ} → {A : Set} → Matrix A (m₁ + m₂) (n₁ + n₂) → Matrix A m₁ n₂
2ndquadrant {_} {n₁} = fst ∘ snd ∘ mquarter {_} {n₁}

3rdquadrant : {m₁ n₁ m₂ n₂ : ℕ} → {A : Set} → Matrix A (m₁ + m₂) (n₁ + n₂) → Matrix A m₂ n₁
3rdquadrant {m₁} = snd ∘ fst ∘ mquarter {m₁}

4thquadrant : {m₁ n₁ m₂ n₂ : ℕ} → {A : Set} → Matrix A (m₁ + m₂) (n₁ + n₂) → Matrix A m₂ n₂
4thquadrant {m₁} {n₁} = snd ∘ snd ∘ mquarter {m₁} {n₁}