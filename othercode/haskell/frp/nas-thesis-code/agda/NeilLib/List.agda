{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import Bool

module List where

infixr 16 _∷_
infixr 15 _++_

data List (A : Set) : Set where
  []   : List A
  _∷_  : A → List A → List A

{-# BUILTIN LIST List #-}
{-# BUILTIN NIL  []  #-}
{-# BUILTIN CONS _∷_ #-}

[_] : {A : Set} → (a : A) → List A
[ a ] = a ∷ []

--- Non-Empty Lists ----------------------

-- data NonEmptyList (A : Set) : Set where
--   _∷_ : A → List A → NonEmptyList A

--- List Functions -----------------------

wrap : {A : Set} → (a : A) → List A
wrap a = a ∷ []


listcases : {A : Set} → {P : List A → Set} → P [] → ((a : A) → (as : List A) → P (a ∷ as)) → (as : List A) → P as
listcases b _ [] = b
listcases _ f (a ∷ as) = f a as

-- foldr is the non-dependent version of listrec

foldr : {A B : Set} → (B → A → A) → A → List B → A
foldr f a []       = a
foldr f a (b ∷ bs) = f b (foldr f a bs)

foldl : {A B : Set} → (A → B → A) → A → List B → A
foldl f a []       = a
foldl f a (b ∷ bs) = foldl f (f a b) bs

_++_ : {A : Set} → List A → List A → List A
[] ++ bs = bs
(a ∷ as) ++ bs = a ∷ (as ++ bs)

map : {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (a ∷ as) = f a ∷ map f as

{-
_++_ : {A : Set} → List A → List A → List A
as ++ bs = foldr _∷_ bs as

map : {A B : Set} → (A → B) → List A → List B
map f = foldr (\a → _∷_ (f a)) []
-}

open import Maybe

head : {A : Set} → List A → Maybe A
head []       =  nothing
head (a ∷ _)  =  just a

safehead : {A : Set} → A → List A → A
safehead a = fromMaybe a ∘ head

concat : {A : Set} → List (List A) → List A
concat = foldr _++_ []

rev : {A : Set} → List A → List A → List A
rev ac []       = ac
rev ac (a ∷ as) = rev (a ∷ ac) as

reverse : {A : Set} → List A → List A
reverse as = rev [] as

slowreverse : {A : Set} → List A → List A
slowreverse []        = []
slowreverse (a ∷ as)  = slowreverse as ++ a ∷ []

partition : {A : Set} → (A → Bool) → List A → List A × List A
partition p = foldr (\a → if p a then first (_∷_ a) else second (_∷_ a)) ([] , [])

filter : {A : Set} → (A → Bool) → List A → List A
filter p [] = []
filter p (a ∷ as) = (if p a then (_∷_ a) else id) (filter p as)

null : {A : Set} → List A → Bool
null = listcases true (λ _ _ → false)

nonEmpty : {A : Set} → List A → Bool
nonEmpty = not ∘ null

zipWith : {A B C : Set} → (A → B → C) → List A → List B → List C
zipWith f [] bs = []
zipWith f as [] = []
zipWith f (a ∷ as) (b ∷ bs) = f a b ∷ zipWith f as bs

zip : {A B : Set} → List A → List B → List (A × B)
zip = zipWith _,_

unzip : {A B : Set} → List (A × B) → List A × List B
unzip = foldr (×-map2 _∷_ _∷_) ([] , [])

⋁ : List Bool → Bool
⋁ = foldr _∨_ false

⋀ : List Bool → Bool
⋀ = foldr _∧_ true

all : {A : Set} → (A → Bool) → List A → Bool
all p = ⋀ ∘ map p

any : {A : Set} → (A → Bool) → List A → Bool
any p = ⋁ ∘ map p

safeinit : {A : Set} → A → List A → List A
safeinit _ [] = []
safeinit a (b ∷ as) = a ∷ safeinit b as

init : {A : Set} → List A → List A
init [] = []
init (a ∷ as) = safeinit a as

safelast : {A : Set} → A → List A → A
safelast a []       = a
safelast _ (b ∷ as) = safelast b as

last : {A : Set} → List A → Maybe A
last []       = nothing
last (a ∷ as) = just (safelast a as)

span : {A : Set} → (A → Bool) → List A → List A × List A
span p [] = [] , []
span p (a ∷ as) = if p a
                   then first (_∷_ a) (span p as)
                   else ([] , a ∷ as)

takeWhile : {A : Set} → (A → Bool) → List A → List A
takeWhile p = fst ∘ span p

dropWhile : {A : Set} → (A → Bool) → List A → List A
dropWhile p = snd ∘ span p

-- The accumulator is a sort of state passed along the computation

mapAccumL : {B C : Set} → {Acc : Set} → (Acc → B → Acc × C) → Acc → List B → Acc × List C
mapAccumL f ac [] =  ac , []
mapAccumL f ac (b ∷ bs) with f ac b
... | ac' , c  =  second (_∷_ c) (mapAccumL f ac' bs)

open import Logic

Null : {A : Set} → List A → Set
Null = Sat null

