{-# OPTIONS --type-in-type #-}

open import NeilPrelude hiding (first ; second ; argument ; result ; argResult ; apply ; applyTo)

module Arrow (Arr : Set → Set → Set)

       (arr'    : {A B : Set} → (A → B) → Arr A B) 
       (_>>>'_  : {A B C : Set} → Arr A B → Arr B C → Arr A C)
       (first'  : {A B C : Set} → Arr A B → Arr (A × C) (B × C))

where

----------------------------------------------

infixr 90 _>>>_ _<<<_
infixr 91 _***_
infix  91 _&&&_

_>>>_  : {A B C : Set} → Arr A B → Arr B C → Arr A C
_>>>_  = _>>>'_

_<<<_  : {A B C : Set} → Arr B C → Arr A B → Arr A C
_<<<_  = flip _>>>_

arr : {A B : Set} → (A → B) → Arr A B
arr = arr'

first : {A B C : Set} → Arr A B → Arr (A × C) (B × C)
first = first'

second : {A B C : Set} → Arr B C → Arr (A × B) (A × C)
second a = arr swap >>> first a >>> arr swap

constant : {A B : Set} → B → Arr A B
constant b = arr (const b)

_***_   : {A B C D : Set} → Arr A C → Arr B D → Arr (A × B) (C × D)
a *** b = first a >>> second b

_&&&_ : {A B C : Set} → Arr A B → Arr A C → Arr A (B × C)
a &&& b = arr dup >>> a *** b

apply : {A B C : Set} → Arr A (B → C) → Arr A B → Arr A C
apply f g = f &&& g >>> arr ×-apply

argument : {A B C : Set} → (A → B) → Arr B C → Arr A C
argument f a = arr f >>> a

result : {A B C : Set} → (B → C) → Arr A B → Arr A C
result f a = a >>> arr f 

argResult : {A B C D : Set} → (A → B) → (C → D) → Arr B C → Arr A D
argResult f g a = arr f >>> a >>> arr g

arr2 : {A B C : Set} → (A → B → C) → Arr (A × B) C
arr2 f = arr (uncurry f)

liftA : {A B : Set} → (A → B) → Arr A B
liftA = arr

liftA2 : {A B C D : Set} → (B → C → D) → Arr A B → Arr A C → Arr A D
liftA2 f a b = a &&& b >>> arr2 f

identity : {A : Set} → Arr A A
identity = arr id

applyTo : {A B C : Set} → Arr A B → Arr A (B → C) → Arr A C
applyTo = flip apply

------------------------------------------------------------

-- for use with static arrows

force : {A B : Set} → Arr Unit (A → B) → Arr A B
force f = arr (λ a → unit , a) >>> first f >>> arr ×-apply

------------------------------------------------------------
