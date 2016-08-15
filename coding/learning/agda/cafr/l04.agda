{- 
  Computer Aided Formal Reasoning (G53CFR, G54CFR)
  Thorsten Altenkirch

  Lecture 4: Dependent types

  Instead of propagating errors at runtime we'd rather avoid them. In
  this lecture we show how dependent types can be used to implement an
  error-free version of !! using Vec (Vactors) and Fin (finite types)
  in Agda.

-}
module l04 where

open import Data.Nat

{- We define the type of Vectors where Vec A n is the n tuples of As
   or equivalently lists over A of length n. Indeed we are using the
   same construtors as for lists (Agda allows constructor overloading).
-}
data Vec (A : Set) : ℕ → Set where 
  [] : Vec A zero
  _∷_ : {n : ℕ} → A → Vec A n → Vec A (suc n)

infixr 5 _∷_

{- In future we will use the standard library:
   open import Data.Vec -}

-- some example vectors:
l1 : Vec ℕ 3
l1 = 0 ∷ 1 ∷ 2 ∷ []

l2 : Vec ℕ 2
l2 = 3 ∷ 4 ∷ []

{- We reimplement append for Vectors: 
   Note that the code is the same as for lists only the type is
   different, indeed more informative. -}
_++_ : {A : Set} → {m n : ℕ} → Vec A m → Vec A n → Vec A (m + n)
[] ++ bs = bs
(a ∷ as) ++ bs = a ∷ as ++ bs


{- To define an error-free version of !! we need to restrict the index
   set to a finite set of size n. I.e. we define the family
    Fin : ℕ → Set
   such that 
    Fin 0 = {}
    Fin 1 = { 0 }
    Fin 2 = { 0 , 1 }
    Fin 3 = { 0 , 1 , 2 }
    ...

   Indeed Fin is related to the natural numbers and has the same
   constructors but with different types.
-}

data Fin : ℕ → Set where
  zero : {n : ℕ} → Fin (suc n) 
  suc : {n : ℕ} → Fin n → Fin (suc n)

{- Examples for Fin -}
i1 : Fin 1
i1 = zero{zero}

i2 : Fin 2
i2 = suc{suc zero} i1

{- we can now implement a total version of !!  Note that the bad case
   is now impossible, since there is no element of Fin 0. Agda can
   recognize this and marks the impossible pattern with (). -}
_!!_ : {A : Set} → {n : ℕ} → Vec A n → Fin n → A
[] !! ()
(a ∷ as) !! zero = a
(a ∷ as) !! suc n = as !! n


{- To illustrate finite sets and getting used to dependent types we
   define some basic functions on Fin -}

{- max returns the maximial element in a non-empty finite set: -}
max : {n : ℕ} → Fin (suc n)
max {zero} = zero
max {suc n} = suc (max {n}) 

{- emb embeds Fin n into Fin (n+1). 
   e.g. emb {2} (suc{1} zero{0}) = suc{2} zero{1}
-}
emb : {n : ℕ} → Fin n → Fin (suc n)
emb zero = zero
emb (suc i) = suc (emb i)

{- Finally, we define inv which reverses the order of a
   given finite set. Conveniently it can be defined using max and
   emb. 

   E.g.
   inv {3} 0 = max {3} = 2
   inv {3} 1 = emb (max {1}) = 1
   inv {3} 2 = emb (emb (max {0})) = 0
-}
inv : {n : ℕ} → Fin n → Fin n
inv zero = max
inv (suc i) = emb (inv i)

{- Finally we looked at ++' the reversing version of append.
   Alas, our technique just to change the type but to keep the code
   doesn't work any longer. 

_++'_ : {A : Set} → {m n : ℕ} → Vec A m → Vec A n → Vec A (m + n)
as ++' [] = as
as ++' (b ∷ bs) = (b ∷ as) ++' bs

  leads to a type error since Agda claims that n + 0 is not equal to n
  Why is this? The Agda type checker only uses definitional equalities,
  e.g. in the case of + these are
  zero  + n = n
  suc m + n = suc (m + n)
  Other equalities such as n + zero = n require explicit proof. 
-}