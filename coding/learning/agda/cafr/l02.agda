{- 
  Computer Aided Formal Reasoning (G53CFR, G54CFR)
  Thorsten Altenkirch

  Lecture 2: A first taste of Agda

  In this lecture we start to explore the Agda system, a functional
  programming language based on Type Theory. We start with some
  ordinary examples which we could have developed in Haskell as well. 
-}

module l02 where

module myNat where

  {- Agda has no automatically loaded prelude. Hence we can start from
     scratch and define the natural numbers. Later we will use the
     standard libray. -}
  
  data ℕ : Set where -- to type ℕ we type \bn 
    zero : ℕ
    suc : (m : ℕ) → ℕ -- \-> or \to

  {- To process an Agda file we use C-c C-c from emacs. Once Agda has
     checked the file the type checker also colours the different
     symbols. -}

  {- We define addition. Note Agda's syntax for mixfix operations. The
     arguments are represented by _s -}

  _+_ : ℕ → ℕ → ℕ
  zero + n = n
  suc m + n = suc (m + n)

  {- Try to evaluate: (suc (suc zero)) + (suc (suc zero))
     by typing in C-c C-n -}

{- Better we import the librayr definition of ℕ
   This way we can type 2 instead of (suc (suc zero))
-}
  
open import Data.Nat

{- We define Lists : -}

data List (A : Set) : Set where
  [] : List A
  _∷_ : (a : A) → (as : List A) → List A

{- declare the fixity of ∷ (type \::) -}

infixr 5 _∷_

{- Two example lists -}

l1 : List ℕ
l1 = 1 ∷ 2 ∷ 3 ∷ []

l2 : List ℕ
l2 = 4 ∷ 5 ∷ []

{- implementing append (++) -}

_++_ : {A : Set} → List A → List A → List A
[] ++ bs = bs
(a ∷ as) ++ bs = a ∷ (as ++ bs)

{- Note that Agda checks wether a function is terminating.
   If we type
   (a ∷ as) ++ bs = (a ∷ as) ++ bs
   in the 2nd line Agda will complain by coloring the offending 
   function calls in red
-}

{- What does the following variant of ++ do ? -}

_++'_ : {A : Set} → List A → List A → List A
as ++' [] = as
as ++' (b ∷ bs) = (b ∷ as) ++' bs

{- Indeed it can be used to define reverse.  This way to implement
   reverse is often called fast reverse because it is "tail recursive"
   which leads to a more efficient execution than the naive
   implementation. -}

rev : {A : Set} → List A → List A
rev as = [] ++' as

{- We tried to define a function which accesses the nth element of a list:

_!!_ : {A : Set} → List A → ℕ → A
[] !! n = {!!}
(a ∷ as) !! zero = a
(a ∷ as) !! suc n = as !! n

  but there is no way to complete the first line (consider what happens
  if A is the empty type!  
-}

{- To fix this we handle errors explicitely, using Maybe -}

open import Data.Maybe

{- This version of the function can either return an element of the list 
    (just a) or an error (nothing).
-}
_!!_ : {A : Set} → List A → ℕ → Maybe A
[] !! n = nothing
(a ∷ as) !! zero = just a
(a ∷ as) !! suc n = as !! n


