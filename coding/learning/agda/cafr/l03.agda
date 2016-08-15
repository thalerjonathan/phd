{- 
  Computer Aided Formal Reasoning (G53CFR, G54CFR)
  Thorsten Altenkirch

  Lecture 3: Error propagation in Agda

  Haskell is supposed to be a pure functional programming langauge but
  in reality it isn't pure but features at least two effects: error
  and divergence. Errors arise when we use incomplete patterns as in
  the implementation of !! in the Haskell prelude.

  In contrast Agda is pure and error propagation has to be made
  explicit using the Maybe monad. In this lecture we look at the
  insert function as an example of how to implement error propagation
  in Agda.
-}

module l03 where

open import Data.Nat
open import Data.List
open import Data.Maybe

{- In the previous lecture we used Maybe to handle errors
   when accessing the nth element of a list: -}

_!!_ : {A : Set} → List A → ℕ → Maybe A
[] !! n = nothing
(a ∷ as) !! zero = just a
(a ∷ as) !! suc n = as !! n

{- A more interesting example is the insert operation
   which inserts a list at the nth position, e.g.
   insert (1 ∷ 2 ∷ 3 ∷ []) 1 (5 ∷ 6 ∷ []) 
   = just (1 ∷ 5 ∷ 6 ∷ 2 ∷ 3 ∷ [])
-}

insert : {A : Set} → List A → ℕ → List A → Maybe (List A)
insert as zero bs = just (bs ++ as)
insert [] (suc n) bs = nothing
{- In Haskell we would just write
   insert (a ∷ as) (suc n) bs = insert as n bs
   but in Agda we have to implement error propagation explicitely.
   We are using the 'with' construct to pattern match over a 
   recursively computed result. -}
insert (a ∷ as) (suc n) bs with insert as n bs
insert (a ∷ as) (suc n) bs | just cs = just (a ∷ cs)
insert (a ∷ as) (suc n) bs | nothing = nothing

{- We are exploiting here the fact that Maybe is a monad which 
   can be used to simulate computations with error. -}