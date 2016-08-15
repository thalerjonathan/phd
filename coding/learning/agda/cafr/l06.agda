{- 
  Computer Aided Formal Reasoning (G53CFR, G54CFR)
  Thorsten Altenkirch

  Lecture 6: Classical principles

  In this lecture we are exploring principles which follow from the
  classical identification of Prop and Bool.

-}
module l06 where

  open import l05 -- we are going to use the definitions from the previous lecture

{- we start with a warm up: proving the basic combinators of combinatory logic -}

  K : {P Q : Prp} → P ⇒ Q ⇒ P
--  K = λ p → λ q → p
  K p q = p

  S : {P Q R : Prp} → (P ⇒ Q ⇒ R) ⇒ (P ⇒ Q) ⇒ (P ⇒ R)
  S = λ pqr → λ pq → λ p → pqr p (pq p)

  {- S, K are universal for propositional logic with ⇒ only. That
  means we can prove any provable proposition just containing ⇒ just
  by combining S and K. As an example we prove I (there is just a
  minor glitch that we have to instatiate a propositional variable
  explicitely. -}

  II : {P : Prp} → P ⇒ P
  II {P} = S K (K {Q = ⊤})

  {- Combinatory logic can also be extended to the other
  connectives. An example is orcase a basic combinator for
  disjunction. -}

  orcase : {P Q R : Prp} → (P ⇒ R) ⇒ (Q ⇒ R) ⇒ (P ∨ Q ⇒ R)
  orcase pr qr (left p) = pr p
  orcase pr qr (right q) = qr q

{-
  We noticed in the previous lecture that the last de Morgan law is not provable:

  deMorgan¬∧ : {P Q : Prp} → ¬ (P ∧ Q) ⇒ (¬ P) ∨ (¬ Q)
  deMorgan¬∧ npq = right (λ q → npq ({!!} , q))

  Here are some other troublesome tautologies:
-}

  {- TND = tertium non datur (excluded middle) 
     states that every proposition is either true or false. It clearly
     expresses the idea that Prop = Bool. -}

  TND : Set₁
  TND = {P : Prp} → P ∨ ¬ P

{- Any attempt to prove TND fails quickly. How would we know wether P
   is true or false without even looking at it. Actually even if we
   were allowed to look at it, we don't know for every proposition
   wether it is true or false once we have introduced predicate
   logic. 

  tnd : TND
  tnd = {!!}
-}
  
  {- RAA = reductio ad absurdum (proof by contradiction) 
     
     states that to prove P it is enough to show that assuming ¬ P
     leades to a contradiction. We can conviniently represent this
     using double negation:
-}
  ¬¬ : Prp → Prp
  ¬¬ P = ¬ (¬ P)

  RAA : Set₁
  RAA = {P : Prp} → ¬¬ P ⇒ P

{-
  An attempt to prove RAA quickly leads to an infinite regress...

  raa : {P : Prp} → ¬¬ P ⇒ P
  raa = λ nnp → efq (nnp (λ p → nnp {!!}))
-}

  {- note that the other direction is fine: -}
  raainv : {P : Prp} → P ⇒ ¬¬ P 
  raainv p np = np p

  {- As another example we investigate the definition of ⇒ in terms of
  ∨ and ¬, namely P => Q ⇔ ¬ P ∨ Q. We try to prove both direction of
  this equivalence:
-}
  ⇒lem-1 : {P Q : Prp} → ¬ P ∨ Q ⇒ (P ⇒ Q)
  ⇒lem-1 (left np) p = efq (np p)
  ⇒lem-1 (right q) p = q
  {- this one is fine, what about the other ? -}

{-
  ⇒lem-2 : {P Q : Prp} → (P ⇒ Q) ⇒ ¬ P ∨ Q
  ⇒lem-2 pq = {!!}

  any attempt fails.
-}

  {- Going back to TND, we can't prove it but can we prove that we
  can't prove it? To the contrary, we can prove that it is not the
  case that we cannot prove it. :-) I mean we can prove the double
  negation of TND : -}

  ¬¬tnd : {P : Prp} → ¬¬ (P ∨ ¬ P)
  ¬¬tnd npnp = npnp (right (λ p → npnp (left p)))

  {- I illustrated this proof with a little fairy tale involving time
  travel... -}

  {- A consequene is that RAA proves TND. -}
  RAA→TND : RAA → TND
  RAA→TND raa = raa ¬¬tnd

  {- I leave the other direction for homework:
  TND → RAA
  TND→RND = {!!}

  and also the double negation of the last deMorgan law:
  ¬¬deMorgan¬∧ : {P Q : Prp} → ¬ (P ∧ Q) ⇒ ¬¬ ((¬ P) ∨ (¬ Q))
  ¬¬deMorgan¬∧ = {!!}
  -}