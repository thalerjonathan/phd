{- 
  Computer Aided Formal Reasoning (G53CFR, G54CFR)
  Thorsten Altenkirch

  Lecture 5: Propositional logic

  In this lecture we start to use Agda as a proof assistant. We define
  the basic propositional connectives using the
  Curry-Howard-Isomorphism: we identify a proposition with the set of
  its proofs, i.e. we set Prop = Set. This is in contrast with the
  classical identification of Prop = Bool. The latter doesn't really
  work in a constructive setting, since we would expect that every P :
  Prop reduces to either true or false. But there is no way to do this
  since there are many propositions of which we don't know wether they
  are true or false.

-}
module l05 where

  {- we identify Prop and Set, Prop is already reserved in Agda hence
     we use Prp instead. -}

  Prp : Set₁
  Prp = Set

  {- implication (⇒ = \=>)
    
      We interpret implication by function space. The idea is that we
      know that P ⇒ Q is true if we have a function transforming
      proofs of P into proofs of Q.
  -}

  _⇒_ : Prp → Prp → Prp
  P ⇒ Q = P → Q

  infixr 0 _⇒_

  {- We prove a very simple tautology: -}

  I : {P : Prp} → P ⇒ P
  I p = p

  {- conjunction  (∧ = \and)

     A proof of P ∧ Q is a pair of proofs one of P and one of Q.
     
  -}

  data _∧_ (P Q : Prp) : Prp where
    _,_ : P → Q → P ∧ Q

  infixr 2 _∧_

  {- a simple tautology: ∧ is commutative. Why don't we have to prove
     the other direction? -}

  ∧-comm : {P Q : Prp} → P ∧ Q ⇒ Q ∧ P
  ∧-comm (p , q) = q , p

  {- disjunction ∨ = \or 
  
     A proof of P ∨ Q is either a proof of P prefixed with left or a
     proof of Q prefixed with right.
  -}

  data _∨_ (P Q : Prp) : Prp where
    left : P → P ∨ Q
    right : Q → P ∨ Q

  infixr 1 _∨_

  {- we show that ∨ is commutative -}

  ∨-comm : {P Q : Prp} → P ∨ Q ⇒ Q ∨ P
  ∨-comm (left p) = right p
  ∨-comm (right q) = left q

  {- Prove distributivity of ∧ over ∨ -}

  distrib-∧-∨-1 : {P Q R : Prp} → P ∧ (Q ∨ R) ⇒ (P ∧ Q) ∨ (P ∧ R)
  distrib-∧-∨-1 (p , left q) = left (p , q)
  distrib-∧-∨-1 (p , right r) = right (p , r)

  {- I leave the other direction as an exercise -}

  {-
  distrib-∧-∨-2 : {P Q R : Prp} → (P ∧ Q) ∨ (P ∧ R) ⇒ P ∧ (Q ∨ R)
  distrib-∧-∨-2 pqr = {!!}
  -}

  {- Logical equivalence (⇔ = \iff)

     we define ⇔ in terms of ∧ and ⇒ 

  -}

  _⇔_ : Prp → Prp → Prp
  P ⇔ Q = (P ⇒ Q) ∧ (Q ⇒ P)

  infixr 0 _⇔_

  {- we can combine the two lemmas above to prove the distributivity 
     as a logical equivalence. -}

  {-
  distrib-∧-∨ : {P Q R : Prp} →  P ∧ (Q ∨ R) ⇔ (P ∧ Q) ∨ (P ∧ R)
  distrib-∧-∨ =  distrib-∧-∨-1 , distrib-∧-∨-2
  -}

  {- True (⊤ = \top)
     
     has a trivial proof. We can view ⊤ as a special case of ∧ with no
     arguments. 
  -}

  data ⊤ : Prp where
    tt : ⊤

  {- False (⊥ = \bot)

     has no proof. We can view ⊥ as a special case of ∨ with no arguments.
  -}

  data ⊥ : Prp where

  {- We can show that from false everything follows
     ("ex falso quod libet" in latin) using the impossible pattern in
     Agda. -}

  efq : {P : Prp} → ⊥ ⇒ P
  efq ()

  {- Negation (¬ = \neg)

     ¬ P is defined as P implies False.
  -}

  ¬ : Prp → Prp
  ¬ P = P ⇒ ⊥

  {- Some basic facts about negation. -}

  contradict : {P : Prp} → ¬ (P ∧ ¬ P)
  contradict (p , np) = np p

  contrapos : {P Q : Prp} → (P ⇒ Q) ⇒ ¬ Q ⇒ ¬ P
  contrapos pq nq p = nq (pq p)

  {- Let's prove the de Morgan laws -}

  deMorgan¬∨ : {P Q : Prp} → ¬ (P ∨ Q) ⇒ ¬ P ∧ ¬ Q 
  deMorgan¬∨ npq = (λ p → npq (left p)) , λ q → npq (right q)
  
  deMorgan¬∧¬ : {P Q : Prp} → (¬ P) ∧ (¬ Q) ⇒ ¬ (P ∨ Q)
  deMorgan¬∧¬ (np , nq) (left p) = np p
  deMorgan¬∧¬ (np , nq) (right q) = nq q
  
  deMorgan¬∨¬ : {P Q : Prp} → (¬ P) ∨ (¬ Q) ⇒ ¬ (P ∧ Q)
  deMorgan¬∨¬ (left np) (p , q) = np p
  deMorgan¬∨¬ (right nq) (p , q) = nq q

{-
  The last one turns out not to be provable!

  deMorgan¬∧ : {P Q : Prp} → ¬ (P ∧ Q) ⇒ (¬ P) ∨ (¬ Q)
  deMorgan¬∧ npq = right (λ q → npq ({!!} , q))

  Here is an attempt, the only choice was that we decided to say
  right. It is easy to see that starting with left leads to a
  symmetric situation. This intuitively shows that there is no way to
  prove deMorgan¬∧. But this doesn't mean that its negation is
  provable, indeed we will show that this is not the case.
-}

  {- to be continued ... -}