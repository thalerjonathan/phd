{- 
  Computer Aided Formal Reasoning (G53CFR, G54CFR)
  Thorsten Altenkirch

  Lecture 7: Basic Predicate Logic

  In this lecture we extend the Curry-Howard correspondence to
  predicate logic. To do this we need to use dependent types which we
  have already seen in programming, e.g. Vec and Fin. 

-}
module l07 where

open import l05 -- we are going to use the definitions for propositional logic

{- 
   What are predicates and relations?

   Given A : Set a predicate is an element of A → Prp 
   a relation over A is an element of A → A → Prp

   Examples:
   Odd : ℕ → Prp
   Prime : ℕ → Prp
   _≡_ : ℕ → ℕ → Prp ( being equal, we use ≡ not to confuse equality
                        with definitions (=)).
   _≡_ : {A : Set} → A → A → Prp (polymorphic equality) 
   _<_ : ℕ → ℕ → Prp 
   _≡_mod2 : ℕ → ℕ → Prp (equality modulo 2, both are even or both are
                          odd. )
   _∈_ : {A : Set} → A → List A → Prp (occurs relation, a ∈ as means a
                                       occurs in the list as.)
-}

{- Universal quantification: given a set A, and a predicate P : A → Prp
   ∀' A P : Prop means that P a is true (inhabited) for all a:A.
   A proof of this is a (depndent) function which assigns to any a:A
   an element of P a.
   We have seen dependent functions before, e.g. 
   max : (n : ℕ) → Fin (suc n)
-}
∀' : (A : Set) → (A → Prp) → Prp
∀' A P = (a : A) → P a  

{- Note: we use ∀' because ∀ is a reserved word in Agda. -}

{- Existential quantification: given a set A, and a predicate P : A → Prp
   ∀' A P : Prop means that P a is true (inhabited) for some a:A.
   A proof of this is a (depndent) pair (a , p) where a : A and 
   p : P a is a proof that P a is true (inhabited).
-}
data ∃ (A : Set)(P : A → Prp) : Prp where
  _,_ : (a : A) → P a → ∃ A P

{- for convenience we are going to use fst and snd. -}

fst : {P Q : Prp} → P ∧ Q ⇒ P
fst (p , q) = p

snd : {P Q : Prp} → P ∧ Q ⇒ Q
snd (p , q) = q

{- As an example we show that ∀ commutes with ∧
   "Everybody has glasses and a watch is equivalent to 
    everybody has glasses and everybody has a watch."
 -}

∀∧-lem-1 : {A : Set}{P Q : A → Prp} → 
  (∀' A λ a → P a ∧ Q a) ⇒ (∀' A λ a → P a) ∧ (∀' A λ a → Q a)
∀∧-lem-1 pq = (λ a → fst (pq a)) , (λ a → snd (pq a))

∀∧-lem-2 : {A : Set}{P Q : A → Prp} → 
  (∀' A λ a → P a) ∧ (∀' A λ a → Q a) ⇒ (∀' A λ a → P a ∧ Q a) 
∀∧-lem-2 (p , q) = λ a → (p a) , (q a)

∀∧-lem : {A : Set}{P Q : A → Prp} → 
      (∀' A λ a → P a ∧ Q a) ⇔ (∀' A λ a → P a) ∧ (∀' A λ a → Q a)
∀∧-lem = ∀∧-lem-1 , ∀∧-lem-2

{- We noticed that ∀ does not commute with ∨. The following direction
   fails: 
-}

{-
∀∨-lem-1 : {A : Set}{P Q : A → Prp} → 
      (∀' A λ a → P a ∨ Q a) ⇒ (∀' A λ a → P a) ∨ (∀' A λ a → Q a)
∀∨-lem-1 pq = left (λ a → {!!})
-}

{- Intuitively: If everybody has a watch or glasses, it is not
   necesserially true that everybody has a watch or everybody has
   glasses. There may be somebody who has only got a watch and
   somebody else who only has got glasses. 
-} 

{- However, ∃ does commute with ∨.
   I leave the proof as an exercise. 
   Can you come up with an intuitive explanation?

∃∨-lem : {A : Set}{P Q : A → Prp} → (∃ A λ a → P a ∨ Q a) ⇔ (∃ A λ a → P a) ∨ (∃ A λ a → Q a)
∃∨-lem = {!!}
-}

{- Next we are going to show a lemma which relates ∀ and ∃. 
   Intuitively: If anybody understands the course then I am happy
   is equivelent to If there exists somebody who understands the
   course then I am happy. 
-}

∀∃-lem-1 : {A : Set}{P : A → Prp}{Q : Prp} → 
  (∀' A λ a → P a ⇒ Q) ⇒ (∃ A λ a → P a) ⇒ Q
∀∃-lem-1 pq (a , p) = pq a p

∀∃-lem-2 : {A : Set}{P : A → Prp}{Q : Prp} → 
  ((∃ A λ a → P a) ⇒ Q) ⇒ (∀' A λ a → P a ⇒ Q)
∀∃-lem-2 pq = λ a p → pq (a , p)

∀∃-lem : {A : Set}{P : A → Prp}{Q : Prp} → 
       (∀' A λ a → P a ⇒ Q) ⇔ (∃ A λ a → P a) ⇒ Q
∀∃-lem = ∀∃-lem-1 , ∀∃-lem-2

{- Note that the proofs look identical to one of the exercises on
   propositional logic:

   P ⇒ Q => R  ⇔ P ∧ Q ⇒ R 
-}


{- Equality: We are going to define polymorphic equality using CH.

   In future we use
   open import Relation.Binary.PropositionalEquality
-}

infix 4 _≡_ -- \equiv

{- We define a dependent type where the only proof of equality is
reflexivity, which for any element a:A proves that a ≡ a. -}

data _≡_ {A : Set} : A → A → Prp where
    refl : {a : A} → a ≡ a

{- Using pattern matching we can prove that ≡ is symmetric & transitive
   and hence an equivalence relation (because refl proves reflexivity)/ -}

sym : {A : Set} → {a b : A} → a ≡ b ⇒ b ≡ a
sym refl = refl

trans : {A : Set}{a b c : A} → a ≡ b ⇒ b ≡ c ⇒ a ≡ c
trans p refl = p

{- Any function respects equality: -}

resp : {A B : Set}{f : A → B}{a b : A} → a ≡ b ⇒ f a ≡ f b
resp refl = refl

{- But not every function is injective. -}

{-
resp' : {A B : Set}{f : A → B}{a b : A} →  f a ≡ f b ⇒ a ≡ b
resp' p = {!p!}
-}

{- Agda rejects the attempt to pattern match on f a ≡ f b. 
   While it is correct that refl is the only proof of this 
   type, we cannot conclude that a ≡ b. -}

{- We prove a very general property: substitutivity.
   The other properties we have mentioned are derivable 
   from this one. -}

subst : {A : Set}{P : A → Prp} → {a b : A} → a ≡ b ⇒ P a ⇒ P b
subst refl p = p 
