{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude
open import Bool

module Logic where

---------------------------------------------------------------

record True : Set where

---------------------------------------------------------------

isTrue : Bool → Set
isTrue true  = True
isTrue false = False

magic : {A : Set} → (True → False) → A
magic p = absurd (p _)

-- "Sat"isfies the predicate

Sat : {A : Set} → (A → Bool) → A → Set
Sat p = isTrue ∘ p

isFalse : Bool → Set
isFalse = Sat not

trueIsTrue : {b : Bool} → b ≡ true → isTrue b
trueIsTrue refl = _

falseIsFalse : {b : Bool} → b ≡ false → isFalse b
falseIsFalse refl = _

---------------------------------------------------------------

isJust : {A : Set} → Maybe A → Bool
isJust nothing  = false
isJust (just _) = true

isNothing : {A : Set} → Maybe A → Bool
isNothing nothing   = true 
isNothing (just _)  = false

IsJust : {A : Set} → Maybe A → Set
IsJust = Sat isJust

IsNothing : {A : Set} → Maybe A → Set
IsNothing = Not ∘ IsJust

lem-isNothing : {A : Set} → {ma : Maybe A} → IsNothing ma → ma ≡ nothing
lem-isNothing {_} {nothing} _ = refl
lem-isNothing {_} {just _}  p = magic p

lem-isNothingEq : {A : Set} → {ma mb : Maybe A} → IsNothing ma → IsNothing mb → ma ≡ mb
lem-isNothingEq p q = trans (lem-isNothing p) (sym (lem-isNothing q))

lem-just≠nothing : {A : Set} → {a : A} → just a ≢ nothing
lem-just≠nothing ()

---------------------------------------------------------------

implDist : {P Q R : Set} → (P ⊎ Q → R) → (P → R) × (Q → R)
implDist = λ f → f ∘ inl , f ∘ inr

implDistR : {P Q R : Set} → (P → R) × (Q → R) → (P ⊎ Q → R)
implDistR = λ pq → case (fst pq) (snd pq)

NotNot : {P : Set} → P → Not (Not P)
NotNot = applyTo

contpos : {P Q : Set} → (P → Q) → (Not Q → Not P)
contpos = _⋙_

nn-antecedent : {P Q : Set} → (Not (Not P) → Q) → (P → Q)
nn-antecedent = argument applyTo

nn-consequent : {P Q : Set} → (P → Q) → (P → Not (Not Q))
nn-consequent f = applyTo ∘ f

impl-contr : {P Q : Set} → (P → Q) → P → Not Q → False
impl-contr f p nq = nq (f p)

ProofIrrelevence : Set → Set
ProofIrrelevence P = (p q : P) → p ≡ q

proofIrrelevence : {A : Set} → {a b : A} → ProofIrrelevence (a ≡ b)
proofIrrelevence refl refl = refl



---------------------------------------------------------------

-- Classical Logic (depends on the excluded middle)

EM : Set
EM = ∀ {P} → P ⊎ Not P

Decidable : Set → Set
Decidable P = P ⊎ Not P

em-isTrue : {b : Bool} → isTrue b ⊎ Not (isTrue b)
em-isTrue {false} = inr id
em-isTrue {true}  = inl _


contradiction : {P : Set} → Decidable P → Not (Not P) → P
contradiction (inl p)  nnp = p
contradiction (inr np) nnp = absurd (nnp np)

contpos-em : {P Q : Set} → Decidable P → (Not P → Not Q) → (Q → P)
contpos-em em f q = contradiction em (flip f q)

nn-antecedent-em : {P Q : Set} → Decidable P → (P → Q) → (Not (Not P) → Q)
nn-antecedent-em (inl p)  f nnp = f p
nn-antecedent-em (inr np) f nnp = absurd (nnp np) 

nn-consequent-em : {P Q : Set} → Decidable Q → (P → Not (Not Q)) → (P → Q)
nn-consequent-em em f p = contradiction em (f p)

notBoth : {P Q : Set} → Decidable P ⊎ Decidable Q → Not (P × Q) → Not P ⊎ Not Q
notBoth (inl (inl p))  nb = inr (λ q → nb (p , q))
notBoth (inl (inr np)) nb = inl np
notBoth (inr (inl q))  nb = inl (λ p → nb (p , q))
notBoth (inr (inr nq)) nb = inr nq

-- nn-Bool : {b : Bool} → Not (Not (isTrue b)) → isTrue b
-- nn-Bool = case const (λ nb → absurd ∘ applyTo nb) em-Bool

-- Some lemmas of forall and exists

-- lem1 : {A : Set} → {P : A → Set} → Σ A P → Not (Π A (Not ∘ P))
-- lem1 (a , pa) f = f a pa

-- lem2 : {A : Set} → {P : A → Set} → Σ A (Not ∘ P) → Not (Π A P)
-- lem2 (a , npa) f = npa (f a)

-- lem3 : {A : Set} → {P : A → Set} → Π A P → Not (Σ A (Not ∘ P))
-- lem3 f (a , npa) = npa (f a)

-- lem4 : {A : Set} → {P : A → Set} → Π A (Not ∘ P) → Not (Σ A P)
-- lem4 f (a , pa) = f a pa

-- lem5 : {A : Set} → {P : A → Set} → EM → Not (Π A (Not ∘ P)) → Σ A P
-- lem5 em p = case id (λ nap → absurd (p (curry nap))) em

-- lem-nAEn : {A : Set} → {P : A → Set} → EM → Not (Π A P) → Σ A (Not ∘ P)
-- lem-nAEn {A} {P} em nap with em {A}
-- ... | inr na = absurd (nap (λ a → absurd (na a)))
-- ... | inl a with em {P a}
-- ...   | inl pa   = {!!}
-- ...   | inr npa = a , npa

must-always-cur : {A : Set} → {P : A → Set} → EM → Π A (Not ∘ Not ∘ P) → Π A P
must-always-cur em npa a = contradiction em (npa a)

must-always : {A : Set} → {P : A → Set} → EM → Not (Σ A (Not ∘ P)) → Π A P
must-always em = must-always-cur em ∘ curry

must-exist : {A : Set} → {P : A → Set} → EM → Not (Π A (Not ∘ P)) → Σ A P
must-exist em nn = contradiction em (nn ∘ curry)

must-exist-not : {A : Set} → {P : A → Set} → EM → Not (Π A P) → Σ A (Not ∘ P)
must-exist-not em nap = must-exist em (λ f → nap (must-always-cur em f))

must-exist-not2 : {A : Set} → {B : A → Set} → {P : (a : A) → B a → Set} → EM → Not (Π₂ A B P) → Σ A (λ a → Σ (B a) (Not ∘ P a))
must-exist-not2 em = second' (must-exist-not em) ∘ must-exist-not em

-- must-never : {A : Set} → {P : A → Set} → Not (Σ A P) → Π A (Not ∘ P)
-- must-never = curry

-- must-never2 : {A : Set} → {B : A → Set} → {P : (a : A) → B a → Set} → Not (Σ₂ A B P) → (a : A) → Π (B a) (Not ∘ P a)
-- must-never2 = curry2 


-- lem-Aux : {A : Set} → {P : A → Set} → EM → Not (Π A (Not ∘ P)) → Σ A (Not ∘ Not ∘ P)
-- lem-Aux em nn with must-exist em nn
-- ... | a , p = a , NotNot p
