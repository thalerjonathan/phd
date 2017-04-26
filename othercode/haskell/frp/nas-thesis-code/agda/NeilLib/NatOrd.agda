{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude
open import Nat
open import NatProps
open import Recursion

module NatOrd where

infix 3 _≤ℕ_ _<ℕ_

infix 8 _<=ℕ_ 

--------------------------------

_<=ℕ_ : ℕ → ℕ → Bool
O     <=ℕ _     = true
(S _) <=ℕ O     = false
S m   <=ℕ S n = m <=ℕ n

--------------------------------

data _<ℕ_ : ℕ → ℕ → Set where
  <O : {n : ℕ}            → O   <ℕ S n
  <S : {m n : ℕ} → m <ℕ n → S m <ℕ S n

<ℕ-trans : Transitive _<ℕ_
<ℕ-trans <O     (<S _) = <O
<ℕ-trans (<S p) (<S q) = <S (<ℕ-trans p q)

≮O : {n : ℕ} → Not (n <ℕ O)
≮O ()

O≠S : {n : ℕ} → O ≢ S n
O≠S ()

S≠O : {n : ℕ} → S n ≢ O
S≠O ()

<ℕ-injS : Injection _<ℕ_ S
<ℕ-injS (<S p) = p

<ℕ-trich : Trichotomous {ℕ} _<ℕ_
<ℕ-trich {O} {O} = inl (refl , ≮O , ≮O)
<ℕ-trich {O} {S m} = inrl (O≠S , <O , ≮O)
<ℕ-trich {S n} {O} = inrr (S≠O , ≮O , <O)
<ℕ-trich {S n} {S m} with <ℕ-trich {n} {m}
<ℕ-trich {S n} {S m} | inl (eq , mn , nm) = inl (cong S eq , mn ∘ <ℕ-injS , nm ∘ <ℕ-injS)
<ℕ-trich {S n} {S m} | inr (inl (eq , mn , nm)) = inrl (eq ∘ S-inj , <S mn , nm ∘ <ℕ-injS)
<ℕ-trich {S n} {S m} | inr (inr (eq , mn , nm)) = inrr (eq ∘ S-inj , mn ∘ <ℕ-injS , <S nm)

import StrictTotalOrder
open StrictTotalOrder _<ℕ_ <ℕ-trans <ℕ-trich public

--------------------------------

_≤ℕ_ : Rel ℕ
_≤ℕ_ = _≤_

≤ℕ-antisymℕ : Antisymmetric _≤ℕ_
≤ℕ-antisymℕ = antisymmetric

≤ℕ-trans : Transitive _≤ℕ_
≤ℕ-trans = ≤-trans

≤ℕ-total : Total _≤ℕ_
≤ℕ-total = totality

≤ℕ-injS : {m n : ℕ} → S m ≤ S n → m ≤ n
≤ℕ-injS (inl (<S p)) = inl p
≤ℕ-injS (inr refl) = ≤-refl

≤O : {n : ℕ} → O ≤ n
≤O {O} = ≤-refl
≤O {S _} = inl <O

≤S : {m n : ℕ} → m ≤ n → S m ≤ S n
≤S (inl p) = inl (<S p)
≤S (inr refl) = ≤-refl

1≤Sn : {n : ℕ} → 1 ≤ S n
1≤Sn = ≤S ≤O

n<S' : (n : ℕ) → n < S n
n<S' = natrec <O <S

n<S : {n : ℕ} → n < S n
n<S {n} = n<S' n

S≤n : {n : ℕ} → Not (S n ≤ n)
S≤n {O} (inl ())
S≤n {O} (inr ())
S≤n {S n} p = S≤n {n} (≤ℕ-injS p)

<S→≤ : {m n : ℕ} → m < S n → m ≤ n
<S→≤ <O = ≤O
<S→≤ .{_} {O} (<S ())
<S→≤ .{_} {S n} (<S p) = ≤S (<S→≤ p)

S≰O : {n : ℕ} → Not (S n ≤ O)
S≰O (inl ())
S≰O (inr ())


Compareℕ : ℕ → ℕ → Set
Compareℕ = OrdCompare

compareℕ : (m n : ℕ) → Compareℕ m n
compareℕ = compare

compareS : {m n : ℕ} → Compareℕ m n → Compareℕ (S m) (S n)
compareS refl     = refl
compareS (less p) = less (<S p)
compareS (more p) = more (<S p)


-- Nats are accessible, thus allowing well founded recursion (see Recursion) --

Accℕ : ℕ → Set
Accℕ = Acc ℕ _<ℕ_


-- This doesn't feel like a good way to prove this, but it works

accℕAux : (m n : ℕ) → m < n → Accℕ m
accℕAux O n p = acc-init (λ m → λ ())
accℕAux .(S m) .(S n) (<S {m} {n} p) = acc (λ l lt → accℕAux l n (≤<-trans (<S→≤ lt) p))

accℕ : {n : ℕ} → Accℕ n
accℕ {n} = accℕAux n (S n) n<S


-----------------------------------

compareℕ⁺ : (m n : ℕ⁺) → Compareℕ (m -1) (n -1) 
compareℕ⁺ (S m) (S n) = compare m n

safeMinus : (m n : ℕ) → n ≤ℕ m → ℕ
safeMinus m O p = m
safeMinus (S m) (S n) p = safeMinus m n (≤ℕ-injS p)
safeMinus O (S n) (inl ())
safeMinus O (S n) (inr ())

safeMinus⁺ : (m n : ℕ) → S n ≤ℕ m → ℕ⁺
safeMinus⁺ O O (inl ())
safeMinus⁺ O O (inr ())
safeMinus⁺ (S m) O p = S m
safeMinus⁺ O (S n) (inl ())
safeMinus⁺ O (S n) (inr ())
safeMinus⁺ (S m) (S n) p = safeMinus⁺ m n (≤ℕ-injS p)

-----------------------------------

Quotient = ℕ
Remainder = ℕ

private divminus : (m n : ℕ) → m ≥ n → n > O → Σ ℕ (\ k → k <ℕ m)
        divminus ._ ._ (inr refl) <O = O , <O
        divminus O .(S n) (inl ()) (<O {n})
        divminus (S m) .1 (inl p) (<O {O}) = m , n<S
        divminus (S m) .(S (S n)) (inl p) (<O {S n}) = second' (flip <-trans n<S) (divminus m (S n) (inl (<ℕ-injS p)) <O) 

divide : ℕ → (d : ℕ) → d > O → Quotient × Remainder
divide n d gt = wfrec n divAux accℕ
   where

        divAux : (m : ℕ) → Accℕ m → ((z : ℕ) → z <ℕ m → Quotient × Remainder) → Quotient × Remainder
        divAux O a f = O , O
        divAux m a f with compareℕ m d
        divAux .d a f | refl = 1 , O
        divAux m a f  | less p = O , m
        divAux m a f  | more p = (first S ∘ uncurry f ∘ divminus m d (inl p)) gt

-- we give f a smaller value, and then transform that smaller value into the larger value we desire
-- we compute the smaller value by performing one reduction step (in this case "subtract")

div : ℕ → (n : ℕ) → n > O → Quotient
div m n = fst ∘ divide m n

_%_ : ℕ → (n : ℕ) → n > O → Remainder
_%_ m n = snd ∘ divide m n

half : ℕ → ℕ
half n = div n 2 <O
