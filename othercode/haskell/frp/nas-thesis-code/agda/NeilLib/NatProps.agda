{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude
open import Nat
open import Recursion

module NatProps where

+O' : (n : ℕ) → n + O ≡ n
+O' = natrec refl (cong S)

+O : {n : ℕ} → n + O ≡ n
+O {n} = +O' n

+Scomm : {n : ℕ} → (m : ℕ) → S m + n ≡ m + S n
+Scomm = natrec refl (cong S)

+assoc : {m n : ℕ} → (l : ℕ) → (l + m) + n ≡ l + (m + n)
+assoc = natrec refl (cong S)

-- +assocR' : {m n : ℕ} → (l : ℕ) → l + (m + n) ≡ (l + m) + n
-- +assocR' = natrec refl (cong S)

-- +assoc : Associative _+_
-- +assoc {l} = +assoc' l

+comm' : (m n : ℕ) → m + n ≡ n + m
+comm' = natrec2 (sym ∘' +O') +O' (\hyp hyp1 hyp2 → cong S (trans2 hyp2 (cong S (sym hyp)) hyp1))

-- Either way of defining +comm is okay

+comm : {n : ℕ} → (m : ℕ) → m + n ≡ n + m
+comm {n} m = +comm' m n

-- +comm : Commutative _+_
-- +comm {m} = +comm' m

-- +comm : {n : ℕ} → (m : ℕ) → m + n ≡ n + m
-- +comm {n} = natrec (comm +O) (\ hyp → cong S (comm hyp) ≡∘ comm (+Scomm n))

import CommSemiGroup
open CommSemiGroup _+_ (λ {n} → +assoc n) (λ {n} → +comm n)

*O : (n : ℕ) → n * O ≡ O
*O = natrec refl id -- (λ {n} hyp → hyp ∘≡ +comm (n * 0))

*O' : {n : ℕ} → n * O ≡ O
*O' {n} = *O n

*+distr : {l m n : ℕ} → l * (m + n) ≡ l * m + l * n
*+distr {O} = refl
*+distr {S l} {m} {n} = trans2 (+assoc m) (cong (_+_ m) (trans (cong (_+_ n) (*+distr {l} {m} {n})) (ascomL {n} {l * m}))) (assocR {m})

*comm' : (m n : ℕ) → m * n ≡ n * m
*comm' = natrec2 (sym ∘' *O) *O (\{m} {n} hyp hyp1 hyp2 → cong S (trans2 (cong (_+_ n) hyp2) (ascomL {n} {m}) (cong (_+_ m) (trans (cong (_+_ n) (sym hyp)) hyp1))))

*comm : {n : ℕ} → (m : ℕ) → m * n ≡ n * m
*comm {n} m = *comm' m n

*assoc : {m n : ℕ} → (l : ℕ) → (l * m) * n ≡ l * (m * n)
*assoc {m} {n} =  natrec refl (\ {l} hyp → trans2 (*comm (m + l * m)) (*+distr {n} {m}) (cong2 _+_ (*comm n) (trans (*comm n) hyp)))


import CommSemiRing
open CommSemiRing _+_ (λ {n} → +assoc n) (λ {n} →  +comm n) O refl _*_ (λ {n} →  *assoc n) (λ {n} →  *comm n) refl 1 +O (λ {n} → *+distr {n}) public

-- S is Injective (Peano Axiom 8)

S-inj : Injective {ℕ} {ℕ} S
S-inj refl = refl

-- O is not the successor of any ℕ (Peano Axiom 7)

S≢O : {n : ℕ} → S n ≢ 0
S≢O ()

1≢n+n : (n : ℕ) → 1 ≢ n + n
1≢n+n O ()
1≢n+n (S n) p with trans (S-inj p) (+comm n)
... | ()

+cancL : CancellativeL _+_
+cancL {O}   = id
+cancL {S n} = +cancL {n} ∘ S-inj

import CancellativeComm
open CancellativeComm _+_ (λ {n} → +comm n) (λ {n} → +cancL {n}) public

+injIdem : {m n : ℕ} → m + m ≡ n + n → m ≡ n
+injIdem {O} {O} p = refl
+injIdem {O} {S n} ()
+injIdem {S m} {O} ()
+injIdem {S m} {S n} p = cong S (+injIdem (S-inj (trans2 (+comm (S m)) (S-inj p) (+comm n))))

+unitR : {m n : ℕ} → m ≡ m + n → n ≡ 0
+unitR {m} eq = sym (⊕canc {m} (trans ⊕unit eq))


-- Properties of subtraction

-inverse : (n : ℕ) → n - n ≡ O
-inverse = natrec refl id

-inversen : {n : ℕ} → (m : ℕ) → m + n - m ≡ n
-inversen = natrec refl id

-+distr : (l m n : ℕ) → l - (m + n) ≡ l - m - n
-+distr l O n = refl
-+distr O (S m) O = refl
-+distr O (S m) (S n) = refl
-+distr (S l) (S m) n = -+distr l m n
