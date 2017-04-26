{-# OPTIONS --type-in-type
   #-}

module IntProps where

open import NeilPrelude

succEq+S : {n : ℕ} -> {i : ℤ} -> i ≡ toℤ n -> succ i ≡ +S n
succEq+S {O} refl = refl
succEq+S {S n} refl = refl

predEqS : {n : ℕ} -> {i : ℤ} -> i ≡ toℤ (S n) -> pred i ≡ toℤ n
predEqS {O} refl = refl
predEqS {S n} refl = refl

+'ℕO : {n : ℕ} -> n +' O ≡ toℤ n
+'ℕO {O} = refl
+'ℕO {S n} = succEq+S (+'ℕO {n})

+'ℕZ : {n : ℕ} -> n +' Z ≡ toℤ n
+'ℕZ {O} = refl
+'ℕZ {S n} = succEq+S (+'ℕZ {n})

+'ℤO : {i : ℤ} -> i +' O ≡ i
+'ℤO { +S n} = succEq+S (+'ℕO {n})
+'ℤO { Z}    = refl
+'ℤO { -S O} = refl
+'ℤO { -S (S n)} = resp pred (+'ℤO { -S n})

+'ℤZ : {i : ℤ} -> i +' Z ≡ i
+'ℤZ { +S n} = succEq+S (+'ℕZ {n})
+'ℤZ { Z}    = refl
+'ℤZ { -S O} = refl
+'ℤZ { -S (S n)} = resp pred (+'ℤZ { -S n})

succpred : {i : ℤ} -> succ (pred i) ≡ i
succpred { +S O}     = refl
succpred { +S (S n)} = refl
succpred { Z}        = refl
succpred { -S n}     = refl

predsucc : {i : ℤ} -> pred (succ i) ≡ i
predsucc { +S n} = refl
predsucc { Z}    = refl
predsucc { -S O} = refl
predsucc { -S (S n)} = refl

+'succ : {s t : NumType} -> {x : numSet s} -> {y : numSet t} -> succ x +' y ≡ succ (x +' y)
+'succ {nat} = refl
+'succ {int} {t} { +S n} = refl
+'succ {int} {t} { Z}    = refl
+'succ {int} {t} { -S O} = comm succpred
+'succ {int} {t} { -S (S n)} = comm succpred

+'pred : {i j : ℤ} -> pred i +' j ≡ pred (i +' j)
+'pred { +S O} = comm predsucc
+'pred { +S (S n)} = comm predsucc
+'pred { Z}    = refl
+'pred { -S n} = refl

+'assocℕ : {j k : ℤ} -> (n : ℕ) -> (n +' j) +' k ≡ n +' (j +' k)
+'assocℕ O = refl
+'assocℕ {j} (S n) = resp succ (+'assocℕ n) ∘≡ +'succ {_} {_} {n +' j}

+'assoc : {j k : ℤ} -> (i : ℤ) -> (i +' j) +' k ≡ i +' (j +' k)
+'assoc {j} (+S n) = resp succ (+'assocℕ n) ∘≡ +'succ {_} {_} {n +' j}
+'assoc     Z      = refl
+'assoc {j} (-S O) = +'pred {j}
+'assoc {j} (-S (S n)) = resp pred (+'assoc (-S n)) ∘≡ +'pred { -S n +' j}

+'commSℕℤ : {m n : ℕ} -> m +' +S n ≡ S m +' n
+'commSℕℤ {O} = comm (succEq+S refl)
+'commSℕℤ {S m} = resp succ (+'commSℕℤ {m})

+'commSℕℕ : {m n : ℕ} -> m +' S n ≡ S m +' n
+'commSℕℕ {O} = comm (succEq+S refl)
+'commSℕℕ {S m} = resp succ (+'commSℕℕ {m})

+'commℕℕ : {m n : ℕ} -> m +' n ≡ n +' m
+'commℕℕ {O} {n} = comm (+'ℕO {n})
+'commℕℕ {S m} {O} = succEq+S (+'ℕO {m})
+'commℕℕ {S m} {S n} = resp succ (+'commℕℕ {S m} ∘≡ +'commSℕℕ {m})

+'succrespℕ : {m n : ℕ} -> S m ≡ S n -> m ≡ n
+'succrespℕ refl = refl

+'succrespℤ : {i j : ℤ} -> succ i ≡ succ j -> i ≡ j
+'succrespℤ { +S m} {+S .m} refl = refl
+'succrespℤ { +S m} { Z} ()
+'succrespℤ { +S m} { -S O} ()
+'succrespℤ { +S m} { -S (S n)} ()
+'succrespℤ { Z}    { +S n} ()
+'succrespℤ { Z}    { Z} refl = refl
+'succrespℤ {Z} { -S O} ()
+'succrespℤ {Z} { -S (S n)} ()
+'succrespℤ { -S O} {+S n} ()
+'succrespℤ { -S (S m)} {+S n} ()
+'succrespℤ { -S O} {Z} ()
+'succrespℤ { -S (S m)} {Z} ()
+'succrespℤ { -S O} { -S O} refl = refl
+'succrespℤ { -S O} { -S (S n)} ()
+'succrespℤ { -S (S m)} { -S O} ()
+'succrespℤ { -S (S m)} { -S (S .m)} refl = refl

+'succSℕ : {i : ℤ} -> {n : ℕ} -> succ (i +' n) ≡ i +' S n
+'succSℕ { +S m} {n} = resp succ (comm (+'commSℕℕ {m}))
+'succSℕ { Z}    = succEq+S refl
+'succSℕ { -S O} {O} = refl
+'succSℕ { -S O} {S n} = succpred
+'succSℕ { -S (S m)} {n} = +'succrespℤ (comm succpred ∘≡ +'succSℕ { -S m}) ∘≡ succpred { -S m +' n} 

+'succSℤ : {i : ℤ} -> {n : ℕ} -> succ (i +' n) ≡ i +' +S n
+'succSℤ { +S m} = resp succ (comm (+'commSℕℤ {m}))
+'succSℤ { Z}    = succEq+S refl
+'succSℤ { -S O} {O} = refl
+'succSℤ { -S O} {S n} = succpred
+'succSℤ { -S (S m)} {n} = +'succrespℤ (comm succpred ∘≡ +'succSℤ { -S m}) ∘≡ succpred { -S m +' n}

+'succℕℤ : {n : ℕ} -> {i : ℤ} -> succ (n +' i) ≡ n +' succ i
+'succℕℤ {O} = refl
+'succℕℤ {S n} = resp succ (+'succℕℤ {n})

+'succℤℤ : {i j : ℤ} -> succ (i +' j) ≡ i +' succ j
+'succℤℤ { +S m} = resp succ (+'succℕℤ {m})
+'succℤℤ { Z}    = refl
+'succℤℤ { -S O} {j} = comm predsucc ∘≡ succpred {j}
+'succℤℤ { -S (S m)} {j} = +'succrespℤ (comm (+'succℤℤ { -S m}) ≡∘ succpred) ∘≡ succpred { -S m +' j}

+'commℕℤ : {i : ℤ} -> (n : ℕ) -> n +' i ≡ i +' n
+'commℕℤ O = comm +'ℤO
+'commℕℤ { +S m} (S n) = resp succ (+'commℕℕ {S n}) ∘≡ resp succ (+'commSℕℤ {n})
+'commℕℤ { Z} (S n)    = succEq+S (+'ℕZ {n})
+'commℕℤ { -S m} (S n) = +'succSℕ { -S m} ∘≡ resp succ (+'commℕℤ n)

+'comm : {j : ℤ} -> (i : ℤ) -> i +' j ≡ j +' i
+'comm {j} (+S n) = +'succSℤ {j} ∘≡ resp succ (+'commℕℤ {j} n)
+'comm Z      = comm +'ℤZ
+'comm {+S O} (-S O) = refl
+'comm {+S (S m)} (-S O) = (succEq+S refl ∘≡ resp succ (succpred {toℤ m})) ≡∘ resp (succ ∘ succ) (+'commℕℤ { -S O} m)
+'comm { Z}    (-S O) = refl
+'comm { -S O} (-S O) = refl
+'comm { -S (S m)} (-S O) = comm (resp pred (+'comm { -S O} (-S m)))
+'comm {j} (-S (S n)) = +'succrespℤ (comm succpred ≡∘ +'succℤℤ {j}) ∘≡ resp pred (+'comm {j} (-S n))

negateAux : {n : ℕ} -> -S n +' S n ≡ Z
negateAux {O} = refl
negateAux {S n} = {! !}

negateℤ : {i : ℤ} -> i +' negate i ≡ Z
negateℤ {+S O} = refl
negateℤ {+S (S n)} = ( {! !} ∘≡ resp succ (succpred { -S n +' n})) ∘≡ resp (succ ∘ succ) (+'commℕℤ { -S (S n)} n)
negateℤ { Z} = {! !}
negateℤ { -S n} = {! !}

-- should be a Commutative Ring, but I can't be bothered to do any more proofs at the moment
-- for that matter, most of the exisiting proofs should be tidied up, and generalised onto Nums to reduce repetition

import CommGroup
open module CG = CommGroup _+'_ +'assoc +'comm Z refl negate {! !} public