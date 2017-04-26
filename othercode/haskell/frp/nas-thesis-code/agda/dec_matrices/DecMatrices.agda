{-# OPTIONS --type-in-type #-}

module DecMatrices where

open import NeilPrelude
open import RealTime
open import Decoupled
open import SVDesc
open import Nat hiding (_*_)
open import Vector

---------------------------------------------------------------------------------------

import VectorMaths
open VectorMaths Dec dec cau _∧_ _∨_ renaming (mat*mat to _*_ ; mat+mat to _+m_)

-- Note: we choose dec to be 0 so as to allow the identity matrix to match "identity"
-- Note: multiplication has to match sequential composition of two basic dec values

-- dec = 0
-- cau = 1
-- ∧ = +
-- ∨ = *

-- An m × n matrix has m rows and n columns

-- m is the number of input signals
-- n is the number of output signals

-- each row corresponds to one input signal
-- each column corresponds to one output signal

-- A matrix built of vectors is a vector of columns
--  _ _
-- |1|4|
-- |2|5|
-- |3|6|
--  - -
-- Thus, +v+ appends two matrices horizontally.

---------------------------------------------------------------------------------------

_++v_ : {l m n : ℕ} → matrix l n → matrix m n → matrix (l + m) n 
_++v_ = _+m+_

_++h_ : {l m n : ℕ} → matrix l m → matrix l n → matrix l (m + n)
_++h_ = _+v+_

postulate extend : {m n : ℕ} → matrix m 1 → matrix m n

---------------------------------------------------------------------------------------

svlength : SVDesc → ℕ
svlength (C A)     = 1
svlength (E A)     = 1
svlength (S A)     = 1
svlength (as , bs) = svlength as + svlength bs

SVMatrix : SVDesc → SVDesc → Set
SVMatrix as bs = matrix (svlength as) (svlength bs)

Mdec : {m n : ℕ} → matrix m n
Mdec = mat dec

Mcau : {m n : ℕ} → matrix m n
Mcau = mat cau

I : {n : ℕ} → matrix n n
I = matId

postulate SF : (as bs : SVDesc) → SVMatrix as bs → Set

---------------------------------------------------------------------------------------

postulate identity : {as : SVDesc} → SF as as I

postulate sfFst : {as bs : SVDesc} → SF (as , bs) as (I ++v Mdec)

postulate sfSnd : {as bs : SVDesc} → SF (as , bs) bs (Mdec {svlength as} {svlength bs} ++v I)

postulate _>>>_ : {as bs cs : SVDesc} → {m₁ : SVMatrix as bs} → {m₂ : SVMatrix bs cs}
                → SF as bs m₁ → SF bs cs m₂ → SF as cs (m₁ * m₂)

postulate _&&&_ : {as bs cs : SVDesc} → {m₁ : SVMatrix as bs} → {m₂ : SVMatrix as cs}
                → SF as bs m₁ → SF as cs m₂ → SF as (bs , cs) (m₁ ++h m₂)

postulate freeze : {as bs : SVDesc} → {m : SVMatrix as bs}
                 → SF as bs m → SF as (bs , C (SF as bs m)) (m ++h Mdec)

postulate switch : {as bs : SVDesc} → {A : Set} → {m₁ m₂ : SVMatrix as bs} → (me : SVMatrix as (E A))
                 → SF as (bs , E A) (m₁ ++h me) → (A → SF as bs m₂) → SF as bs ((m₁ +m m₂) +m extend me)

postulate dswitch : {as bs : SVDesc} → {A : Set} → {m₁ m₂ : SVMatrix as bs} → (me : SVMatrix as (E A))
                  → SF as (bs , E A) (m₁ ++h me) → (A → SF as bs m₂) → SF as bs (m₁ +m m₂)

postulate loop : {as bs cs : SVDesc} → {m₁ : SVMatrix as bs} → {m₂ : SVMatrix cs bs}
                  → SF (as , cs) bs (m₁ ++v m₂) → SF cs bs Mdec → SF as bs m₁

postulate arrowLoop : {as bs cs : SVDesc} → {m₁₁ : SVMatrix as bs} → {m₁₂ : SVMatrix as cs} → {m₂₁ : SVMatrix cs bs}
                    → SF (as , cs) (bs , cs) ((m₁₁ ++v m₂₁) ++h (m₁₂ ++v Mdec)) → SF as bs (m₁₁ +m (m₁₂ * m₂₁))

postulate loop' : {as bs cs : SVDesc} → {m : SVMatrix bs cs} → SF (as , cs) bs Mdec → SF bs cs m → SF as bs Mdec
-- loop' sff sfb = loop (sfSecond (forkFirst sfb) >>> sfAssocL) (sfFst >>> sff) >>> {!sfSnd!}

---------------------------------------------------------------------------------------

postulate now : {as : SVDesc} → SF as (E Unit) Mdec
postulate delayE : {A : Set} → Time⁺ → SF (E A) (E A) Mdec

postulate toFst : {as bs cs : SVDesc} → {m : SVMatrix as cs} → SF as cs m → SF (as , bs) cs (m ++v Mdec) 
-- toFst sf = sfFst >>> sf

postulate toSnd : {as bs cs : SVDesc} → {m : SVMatrix bs cs} → SF bs cs m →  SF (as , bs) cs (Mdec {svlength as} {svlength cs} ++v m)
-- toSnd sf = sfSnd >>> sf

_***_ : {as bs cs ds : SVDesc} → {m₁ : SVMatrix as bs} → {m₂ : SVMatrix cs ds} → SF as bs m₁ → SF cs ds m₂ → SF (as , cs) (bs , ds) ((m₁ ++v Mdec) ++h (Mdec {svlength as} {svlength ds} ++v m₂))
sf₁ *** sf₂ = toFst sf₁ &&& toSnd sf₂

inaccurate : {A : Set} → SF (E A) (E A , E Unit) Mdec
inaccurate = (identity &&& now) >>> (delayE ı⁺ *** identity)

---------------------------------------------------------------------------------------
