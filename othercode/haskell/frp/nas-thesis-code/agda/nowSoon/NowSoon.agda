{-# OPTIONS --type-in-type #-}

module NowSoon where

open import NeilPrelude
open import RealTime
open import TimeDeltaList
open import SVDesc

-----------------------------------------------------

module NowSoon1 where

  data When (A : Set) : Set where
    now   : A → When A
    soon  : A → When A

  ChangeList : Set → Set
  ChangeList = TimeDeltaList

  ChangePrefix : Set → Set
  ChangePrefix A = Time → ChangeList (When A)

  SigVec : SVDesc → Set
  SigVec (C A)      = Time → A
  SigVec (E A)      = Maybe (When A) × ChangePrefix A
  SigVec (S A)      = A × ChangePrefix A
  SigVec (as , bs)  = SigVec as × SigVec bs

-----------------------------------------------------

module NowSoon2 where

  data When (A : Set) : Set where
    now      : A      → When A
    soon     : A      → When A
    nowSoon  : A → A  → When A

  ChangeList : Set → Set
  ChangeList = TimeDeltaList

  ChangePrefix : Set → Set
  ChangePrefix A = Time → ChangeList (When A)

  SigVec : SVDesc → Set
  SigVec (C A)      = Time → A
  SigVec (E A)      = Maybe (When A) × ChangePrefix A
  SigVec (S A)      = A × Maybe A × ChangePrefix A
  SigVec (as , bs)  = SigVec as × SigVec bs

-----------------------------------------------------

module SuperDenseTime where

  open import Nat

  ChangeList : Set → Set
  ChangeList = TDList

  ChangePrefix : Set → Set
  ChangePrefix A = Time → ChangeList (((Δt × ℕ) ⊎ ℕ⁺) × A)

  SigVec : SVDesc → Set
  SigVec (C A)      = Time → A
  SigVec (E A)      = Maybe A × ChangePrefix A
  SigVec (S A)      = A × ChangePrefix A
  SigVec (as , bs)  = SigVec as × SigVec bs

-----------------------------------------------------
