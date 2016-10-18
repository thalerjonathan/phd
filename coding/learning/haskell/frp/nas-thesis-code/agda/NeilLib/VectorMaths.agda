{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import Vector
open import Nat

module VectorMaths (A : Set) (zero : A) (one : A) (plus : Op A) (mult : Op A) where

vector : ℕ → Set
vector = Vec A

vecZero : {n : ℕ} → vector n
vecZero = vec zero

vecOne : {n : ℕ} → vector n
vecOne = vec one

vec+vec : {n : ℕ} → vector n → vector n → vector n
vec+vec = vzipWith plus

vdot : {n : ℕ} → vector n → vector n → A
vdot v1 v2 = vfoldl plus zero (vzipWith mult v1 v2)


-- Matrices ------------

matrix : ℕ → ℕ → Set
matrix = Matrix A

matZero : {m n : ℕ} → matrix m n
matZero = mat zero

matOne : {m n : ℕ} → matrix m n
matOne = mat one

matId : {n : ℕ} → matrix n n
matId {O}   = []
matId {S n} = _∷_ (one ∷ vec zero) (vmap (_∷_ zero) (matId {n}))

vec*mat : {m n : ℕ} → vector m → matrix m n → vector n
vec*mat v m = vmap (vdot v) m

mat*vec : {m n : ℕ} → matrix m n → vector n → vector m
mat*vec m v = vec*mat v (transpose m)

mat*mat : {l m n : ℕ} → matrix l m → matrix m n → matrix l n
mat*mat m1 m2 = vmap (mat*vec m1) m2

mat+mat : {m n : ℕ} → matrix m n → matrix m n → matrix m n
mat+mat = mzipWith plus
