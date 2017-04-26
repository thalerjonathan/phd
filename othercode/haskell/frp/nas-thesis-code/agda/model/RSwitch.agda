{-# OPTIONS --type-in-type  --no-termination-check #-}

module RSwitch where

open import SigVecs
open import NaryFRP
open import Library1

------------------------------------------------------

-- rswitch is an infinite term and should only be evaluated lazily

rswitch : {as bs : SVDesc} → {A : Set} → SF as (bs , E A) → (A → SF as (bs , E A)) → SF as bs
rswitch {as} {bs} {A} sf f = switch {as} {bs} sf (λ e → rswitch {as} {bs} (_>>>_ {as} {bs , E A} {bs , E A} (f e) (sfSecond {bs} {E A} {E A} notYet)) f)

------------------------------------------------------

