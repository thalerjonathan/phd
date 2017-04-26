{-# OPTIONS --type-in-type  #-}

module Library1 where

open import NeilPrelude
open import SigVecs
open import NaryFRP

------------------------------------------------------

-- For human-readable versions of this library code
-- (i.e. without the implicit arguments)
-- see the Agda embeddings 

------------------------------------------------------

sfSwap : {as bs : SVDesc} → SF (as , bs) (bs , as)
sfSwap {as} {bs} = _&&&_ {as , bs} {bs} {as} (sfSnd {as} {bs}) (sfFst {as} {bs})

toFst : {as bs cs : SVDesc} → SF as cs → SF (as , bs) cs
toFst {as} {bs} {cs} sf = _>>>_ {as , bs} {as} {cs} (sfFst {as} {bs}) sf

toSnd : {as bs cs : SVDesc} → SF bs cs → SF (as , bs) cs
toSnd {as} {bs} {cs} sf = _>>>_ {as , bs} {bs} {cs} (sfSnd {as} {bs}) sf

_***_ : {as bs cs ds : SVDesc} → SF as cs → SF bs ds → SF (as , bs) (cs , ds)
_***_ {as} {bs} {cs} {ds} sf₁ sf₂ = _&&&_ {as , bs} {cs} {ds} (toFst {as} {bs} {cs} sf₁) (toSnd {as} {bs} {ds} sf₂)

sfFirst : {as bs cs : SVDesc} → SF as bs → SF (as , cs) (bs , cs)
sfFirst {as} {bs} {cs} sf = _***_ {as} {cs} {bs} {cs} sf (identity {cs})

sfSecond : {as bs cs : SVDesc} → SF bs cs → SF (as , bs) (as , cs)
sfSecond {as} {bs} {cs} sf = _***_ {as} {bs} {as} {cs} (identity {as}) sf

sfFork : {as : SVDesc} → SF as (as , as)
sfFork {as} = _&&&_ {as} {as} {as} (identity {as}) (identity {as})

forkFirst : {as bs : SVDesc} → SF as bs → SF as (bs , as)
forkFirst {as} {bs} sf = _&&&_ {as} {bs} {as} sf (identity {as})

forkSecond : {as bs : SVDesc} → SF as bs → SF as (as , bs)
forkSecond {as} {bs} sf = _&&&_ {as} {as} {bs} (identity {as}) sf

sfAssocR : {as bs cs : SVDesc} → SF ((as , bs) , cs) (as , (bs , cs))
sfAssocR {as} {bs} {cs} = _&&&_ {(as , bs) , cs} {as} {bs , cs} (toFst {as , bs} {cs} {as} (sfFst {as} {bs})) (sfFirst {as , bs} {bs} {cs} (sfSnd {as} {bs}))

sfAssocL : {as bs cs : SVDesc} → SF (as , (bs , cs)) ((as , bs) , cs)
sfAssocL {as} {bs} {cs} = _&&&_ {as , (bs , cs)} {as , bs} {cs} (sfSecond {as} {bs , cs} {bs} (sfFst {bs} {cs})) (toSnd {as} {bs , cs} {cs} (sfSnd {bs} {cs}))

-------------------------------------------------------------------
