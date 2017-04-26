{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude
open import List


module ListSize {A : Set} where

open import Recursion

infix 4 _≼_ _≈_ _≺_

data _≼_ : List A -> List A -> Set where
  ≼[] : {bs : List A} -> [] ≼ bs
  ≼∷  : {a b : A} -> {as bs : List A} -> as ≼ bs -> (a ∷ as) ≼ (b ∷ bs)

≼trans : {as bs cs : List A} -> as ≼ bs -> bs ≼ cs -> as ≼ cs
≼trans ≼[] _ = ≼[]
≼trans (≼∷ lt₁) (≼∷ lt₂) = ≼∷ (≼trans lt₁ lt₂)

≼refl' : (as : List A) -> as ≼ as
≼refl' = listrec ≼[] ≼∷

≼refl : {as : List A} -> as ≼ as
≼refl {as} = ≼refl' as

≼resp : {a b : A} -> {as bs : List A} -> a ∷ as ≼ b ∷ bs -> as ≼ bs
≼resp (≼∷ lt) = lt 

≼∷R' : {a : A} -> (as : List A) -> as ≼ (a ∷ as)
≼∷R' [] = ≼[]
≼∷R' (_ ∷ as) = ≼∷ (≼∷R' as)

≼∷R : {a : A} -> {as : List A} -> as ≼ (a ∷ as)
≼∷R {_} {as} = ≼∷R' as

_≺_ : List A -> List A -> Set
as ≺ []       = False
as ≺ (_ ∷ bs) = as ≼ bs

_≈_ : List A -> List A -> Set
as ≈ bs = as ≼ bs × bs ≼ as


≼mid : {a b : A} -> {bs : List A} -> (as : List A) -> as ++ b ∷ bs ≈ a ∷ (as ++ bs)
≼mid [] = ≼∷ ≼refl & ≼∷ ≼refl
≼mid (_ ∷ as) = (≼∷ ∥ ≼∷) (≼mid as)

≼++[]' : (as : List A) -> as ≼ as ++ []
≼++[]' = listrec ≼[] ≼∷

≼++[] : {as : List A} -> as ≼ as ++ []
≼++[] {as} = ≼++[]' as

≼++bs : {bs : List A} -> (as : List A) -> bs ≼ as ++ bs
≼++bs = listrec ≼refl (flip ≼trans ≼∷R)

≼++ : {as bs cs ds : List A} -> as ≼ cs -> bs ≼ ds -> as ++ bs ≼ cs ++ ds
≼++ ≼[] ≼[] = ≼[]
≼++ (≼[] {cs}) (≼∷ {_} {d} lt₂) = ≼trans (≼∷ {_} {d} (≼trans lt₂ (≼++bs cs))) (snd (≼mid cs))
≼++ (≼∷ lt₁) ≼[] = ≼∷ (≼++ lt₁ ≼[])
≼++ (≼∷ lt₁) (≼∷ lt₂) = ≼∷ (≼++ lt₁ (≼∷ lt₂))

≼++R : {bs cs : List A} -> (as : List A) -> bs ≼ cs -> bs ++ as ≼ cs ++ as
≼++R as = flip ≼++ ≼refl

≼++L : {bs cs : List A} -> (as : List A) -> bs ≼ cs -> as ++ bs ≼ as ++ cs
≼++L as = ≼++ (≼refl' as)

≼++as : {as : List A} -> (bs : List A) -> as ≼ as ++ bs
≼++as {as} bs = ≼trans ≼++[] (≼++L as ≼[])

≼++split : {bs cs : List A} -> (as : List A) -> as ++ bs ≼ cs -> as ≼ cs × bs ≼ cs
≼++split {bs} = listcases (_&_ ≼[]) (\_ as -> ≼trans (≼∷ (≼++as bs)) ∥ ≼trans (≼trans (≼++bs as) ≼∷R) ∘ fork)


-- Lists are accessible (for well founded recursion) -- 

AccL : List A -> Set
AccL = Acc (List A) _≺_

accLAux : (as bs : List A) -> as ≼ bs -> AccL as
accLAux [] bs ≼[] = acc-init [] (const id)
accLAux .(a ∷ as) .(b ∷ bs) (≼∷ {a} {b} {as} {bs} lt) = acc (\zs ltz -> accLAux zs bs (≼trans ltz lt))

accL' : (as : List A) -> AccL as
accL' as = accLAux as as ≼refl

accL : {as : List A} -> AccL as
accL {as} = accL' as