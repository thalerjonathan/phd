{-# OPTIONS --type-in-type #-}

open import NeilPrelude renaming (first to first` ; second to second`)

module HigherOrderArrowProps (Arr : Set → Set → Set)

       (arr      : {A B : Set} → (A → B) → Arr A B) 
       (_>>>'_   : {A B C : Set} → Arr A B → Arr B C → Arr A C)
       (first    : {A B C : Set} → Arr A B → Arr (A × C) (B × C))
       (second'  : {A B C : Set} → Arr A B → Arr (C × A) (C × B))
       (app      : {A B : Set} → Arr (Arr A B × A) B)

-- Arrow laws

       (1stLaw : {A B : Set} → {f : Arr A B} → (arr id >>>' f) ≡ f)
       (3rdLaw : {A B C D : Set} → {f : Arr A B} → {g : Arr B C} → {h : Arr C D} → ((f >>>' g) >>>' h) ≡ (f >>>' (g >>>' h)))
       (4thLaw : {A B C : Set} → {f : A → B} → {g : B → C} → arr (g ∘ f) ≡ arr f >>>' arr g)
       (5thLaw : {A B C : Set} → {f : A → B} → first {_} {_} {C} (arr f) ≡ arr (first` f))
       (6thLaw : {A B C D : Set} → {f : Arr A B} → {g : Arr B C} → first {_} {_} {D} (f >>>' g) ≡ first f >>>' first g)
       (7thLaw : {A B C D : Set} → {f : Arr A B} → {g : C → D} → first f >>>' arr (second` g) ≡ arr (second` g) >>>' first f)
       (8thLaw : {A B C : Set} → {f : Arr A B} → first {_} {_} {C} f >>>' arr fst ≡ arr fst >>>' f)
       (9thLaw : {A B C D : Set} → {f : Arr A B} → first {A × C} {_} {D} (first f) >>>' arr ×-assocR ≡ arr ×-assocR >>>' first f)

       (HOLaw1 : {A B : Set} → first {A} {Arr B (A × B)} {B} (arr (λ a → arr (λ b → (a , b)))) >>>' app ≡ arr id)
--     A---[]---Arr B (A,B)---
--                            app---(A,B)      ≡      identity
--     B----------------------

       (HOLaw2 : {A B C : Set} → {g : Arr A B} → first (arr (_>>>'_ g)) >>>' app {A} {C} ≡ second' g >>>' app)
--     Arr B C---[g>>>]---Arr A C---                      Arr B C----------
--                                  app---C        ≡                       app---C
--     A----------------------------                      A---------[g]---B

       (HOLaw3 : {A B C : Set} → {h : Arr B C} → first (arr (flip _>>>'_ h)) >>>' app {A} {C} ≡ app >>>' h)
--     Arr A B---[>>>h]---Arr A C                         Arr A B----
--                                app---C          ≡                 app---B---[h]---C
--     A--------------------------                        A----------

where

----------------------------------------------

import HigherOrderArrow
open HigherOrderArrow Arr arr _>>>'_ first app

----------------------------------------------

import ArrowProps
open ArrowProps Arr arr _>>>'_ first 1stLaw 3rdLaw 4thLaw 5thLaw 6thLaw 7thLaw 8thLaw 9thLaw
