{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module DirectedCompletePartialOrder {A : Set}

                    (_≤_ : Rel A)

                    (_⊔_ : Op A)

                    (reflex   : Reflexive _≤_)
                    (antisym  : Antisymmetric _≤_)
                    (transit  : Transitive _≤_)

                    (supL'     : {a b : A} → a ≤ (a ⊔ b))
                    (supR'     : {a b : A} → a ≤ (b ⊔ a))
                    (leastSup' : {x a b : A} → a ≤ x → b ≤ x → (a ⊔ b) ≤ x)

where

supL : {a b : A} → a ≤ (a ⊔ b)
supL = supL'

supR : {a b : A} → a ≤ (b ⊔ a)
supR = supR'

leastSup : {x a b : A} → a ≤ x → b ≤ x → (a ⊔ b) ≤ x
leastSup = leastSup'

supResp : {a b c d : A} → a ≤ c → b ≤ d → (a ⊔ b) ≤ (c ⊔ d)
supResp p q = leastSup (transit p supL) (transit q supR)

supRespL : {a b c : A} →  a ≤ c → (a ⊔ b) ≤ (c ⊔ b)
supRespL p = supResp p reflex

supRespR : {a b c : A} →  b ≤ c → (a ⊔ b) ≤ (a ⊔ c)
supRespR = supResp reflex

idem : Idempotent _⊔_
idem = antisym (leastSup reflex reflex) supL

leastSupR : {x a b : A} → (a ⊔ b) ≤ x → (a ≤ x) × b ≤ x
leastSupR p = transit supL p , transit supR p

associative : Associative _⊔_
associative = antisym (leastSup (leastSup supL (transit supL supR)) (transit supR supR))
                      (leastSup (transit supL supL) (leastSup (transit supR supL) supR))

commutative : Commutative _⊔_
commutative = antisym (leastSup supR supL) (leastSup supR supL)

import PartialOrder
open PartialOrder _≤_ reflex antisym transit public

import CommSemiGroup
open CommSemiGroup _⊔_ associative commutative

supTransR : {l a b : A} → a ≤ b → a ≤ (l ⊔ b)
supTransR {l} p = transitive p supR

supTransL : {r a b : A} → a ≤ b → a ≤ (b ⊔ r)
supTransL {r} p = transitive p supL
