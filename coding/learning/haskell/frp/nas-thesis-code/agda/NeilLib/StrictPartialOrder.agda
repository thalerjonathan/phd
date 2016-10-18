{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module StrictPartialOrder {A : Set} (_<'_ : Rel A)

                    (irreflex : Irreflexive _<'_)
                    (asym     : Asymmetric _<'_)
                    (transit  : Transitive _<'_)

where

import StrictPreOrder
open StrictPreOrder _<'_ irreflex transit public

asymmetric : Asymmetric _<_
asymmetric = asym

------------------------------------------------------------------

antisymmetric : Antisymmetric _≤_
antisymmetric (inl p) (inl q) = absurd (asym p q)
antisymmetric _ (inr refl)    = refl
antisymmetric (inr refl) _    = refl

<≤-asym : {a b : A} → a < b → Not (b ≤ a)
<≤-asym p (inl q)    = asym p q
<≤-asym p (inr refl) = irreflex p

≤<-asym : {a b : A} → a ≤ b → Not (b < a)
≤<-asym (inl p)    = asym p
≤<-asym (inr refl) = irreflex

------------------------------------------------------------------
