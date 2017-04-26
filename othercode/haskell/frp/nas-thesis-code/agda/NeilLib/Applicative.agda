{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module Applicative (F : Set → Set)

       (fmap   : {A B : Set} → (A → B) → F A → F B)
       (pure   : {A : Set} → A → F A)
       (_<**>_ : {A B : Set} → F (A → B) → F A → F B) 

where

import Pointed
open Pointed F fmap pure public

_<*>_  : {A B : Set} → F (A → B) → F A → F B
_<*>_ = _<**>_

fmap2 : {A B C : Set} → (A → B → C) → F A → F B → F C
fmap2 f a b = fmap f a <*> b

fmap3 : {A B C D : Set} → (A → B → C → D) → F A → F B → F C → F D
fmap3 f a b c = fmap2 f a b <*> c

fmap4 : {A B C D E : Set} → (A → B → C → D → E) → F A → F B → F C → F D → F E
fmap4 f a b c d = fmap3 f a b c <*> d

_<∘>_ : {A B C : Set} → F (B → C) → F (A → B) → F (A → C)
_<∘>_ = fmap2 _∘_

----------------------------------------------

-- Applicative Functors are equivalent to Static Arrows

-- Arr : Set → Set → Set
-- Arr A B = F (A → B)

-- import StaticArrow
-- open StaticArrow Arr pure (fmap2 _⋙_) (fmap first) (fmap const)

-- force' : {A B : Set} → Arr Unit (A → B) → Arr A B
-- force' = force