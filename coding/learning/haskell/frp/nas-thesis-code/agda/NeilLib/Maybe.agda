{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module Maybe where

maybe : {A B : Set} → B → (A → B) → Maybe A → B 
maybe b _ nothing = b
maybe _ f (just a) = f a

fromMaybe : {A : Set} → A → Maybe A → A
fromMaybe a nothing  = a
fromMaybe a (just b) = b

maybeMerge : {A B C : Set} → (A → C) → (B → C) → (A → B → C) → Maybe A → Maybe B → Maybe C
maybeMerge fa fb fab nothing nothing   = nothing
maybeMerge fa fb fab nothing (just b)  = just (fb b)
maybeMerge fa fb fab (just a) nothing  = just (fa a)
maybeMerge fa fb fab (just a) (just b) = just (fab a b)

maybeMergeWith : {A : Set} → (A → A → A) → Maybe A → Maybe A → Maybe A
maybeMergeWith f (just a) (just b) = just (f a b)
maybeMergeWith f nothing mb = mb
maybeMergeWith f ma nothing = ma

maybeMap : {A B : Set} → (A → B) → Maybe A → Maybe B
maybeMap f = maybe nothing (just ∘ f)

maybeMap2 : {A B Z : Set} → (A → B → Z) → Maybe A → Maybe B → Maybe Z
maybeMap2 f nothing   mb  = nothing
maybeMap2 f (just a)  mb  = maybeMap (f a) mb

maybeFilter : {A : Set} → (A → Bool) → Maybe A → Maybe A
maybeFilter p nothing  = nothing
maybeFilter p (just a) with p a
... | true  = just a
... | false = nothing

just-inj : {A : Set} → Injective {A} just
just-inj refl = refl