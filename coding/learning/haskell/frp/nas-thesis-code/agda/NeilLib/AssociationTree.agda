{-# OPTIONS --type-in-type #-}

open import NeilPrelude

module AssociationTree (K : Set)
                       (_<_ : K → K → Set)
                       (irreflex : {k : K} → Not (k < k))
                       (asym     : {j k : K} → j < k → Not (k < j))
                       (transit  : {i j k : K} → i < j → j < k → i < k)
                       (compare  : (j k : K) → OrdCompare _<_ j k)
where

import OrdCompare
open OrdCompare _<_ irreflex asym transit compare

-------------------------------------------------------------------------------------------

data ATree (V : Set) : Set where
  leaf : ATree V
  node : (l : ATree V) → (k : K) → (v : V) → (r : ATree V) → ATree V

-------------------------------------------------------------------------------------------

empty : {V : Set} → ATree V
empty = leaf

insert : {V : Set} → K → V → ATree V → ATree V
insert k v leaf = node leaf k v leaf
insert k v (node l k' v' r) with compare k k'
insert k v (node l .k v' r) | refl   = node l k v r
insert k v (node l k' v' r) | less p = node (insert k v l) k' v' r
insert k v (node l k' v' r) | more p = node l k' v' (insert k v r)

lookup : {V : Set} → K → ATree V → Maybe V
lookup k leaf = nothing
lookup k (node l k' v r) with compare k k'
lookup k (node l .k v r) | refl   = just v
lookup k (node l k' v r) | less p = lookup k l
lookup k (node l k' v r) | more p = lookup k r

extractLeast : {V : Set} → (t : ATree V) → (ATree V × K × V) ⊎ t ≡ leaf
extractLeast leaf = inr refl
extractLeast (node l k v r) with extractLeast l
extractLeast (node .leaf k v r) | inr refl = inl (r & k & v)
extractLeast (node l k v r)     | inl (l' & k₀ & v₀) = inl (node l' k v r & k₀ & v₀)

extractGreatest : {V : Set} → (t : ATree V) → (ATree V × K × V) ⊎ t ≡ leaf
extractGreatest leaf = inr refl
extractGreatest (node l k v r) with extractGreatest r
extractGreatest (node l k v .leaf) | inr refl = inl (l & k & v)
extractGreatest (node l k v r)     | inl (r' & k₁ & v₁) = inl (node l k v r' & k₁ & v₁)

delete : {V : Set} → K → ATree V → ATree V
delete k leaf = leaf
delete k (node l k' v r) with compare k k'
delete k (node l k' v r) | less p = node (delete k l) k' v r
delete k (node l k' v r) | more p = node l k' v (delete k r)
delete k (node l .k _ r) | refl with extractLeast r
delete k (node l .k _ r) | refl | inl (r' & k₀ & v₀) = node l k₀ v₀ r'
delete k (node l .k _ .leaf) | refl | inr refl = l

-------------------------------------------------------------------------------------------
