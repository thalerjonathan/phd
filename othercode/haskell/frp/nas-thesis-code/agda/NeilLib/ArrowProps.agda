{-# OPTIONS --type-in-type #-}

open import NeilPrelude hiding (result ; apply) renaming (first to first` ; second to second`)
open import Equational

module ArrowProps (Arr : Set → Set → Set)

       (arr    : {A B : Set} → (A → B) → Arr A B) 
       (_>>>'_  : {A B C : Set} → Arr A B → Arr B C → Arr A C)
       (first  : {A B C : Set} → Arr A B → Arr (A × C) (B × C))

-- Arrow laws

       (1stLaw : {A B : Set} → {f : Arr A B} → (arr id >>>' f) ≡ f)
       (3rdLaw : {A B C D : Set} → {f : Arr A B} → {g : Arr B C} → {h : Arr C D} → ((f >>>' g) >>>' h) ≡ (f >>>' (g >>>' h)))
       (4thLaw : {A B C : Set} → {f : A → B} → {g : B → C} → arr (g ∘ f) ≡ arr f >>>' arr g)
       (5thLaw : {A B C : Set} → {f : A → B} → first {_} {_} {C} (arr f) ≡ arr (first` f))
       (6thLaw : {A B C D : Set} → {f : Arr A B} → {g : Arr B C} → first {_} {_} {D} (f >>>' g) ≡ first f >>>' first g)
       (7thLaw : {A B C D : Set} → {f : Arr A B} → {g : C → D} → first f >>>' arr (second` g) ≡ arr (second` g) >>>' first f)
       (8thLaw : {A B C : Set} → {f : Arr A B} → first {_} {_} {C} f >>>' arr fst ≡ arr fst >>>' f)
       (9thLaw : {A B C D : Set} → {f : Arr A B} → first {A × C} {_} {D} (first f) >>>' arr ×-assocR ≡ arr ×-assocR >>>' first f)

where

----------------------------------------------

import Arrow
open Arrow Arr arr _>>>'_ first

-- The Second Arrow Law was proved redundant in the Arrow Calculus Paper (Lindley, Wadler and Yallop)

2ndLaw : {A B : Set} → {f : Arr A B} → (f >>> arr id) ≡ f
2ndLaw {A} {B} {f} =  f >>> arr id
                          ≡⟨ sym 1stLaw ⟩
                      arr id >>> f >>> arr id
                          ≡⟨ refl ⟩
                      arr (fst ∘ dup) >>> f >>> arr id
                          ≡⟨ cong2R _>>>_ 4thLaw ⟩
                      (arr dup >>> arr fst) >>> f >>> arr id
                          ≡⟨ trans 3rdLaw (cong2L _>>>_ (sym 3rdLaw)) ⟩
                      arr dup >>> (arr fst >>> f) >>> arr id
                          ≡⟨ cong2L _>>>_ (cong2R _>>>_ (sym 8thLaw)) ⟩
                      arr dup >>> (first f >>> arr fst) >>> arr id
                          ≡⟨ cong2L _>>>_ 3rdLaw ⟩
                      arr dup >>> first f >>> arr fst >>> arr id
                          ≡⟨ cong2L _>>>_ (cong2L _>>>_ (sym 4thLaw)) ⟩
                      arr dup >>> first f >>> arr (id ∘ fst)
                          ≡⟨ refl ⟩
                      arr dup >>> first f >>> arr fst
                          ≡⟨ cong2L _>>>_ 8thLaw ⟩                          
                      arr dup >>> arr fst >>> f
                          ≡⟨ sym 3rdLaw ⟩
                      (arr dup >>> arr fst) >>> f
                          ≡⟨ cong2R _>>>_ (sym 4thLaw) ⟩
                      arr (fst ∘ dup) >>> f
                          ≡⟨ 1stLaw ⟩
                      f
                          QED

----------------------------------------------
