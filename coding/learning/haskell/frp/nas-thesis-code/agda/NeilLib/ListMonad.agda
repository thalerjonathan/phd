{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude

module ListMonad where

open import ListProps

lm5thLaw : {A B : Set} -> (as : List A) -> concat {B} (map (const []) as) ≡ []
lm5thLaw [] = refl
lm5thLaw (_ ∷ as) = lm5thLaw as

lm3rdLaw : {A B C : Set} {f : A -> List B} {g : B -> List C} (as : List A)
           -> concat (map g (concat (map f as))) ≡ concat (map (\b -> concat (map g (f b))) as)
lm3rdLaw [] = refl
lm3rdLaw {_} {_} {_} {f} {g} (a ∷ as) = resp (_++_ (concat (map g (f a)))) (lm3rdLaw as) ∘≡ concat++ {_} {map g (f a)} ∘≡ resp concat (map++ {_} {f a})


import MonadPlus
open module MM = MonadPlus List (\as f -> concat (map f as)) wrap _++_ [] ++[] concatMapWrap lm3rdLaw refl lm5thLaw refl ++[] public

