{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude
open import Recursion
open import List

module QuickSort {t : OrdType} where

A : Set
A = ordSet t


import ListSize
open module ListySize = ListSize {A}


qpartition : (A -> Bool) -> (as : List A) -> Σ (List A × List A) (\asbs -> (uncurry _++_) asbs ≈ as)
qpartition p [] = fork [] & fork ≼[]
qpartition p (a ∷ as) with qpartition p as
... | (bs & cs) & lt₁ & lt₂ = if p a
                        then (a ∷ bs & cs) & ≼∷ lt₁ & ≼∷ lt₂
                        else (bs & a ∷ cs) & (flip ≼trans (≼∷ {a} lt₁) ∥ ≼trans (≼∷ lt₂)) (≼mid bs)


quicksort : (as : List A) -> Σ (List A) (\bs -> bs ≈ as)
quicksort as = wfrec as g accL
  where
        g : (bs : List A) -> AccL bs -> ((cs : List A) -> cs ≺ bs -> Σ (List A) (\ds -> ds ≈ cs)) -> Σ (List A) (\ds -> ds ≈ bs)
        g [] Abs f = [] & fork ≼[]
        g (b ∷ bs) Abs f with qpartition (_>=_ b) bs
        ... | (pass & fail) & lt₁ & gt₁ with ≼++split pass lt₁
        ...   | ltp₂ & ltf₂ with f pass ltp₂ | f fail ltf₂
        ...     | passS & ltp₃ & gtp₃ | failS & ltf₃ & gtf₃ = passS ++ b ∷ failS & ≼trans (fst (≼mid {b} {b} passS)) (≼∷ (≼trans (≼++ ltp₃ ltf₃) lt₁))
                                                                                 & ≼trans (≼∷ (≼trans gt₁ (≼++ gtp₃ gtf₃))) (snd (≼mid {b} {b} passS))

qsort : List A -> List A
qsort = fst ∘' quicksort
