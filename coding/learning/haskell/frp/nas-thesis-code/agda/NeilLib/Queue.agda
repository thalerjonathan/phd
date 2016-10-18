{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import Nat
open import MaybeMonad
open import List

module Queue where

-- When adding a list to a queue, the head of the list joins the queue first
-- When taking from a queue, the head of the resultant list is the first out of the queue

-- Back × Front

Queue : Set → Set
Queue A = List A × List A

emptyQueue : {A : Set} → Queue A
emptyQueue = [] , []

newQueue : {A : Set} → List A → Queue A
newQueue as = [] , as

enQueue : {A : Set} → A → Queue A → Queue A
enQueue = first ∘ _∷_

singletonQueue : {A : Set} → A → Queue A
singletonQueue a = enQueue a emptyQueue 

deQueue : {A : Set} → Queue A → Maybe (Queue A × A)
deQueue (zs , a ∷ as) = just ((zs , as) , a)
deQueue (zs , []) with reverse zs
... | []     = nothing
... | a ∷ as = just (newQueue as , a)

enQueues : {A : Set} → List A → Queue A → Queue A
enQueues as q = foldl (flip enQueue) q as

deQueueWhile : {A : Set} → (A → Bool) → Queue A → Queue A × List A
deQueueWhile p (zs , as) with span p as
... | pas , []  = (_,_ [] ∥ _++_ pas ∘ swap ∘ span p ∘ reverse) zs
... | pas , npas = (zs , npas) , pas

deQueueWhileLast : {A : Set} → (A → Bool) → Queue A → Maybe (Queue A × A)
deQueueWhileLast p q with deQueueWhile p q
... | _  , []       = nothing
... | q' , (a ∷ as) = just (q' , safelast a as)

deQueueIf : {A : Set} → (A → Bool) → Queue A → Maybe (Queue A × A)
deQueueIf p q = deQueue q >>= \qa →
                guard (p (snd qa)) >>
                return qa

qmap : {A B : Set} → (A → B) → Queue A → Queue B 
qmap f = map f ∥ map f

qreverse : {A : Set} → Queue A → Queue A
qreverse = swap

qlength : {A : Set} → Queue A → ℕ
qlength (as , bs) = length as + length bs
