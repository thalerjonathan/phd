{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude
open import Coinduction
open import Nat
open import List
open import Vector

module Stream where

infix  3  _≈_
infixr 16 _∷_

-- Streams -------------------------------------------------

data Stream (A : Set) : Set where
  _∷_ : A → ∞ (Stream A) → Stream A

shead : {A : Set} → Stream A → A
shead (a ∷ _) = a

stail : {A : Set} → Stream A → Stream A
stail (_ ∷ as) = ♭ as

sdrop : {A : Set} → ℕ → Stream A → Stream A
sdrop O as = as
sdrop (S n) (_ ∷ as) = sdrop n (♭ as)

stake : {A : Set} → (n : ℕ) → Stream A → Vec A n
stake O _ = []
stake (S n) (a ∷ as) = a ∷ stake n (♭ as)

smap : {A B : Set} → (A → B) → Stream A → Stream B
smap f (a ∷ as) = f a ∷ (♯ smap f (♭ as))

srepeat : {A : Set} → A → Stream A
srepeat a = a ∷ (♯ srepeat a)

sinterleave : {A : Set} → Stream A → Stream A → Stream A
sinterleave (a ∷ as) bs = a ∷ (♯ sinterleave bs (♭ as))

_!!_ : {A : Set} → Stream A → ℕ → A
(a ∷ _)  !! O     = a
(_ ∷ as) !! (S n) = ♭ as !! n 

-- Colists -----------------------------------------------

data Colist (A : Set) : Set where
  []  : Colist A
  _∷_ : A → ∞ (Colist A) → Colist A

clmap : {A B : Set} → (A → B) → Colist A → Colist B
clmap _ [] = []
clmap f (a ∷ as) = f a ∷ (♯ clmap f (♭ as))

clinterleave : {A : Set} → Colist A → Colist A → Colist A
clinterleave [] bs = bs
clinterleave (a ∷ as) bs = a ∷ (♯ clinterleave bs (♭ as))


-- Streams of ℕs -----------------------------------------

natsFrom : ℕ → Stream ℕ
natsFrom n = n ∷ (♯ natsFrom (S n))

nats : Stream ℕ
nats = natsFrom O


----------------------------------------------------------

listToColist : {A : Set} → List A → Colist A
listToColist [] = []
listToColist (a ∷ as) = a ∷ (♯ listToColist as)

streamToColist : {A : Set} → Stream A → Colist A
streamToColist (a ∷ as) = a ∷ (♯ streamToColist (♭ as))


-- Equivalence of Streams --------------------------------

data _≈_ {A} : Stream A → Stream A → Set where
  ≈∷ : {as bs : ∞ (Stream A)} → {a : A} → ∞ (♭ as ≈ ♭ bs) → a ∷ as ≈ a ∷ bs

smapLem : {A B C : Set} → {f : A → B} → {g : B → C} → (as : Stream A) → smap g (smap f as) ≈ smap (g ∘ f) as
smapLem (_ ∷ as) = ≈∷ (♯ smapLem (♭ as))


-- Asynchronous Stream Processors ------------------------

data SP (A B : Set) : Set where
  get : (A → SP A B)   → SP A B
  put : B → ∞ (SP A B) → SP A B


⟦_⟧ : {A B : Set} → SP A B → Stream A → Stream B
⟦ get f ⟧ (a ∷ as) = ⟦ f a ⟧ (♭ as)
⟦ put b sp ⟧ as    = b ∷ (♯ ⟦ ♭ sp ⟧ as)


addSP : SP ℕ ℕ
addSP = get (λ m → get (λ n → put (m + n) (♯ addSP)))

-- blackHoleSP : {A B : Set} → SP A B
-- blackHoleSP = get (λ _ → {!blackHoleSP!})

constantSP : {A B : Set} → B → SP A B
constantSP b = put b (♯ constantSP b)

identitySP : {A : Set} → SP A A
identitySP = get (λ a → put a (♯ identitySP))

