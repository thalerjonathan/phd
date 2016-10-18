module Intro where

data Unit : Set where
  unit : Unit

data Bool : Set where
  false  : Bool
  true   : Bool

data ℕ : Set where
  zero  : ℕ
  succ  : ℕ → ℕ

data Maybe (A : Set) : Set where
  nothing  : Maybe A
  just     : A → Maybe A

data List (A : Set) : Set where
  []   : List A
  _∷_  : A → List A → List A

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

data _⊎_ (A B : Set) : Set where
  inl  : A → A ⊎ B
  inr  : B → A ⊎ B

data Vec (A : Set) : ℕ → Set where
  []   : Vec A zero
  _∷_  : {n : ℕ} → A → Vec A n → Vec A (succ n) 

data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (a : A) → B a → Σ A B

isZero : ℕ → Bool
isZero zero      = true
isZero (succ _)  = false

id₀ : (A : Set) → A → A
id₀ A a = a

id : {A : Set} → A → A
id a = a

id₁ : {A : Set} → A → A
id₁ {A} a = a

idBool₁ : Bool → Bool
idBool₁ = id

idBool₂ : Bool → Bool
idBool₂ = id {Bool}

listToVec : {A : Set} → List A → Σ ℕ (Vec A)
listToVec [] = (zero , [])
listToVec (a ∷ as) with listToVec as
... | (n , v) = (succ n , (a ∷ v))


data False : Set where

True : Set
True = Unit

Not : Set → Set
Not A = A → False

Not₀ : Set → Set
Not₀ = λ A → A → False

IsNothing : {A : Set} → Maybe A → Set
IsNothing nothing  = True
IsNothing (just _) = False

IsJust : {A : Set} → Maybe A → Set
IsJust ma = Not (IsNothing ma)


data _≡_ {A : Set} : A → A → Set where
  refl : {a : A} → a ≡ a

data _<_ : ℕ → ℕ → Set where
  zlt : ∀ {n}            → zero    < succ n
  slt : ∀ {m n} → m < n  → succ m  < succ n

_≤_ : ℕ → ℕ → Set
m ≤ n = (m < n) ⊎ (m ≡ n)

sub : (m n : ℕ) → (n ≤ m) → ℕ
sub m         zero       p                = m
sub zero      (succ n)   (inl ())
sub zero      (succ n)   (inr ())
sub (succ m)  (succ n)   (inl (slt p))    = sub m n (inl p)
sub (succ m)  (succ .m)  (inr refl)       = zero
