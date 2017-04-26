{-# OPTIONS --type-in-type #-}

-- Everything I want, as I want it.

module NeilPrelude where


-- Precedence -------------------------------------

-- Higher precedence numbers bind tighter

-- infix  0  if_then_else_

infix  0  _↔_
infixr 1  _⊎_
infixr 2  _×_ _,_

infix  3  _≡_ _≢_ _≅_ -- _≤_ _∈_ 

-- infixl 4 _>>=_ _>>_

-- infixr 5  _∨_ _xor_
-- infixr 6  _∧_

-- infix  8  _==_ _!=_ _<=_
-- infixl 10 _+_ _-_
-- infixl 11 _*_
-- infixr 12 _^_

-- infixr 13 _+m+_
-- infixr 15 _++_ _+v+_
-- infixr 16 _∷_

infixr 90 _∘_ _∘'_ _∘₂_ _∘₃_ _⋙_ _⋙'_
infixr 91 _∥_ _∥₂_ _&_ _&₂_ _&₃_ _&₄_ _&₅_ _&₆_


-- Basic Datatypes --------------------------------------------------------------

data Unit : Set where
  unit : Unit

data Bool : Set where
  false : Bool
  true  : Bool

{-# BUILTIN BOOL  Bool  #-}
{-# BUILTIN TRUE  true  #-}
{-# BUILTIN FALSE false #-}

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A

-- Dependent Products (Σ types)

data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (a : A) → B a → Σ A B

-- Cartesian Product

_×_ : (A B : Set) → Set
A × B = Σ A (λ _ → B)

-- Sum Types (a.k.a. co-products) --

data _⊎_ (A B : Set) : Set where
  inl : A → A ⊎ B
  inr : B → A ⊎ B


-- Propositional Equality

data False : Set where

absurd : {A : Set} → False → A
absurd ()

Not : Set → Set
Not A = A → False

Rel : Set → Set
Rel A = A → A → Set

data _≡_ {A : Set} (a : A) : A → Set where
  refl : a ≡ a 

{-# BUILTIN EQUALITY _≡_ #-}
{-# BUILTIN REFL refl #-}

Eq : (A : Set) → Rel A
Eq A = _≡_ {A}


-- Properties ----------------------------------------

Reflexive : {A : Set} → Rel A → Set
Reflexive {A} _~_ = {a : A} → a ~ a

Irreflexive : {A : Set} → Rel A → Set
Irreflexive {A} _~_ = {a : A} → Not (a ~ a)

Symmetric : {A : Set} → Rel A → Set
Symmetric {A} _~_ = {a b : A} → a ~ b → b ~ a

Asymmetric : {A : Set} → Rel A → Set
Asymmetric {A} _~_ = {a b : A} → a ~ b → Not (b ~ a)

Antisymmetric : {A : Set} → Rel A → Set
Antisymmetric {A} _~_ = {a b : A} → a ~ b → b ~ a → a ≡ b

Trans : {A : Set} → Rel A → Rel A → Rel A → Set
Trans {A} P Q R = {a b c : A} → P a b → Q b c → R a c

Trans2 : {A : Set} → Rel A → Rel A → Rel A → Rel A → Set
Trans2 {A} P Q R S = {a b c d : A} → P a b → Q b c → R c d → S a d

Trans3 : {A : Set} → Rel A → Rel A → Rel A → Rel A → Rel A → Set
Trans3 {A} P Q R S T = {a b c d e : A} → P a b → Q b c → R c d → S d e → T a e

Transitive : {A : Set} → Rel A → Set
Transitive R = Trans R R R

Transitive2 : {A : Set} → Rel A → Set
Transitive2 R = Trans2 R R R R

Transitive3 : {A : Set} → Rel A → Set
Transitive3 R = Trans3 R R R R R

Total : {A : Set} → Rel A → Set
Total {A} _~_ = {a b : A} → (a ~ b) ⊎ (b ~ a)

Trichotomy : (A B C : Set) → Set
Trichotomy A B C = (A × Not B × Not C) ⊎
                   (Not A × B × Not C) ⊎
                   (Not A × Not B × C)

Trichotomous : {A : Set} → Rel A → Set
Trichotomous {A} _~_ = {a b : A} → Trichotomy (a ≡ b) (a ~ b) (b ~ a)

Equivalence : {A : Set} → Rel A → Set
Equivalence R = Reflexive R × Symmetric R × Transitive R

------------------------------------------------------

Op : Set → Set
Op A = A → A → A

Associative : {A : Set} → Op A → Set
Associative {A} _⊕_ = {a b c : A} → (a ⊕ b) ⊕ c ≡ a ⊕ (b ⊕ c)

AssociativeR : {A : Set} → Op A → Set
AssociativeR {A} _⊕_ = {a b c : A} → a ⊕ (b ⊕ c) ≡ (a ⊕ b) ⊕ c

Comm : {A B : Set} → (A → A → B) → Set
Comm {A} _⊕_ = {a b : A} → a ⊕ b ≡ b ⊕ a

Commutative : {A : Set} → Op A → Set
Commutative = Comm

Idempotent : {A : Set} → Op A → Set
Idempotent {A} _⊕_ = {a : A} → a ⊕ a ≡ a

------------------------------------------------------

-- I should really create a more general case that subsumes both Injection and Injective

Injection : {A : Set} → Rel A → (A → A) → Set
Injection {A} _~_ f = {a b : A} → f a ~ f b → a ~ b

Injection2 : {A : Set} → Rel A → (A → A → A) → Set
Injection2 {A} _~_ f = {a₁ a₂ b₁ b₂ : A} → f a₁ b₁ ~ f a₂ b₂ → (a₁ ~ a₂) × (b₁ ~ b₂)

Injective : {A B : Set} → (A → B) → Set
Injective {A} f = {a b : A} → f a ≡ f b → a ≡ b

Injective2 : {A B C : Set} → (A → B → C) → Set
Injective2 {A} {B} f = {a₁ a₂ : A} → {b₁ b₂ : B} → f a₁ b₁ ≡ f a₂ b₂ → a₁ ≡ a₂ × b₁ ≡ b₂

CancellativeL : {A B C : Set} → (A → B → C) → Set
CancellativeL {A} f = {a : A} → Injective (f a)

CancellativeR : {A B C : Set} → (A → B → C) → Set
CancellativeR {A} {B} f = {a₁ a₂ : A} → {b : B} → f a₁ b ≡ f a₂ b → a₁ ≡ a₂

Congruent : {A B : Set} → (A → B) → Set
Congruent {A} f = {a b : A} → a ≡ b → f a ≡ f b

Congruent2 : {A B C : Set} → (A → B → C) → Set
Congruent2 {A} {B} f = {a₁ a₂ : A} → {b₁ b₂ : B} → a₁ ≡ a₂ → b₁ ≡ b₂ → f a₁ b₁ ≡ f a₂ b₂

Congruent2L : {A B C : Set} → (A → B → C) → Set
Congruent2L {A} {B} f = {a : A} → {b₁ b₂ : B} → b₁ ≡ b₂ → f a b₁ ≡ f a b₂

Congruent2R : {A B C : Set} → (A → B → C) → Set
Congruent2R {A} {B} f = {a₁ a₂ : A} → {b : B} → a₁ ≡ a₂ → f a₁ b ≡ f a₂ b

Congruent3 : {A B C D : Set} → (A → B → C → D) → Set
Congruent3 {A} {B} {C} f = {a₁ a₂ : A} → {b₁ b₂ : B} → {c₁ c₂ : C} → a₁ ≡ a₂ → b₁ ≡ b₂ → c₁ ≡ c₂ → f a₁ b₁ c₁ ≡ f a₂ b₂ c₂

Congruent4 : {A B C D E : Set} → (A → B → C → D → E) → Set
Congruent4 {A} {B} {C} {D} f = {a₁ a₂ : A} → {b₁ b₂ : B} → {c₁ c₂ : C} → {d₁ d₂ : D} → a₁ ≡ a₂ → b₁ ≡ b₂ → c₁ ≡ c₂ → d₁ ≡ d₂ → f a₁ b₁ c₁ d₁ ≡ f a₂ b₂ c₂ d₂

cong : {A B : Set} → (f : A → B) → Congruent f
cong f refl = refl

cong2 : {A B C : Set} → (f : A → B → C) → Congruent2 f
cong2 f refl refl = refl

cong2L : {A B C : Set} → (f : A → B → C) → Congruent2L f
cong2L f = cong2 f refl

cong2R : {A B C : Set} → (f : A → B → C) → Congruent2R f
cong2R f eq = cong2 f eq refl

cong3 : {A B C D : Set} → (f : A → B → C → D) → Congruent3 f
cong3 f refl refl refl = refl

cong4 : {A B C D E : Set} → (f : A → B → C → D → E) → Congruent4 f
cong4 f refl refl refl refl = refl


-- Equality Properties -------------------------------

sym : {A : Set} → Symmetric (Eq A)
sym refl = refl

trans : {A : Set} → Transitive (Eq A)
trans refl refl = refl

trans2 : {A : Set} → Transitive2 (Eq A)
trans2 refl refl refl = refl

trans3 : {A : Set} → Transitive3 (Eq A)
trans3 refl refl refl refl = refl


-- Dependent Functions (Π types) ---------------------

Π : (A : Set) → (A → Set) → Set
Π A B = (a : A) → B a

-- Type Synonyms for dependent types

SetΠ : Set → Set
SetΠ A = A → Set 

SetΠ₂ : (A : Set) → SetΠ A → Set
SetΠ₂ A B = (a : A) → B a → Set 

SetΠ₃ : (A : Set) → (B : SetΠ A) → SetΠ₂ A B → Set
SetΠ₃ A B C = (a : A) → (b : B a) → (c : C a b) → Set 

SetΠ₄ : (A : Set) → (B : SetΠ A) → (C : SetΠ₂ A B) → SetΠ₃ A B C → Set
SetΠ₄ A B C D = (a : A) → (b : B a) → (c : C a b) → (d : D a b c) → Set 

SetΠ₅ : (A : Set) → (B : SetΠ A) → (C : SetΠ₂ A B) → (D : SetΠ₃ A B C) → SetΠ₄ A B C D → Set
SetΠ₅ A B C D E = (a : A) → (b : B a) → (c : C a b) → (d : D a b c) → (e : E a b c d) → Set 

SetΠ₆ : (A : Set) → (B : SetΠ A) → (C : SetΠ₂ A B) → (D : SetΠ₃ A B C) → (E : SetΠ₄ A B C D) → SetΠ₅ A B C D E → Set
SetΠ₆ A B C D E F = (a : A) → (b : B a) → (c : C a b) → (d : D a b c) → (e : E a b c d) → (f : F a b c d e) → Set 

SetΠ₇ : (A : Set) → (B : SetΠ A) → (C : SetΠ₂ A B) → (D : SetΠ₃ A B C) → (E : SetΠ₄ A B C D) → (F : SetΠ₅ A B C D E) → SetΠ₆ A B C D E F → Set
SetΠ₇ A B C D E F G = (a : A) → (b : B a) → (c : C a b) → (d : D a b c) → (e : E a b c d) → (f : F a b c d e) → (g : G a b c d e f) → Set 


-- Fully dependent function types

Π₂ : (A : Set) → (B : SetΠ A) → SetΠ₂ A B → Set
Π₂ A B C = (a : A) → Π (B a) (C a)

Π₃ : (A : Set) → (B : SetΠ A) → (C : SetΠ₂ A B) → SetΠ₃ A B C → Set
Π₃ A B C D = (a : A) → Π₂ (B a) (C a) (D a)

Π₄ : (A : Set) → (B : SetΠ A) → (C : SetΠ₂ A B) → (D : SetΠ₃ A B C) → SetΠ₄ A B C D → Set
Π₄ A B C D E = (a : A) → Π₃ (B a) (C a) (D a) (E a)

Π₅ : (A : Set) → (B : SetΠ A) → (C : SetΠ₂ A B) → (D : SetΠ₃ A B C) → (E : SetΠ₄ A B C D) → SetΠ₅ A B C D E → Set
Π₅ A B C D E F = (a : A) → Π₄ (B a) (C a) (D a) (E a) (F a)

Π₆ : (A : Set) → (B : SetΠ A) → (C : SetΠ₂ A B) → (D : SetΠ₃ A B C) → (E : SetΠ₄ A B C D) → (F : SetΠ₅ A B C D E) → SetΠ₆ A B C D E F → Set
Π₆ A B C D E F G = (a : A) → Π₅ (B a) (C a) (D a) (E a) (F a) (G a)

Π₇ : (A : Set) → (B : SetΠ A) → (C : SetΠ₂ A B) → (D : SetΠ₃ A B C) → (E : SetΠ₄ A B C D) → (F : SetΠ₅ A B C D E) → (G : SetΠ₆ A B C D E F) → SetΠ₇ A B C D E F G → Set
Π₇ A B C D E F G H = (a : A) → Π₆ (B a) (C a) (D a) (E a) (F a) (G a) (H a)


-- Function Combinators --

_∘_ : {A B C : Set} → (B → C) → (A → B) → (A → C)
f ∘ g = λ a → f (g a)

_∘'_ : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → ({a : A} → Π (B a) (C a)) → (g : Π A B) → (a : A) → C a (g a)
f ∘' g = λ a → f (g a)

-- _∘₂_ is the same as result2, but they have different intuition

_∘₂_ : {A B C D : Set} → (C → D) → (A → B → C) → (A → B → D)
(f ∘₂ g) a = f ∘ g a 

_∘₂'_ : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C}
        → ({a : A} → {b : B a} → Π (C a b) (D a b)) → (g : Π₂ A B C) → (a : A) → (b : B a) → D a b (g a b)
(f ∘₂' g) a = f ∘' g a

_∘₃_ : {A B C D E : Set} → (D → E) → (A → B → C → D) → (A → B → C → E)
(f ∘₃ g) a = f ∘₂ g a 

_∘₃'_ : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C} → {E : SetΠ₄ A B C D}
          → ({a : A} → {b : B a} → {c : C a b} → Π (D a b c) (E a b c)) → (g : Π₃ A B C D) → (a : A) → (b : B a) → (c : C a b) → E a b c (g a b c)
(f ∘₃' g) a = f ∘₂' g a

id : {A : Set} → A → A
id = λ a → a


flip : {A B C : Set} → (A → B → C) → B → A → C
flip f b a = f a b

flip' : {A B : Set} → {C : A → B → Set} → ((a : A) → (b : B) → C a b) → (b : B) → (a : A) → C a b
flip' f b a = f a b


-- apply : (A → B) → A → B

apply : {A : Set} → {B : SetΠ A} → Π A B → Π A B
apply = id

-- applyTo : A → (A → B) → B

applyTo : {A : Set} → {B : SetΠ A} → (a : A) → Π A B → B a 
applyTo = flip' apply


const : {A B : Set} → A → B → A
const a _ = a

const' : {A : Set} → {B : SetΠ A} → (a : A) → B a → A
const' a _ = a

const2 : {A B C : Set} → A → B → C → A
const2 a _ _ = a


_⋙_ : {A B C : Set} → (A → B) → (B → C) → (A → C)
_⋙_ = flip _∘_ 

_⋙'_ : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → (g : Π A B) → ({a : A} → Π (B a) (C a)) → (a : A) → C a (g a)
_⋙'_ = flip' _∘'_


explicit : {A : Set} → {B : SetΠ A} → ({a : A} → B a) → Π A B
explicit f a = f {a}

implicit : {A : Set} → {B : SetΠ A} → Π A B → ({a : A} → B a)
implicit f {a} = f a


-- Conal's Semantic Editor Combinators

result : {A B C : Set} → (B → C) → (A → B) → (A → C)
result = _∘_

result' : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → ({a : A} → Π (B a) (C a)) → (g : Π A B) → (a : A) → C a (g a)
result' = _∘'_

result2 : {A B C D : Set} → (C → D) → (A → B → C) → (A → B → D)
result2 = _∘₂_

result2' : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C}
          → ({a : A} → {b : B a} → Π (C a b) (D a b)) → (g : Π₂ A B C) → (a : A) → (b : B a) → D a b (g a b)
result2' = _∘₂'_

result3 : {A B C D E : Set} → (D → E) → (A → B → C → D) → (A → B → C → E)
result3 = _∘₃_

result3' : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C} → {E : SetΠ₄ A B C D}
          → ({a : A} → {b : B a} → {c : C a b} → Π (D a b c) (E a b c)) → (g : Π₃ A B C D) → (a : A) → (b : B a) → (c : C a b) → E a b c (g a b c)
result3' = _∘₃'_

argument : {A B C : Set} → (A → B) → (B → C) → (A → C)
argument = _⋙_

argument' : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → (g : Π A B) → ({a : A} → Π (B a) (C a)) → (a : A) → C a (g a)
argument' = _⋙'_

argResult : {A B C D : Set} → (A → B) → (C → D) → (B → C) → (A → D)
argResult fa fr f = fr ∘ f ∘ fa


-- SKI Calculus

-- K = const
-- I = id
-- S = (A → B → C) → (A → B) → A → C

S-comb : {A : Set} → {B C : SetΠ A} → ((a : A) → B a → C a) → Π A B → Π A C
S-comb f g a = f a (g a)

S-comb₂ : {A : Set} → {B : SetΠ A} → {C D : SetΠ₂ A B} → ((a : A) → (b : B a) → C a b → D a b) → Π₂ A B C → Π₂ A B D
S-comb₂ f g a b = f a b (g a b)


-- Equality Combinators --------------------------------------

_≢_ : {A : Set} → Rel A
a ≢ b = Not (a ≡ b)

subst : {A B : Set} → A ≡ B → A → B 
subst refl a = a

substR : {A B : Set} → A ≡ B → B → A 
substR refl b = b

---------------------------------------------

data Inspect {A : Set} (a : A) : Set where
  it : (b : A) → a ≡ b → Inspect a

inspect : {A : Set} → (a : A) → Inspect a
inspect a = it a refl


-- Product Combinators ----------------------

Σ₂ : (A : Set) → (B : SetΠ A) → SetΠ₂ A B → Set
Σ₂ A B C = Σ A (λ a → Σ (B a) (C a))

Σ₃ : (A : Set) → (B : SetΠ A) → (C : SetΠ₂ A B) → SetΠ₃ A B C → Set
Σ₃ A B C D = Σ A (λ a → Σ₂ (B a) (C a) (D a)) -- Σ A (λ a → Σ (B a) (λ b → Σ (C a b) (D a b)))

∃ : {A : Set} → (B : SetΠ A) → Set
∃ = Σ _

∃₂ : {A : Set} → {B : SetΠ A} → (SetΠ₂ A B) → Set
∃₂ C = ∃ (λ a → ∃ (C a))

fst : {A : Set} → {B : SetΠ A} → Σ A B → A
fst (a , b) = a

snd : {A : Set} → {B : SetΠ A} → (ab : Σ A B) → B (fst ab)
snd (a , b) = b

fstsnd : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → (abc : Σ₂ A B C) → B (fst abc)
fstsnd (_ , b , _) = b

thd : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → (abc : Σ₂ A B C) → C (fst abc) (fstsnd abc)
thd (_ , _ , c) = c

curry : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → ((ab : Σ A B) → C (fst ab) (snd ab)) → Π₂ A B C
curry f a b = f (a , b)

curry2 : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C}
         → ((abc : Σ₂ A B C) → D (fst abc) (fst (snd abc)) (snd (snd abc))) → Π₃ A B C D
curry2 f a b c = f (a , b , c)

curry3 : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C} → {E : SetΠ₄ A B C D}
         → ((abcd : Σ₃ A B C D) → E (fst abcd) (fstsnd abcd) (fst (thd abcd)) (snd (thd abcd))) → Π₄ A B C D E
curry3 f a b c d = f (a , b , c , d)

uncurry : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → Π₂ A B C → (ab : Σ A B) → C (fst ab) (snd ab)
uncurry f (a , b) = f a b

uncurry2 : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C}
           → Π₃ A B C D → (abc : Σ₂ A B C) → D (fst abc) (fst (snd abc)) (snd (snd abc)) 
uncurry2 f (a , b , c) = f a b c

uncurry3 : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C} → {E : SetΠ₄ A B C D}
           → Π₄ A B C D E → (abcd : Σ₃ A B C D) → E (fst abcd) (fstsnd abcd) (fst (thd abcd)) (snd (thd abcd))
uncurry3 f (a , b , c , d) = f a b c d

×-apply : {A B : Set} → (A → B) × A → B
×-apply (f , a) = f a

-- first : {A B C : Set} → (A → C) → A × B → C × B

first : {A B C : Set} → (A → C) → A × B → C × B
first f (a , b) = f a , b

-- second : (B → C) → A × B → A × C

second : {A B C : Set} → (B → C) → A × B → A × C
second f (a , b) = a , f b

second' : {A : Set} → {B C : SetΠ A} → ({a : A} → B a → C a) → Σ A B → Σ A C
second' f (a , b) = a , f b

-- third : (C → D) → A × B × C → A × B × D

third : {A : Set} → {B : SetΠ A} → {C D : SetΠ₂ A B} → ({a : A} → {b : B a} → C a b → D a b) → Σ₂ A B C → Σ₂ A B D
third f (a , b , c) = a , b , f c

-- fourth : (C → D) → A × B × C × D → A × B × C × E

fourth : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D E : SetΠ₃ A B C}
         → ({a : A} → {b : B a} → {c : C a b} → D a b c → E a b c) → Σ₃ A B C D → Σ₃ A B C E
fourth f (a , b , c , d) = a , b , c , f d

-- _&_ : (A → B) → (A → C) → A → B × C

-- _&_ : {A B : Set} → {C : B → Set} → (f : A → B) → ((a : A) → C (f a)) → A → Σ B C
-- (f & g) a = f a , g a 

_&_ : {A : Set} → {B C : SetΠ A} → (f : Π A B) → (g : Π A C) → (a : A) → B a × C a
(f & g) a = f a , g a 

-- _&₂_ : {A B C D : Set} → (A → B → C) → (A → B → D) → A → B → C × D
-- (f &₂ g) a b = f a b , g a b 

_&₂_ : {A : Set} → {B : SetΠ A} → {C D : SetΠ₂ A B} → (f : Π₂ A B C) → (g : Π₂ A B D) → (a : A) → (b : B a) → C a b × D a b
(f &₂ g) a = f a & g a

_&₃_ : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D E : SetΠ₃ A B C} → (f : Π₃ A B C D) → (g : Π₃ A B C E) → (a : A) → (b : B a) → (c : C a b) → D a b c × E a b c
(f &₃ g) a = f a &₂ g a 

_&₄_ : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C} → {E F : SetΠ₄ A B C D} → (f : Π₄ A B C D E) → (g : Π₄ A B C D F) → (a : A) → (b : B a) → (c : C a b) → (d : D a b c) → E a b c d × F a b c d
(f &₄ g) a = f a &₃ g a 

_&₅_ : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C} → {E : SetΠ₄ A B C D} → {F G : SetΠ₅ A B C D E} → (f : Π₅ A B C D E F) → (g : Π₅ A B C D E G) → (a : A) → (b : B a) → (c : C a b) → (d : D a b c) → (e : E a b c d) → F a b c d e × G a b c d e
(f &₅ g) a = f a &₄ g a 

_&₆_ : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C} → {E : SetΠ₄ A B C D} → {F : SetΠ₅ A B C D E} → {G H : SetΠ₆ A B C D E F} → (f : Π₆ A B C D E F G) → (g : Π₆ A B C D E F H) → (a : A) → (b : B a) → (c : C a b) → (d : D a b c) → (e : E a b c d) → (f : F a b c d e) → G a b c d e f × H a b c d e f
(f &₆ g) a = f a &₅ g a 

_&₇_ : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C} → {E : SetΠ₄ A B C D} → {F : SetΠ₅ A B C D E} → {G : SetΠ₆ A B C D E F} → {H I : SetΠ₇ A B C D E F G} → (f : Π₇ A B C D E F G H) → (g : Π₇ A B C D E F G I) → (a : A) → (b : B a) → (c : C a b) → (d : D a b c) → (e : E a b c d) → (f : F a b c d e) → (g : G a b c d e f) → H a b c d e f g × I a b c d e f g
(f &₇ g) a = f a &₆ g a 


-- _∥_ : (A → C) → (B → D) → A × B → C × D
_∥_ : {A C : Set} → {B : SetΠ A} → {D : C → Set} → (f : A → C) → ({a : A} → B a → D (f a)) → Σ A B → Σ C D
_∥_ f g (a , b) = f a , g b 

-- _∥₂_ : (A → B → C) → (J → K → L) → (A × J) × (B × K) → C × L
_∥₂_ : {A B C : Set} → {J : SetΠ A} → {K : SetΠ B} → {L : SetΠ C} →
           (f : A → B → C) → ({a : A} → {b : B} → J a → K b → L (f a b)) → Σ A J × Σ B K → Σ C L
(f ∥₂ g) ((a , j) , (b , k)) = f a b , g j k

-- cross : (A → C) × (B → D) → A × B → C × D
-- cross : {A C : Set} → {B : SetΠ A} → {D : C → Set} → Σ (A → C) (λ f → {a : A} → B a → D (f a)) → Σ A B → Σ C D
-- cross = uncurry _∥_

-- cross2 : (A → B → C) × (J → K → L) → (A × J) × (B × K) → C × L
-- cross2 : {A B C : Set} → {J : SetΠ A} → {K : SetΠ B} → {L : SetΠ C} →
--         Σ (A → B → C) (λ f → {a : A} → {b : B} → J a → K b → L (f a b)) → Σ A J × Σ B K → Σ C L
-- cross2 = uncurry _∥₂_

-- ×-map : (A → B) → (J → K) → (A × J) → (B × K)
×-map : {A B : Set} → {J : SetΠ A} → {K : SetΠ B} → (f : A → B) → ({a : A} → J a → K (f a)) → Σ A J → Σ B K
×-map = _∥_

-- ×-map2 : (A → B → C) → (J → K → L) → (A × J) → (B × K) → C × L
×-map2 : {A B C : Set} → {J : SetΠ A} → {K : SetΠ B} → {L : SetΠ C} →
           (f : A → B → C) → (g : {a : A} → {b : B} → J a → K b → L (f a b)) → Σ A J → Σ B K → Σ C L
×-map2 f g (a , j) (b , k) = f a b , g j k

-- ×₂-map : (A → B) → (J → K) → (X → Y) → (A × J × X) → (B × K × Y)
×₂-map : {A B : Set} → {J : SetΠ A} → {K : SetΠ B} → {X : SetΠ₂ A J} → {Y : SetΠ₂ B K} →
         (f : A → B) → (g : {a : A} → J a → K (f a)) → (h : {a : A} → {j : J a} → X a j → Y (f a) (g j)) → Σ₂ A J X → Σ₂ B K Y
×₂-map f g h = f ∥ g ∥ h

-- ×₂-map2 : (A → B → C) → (J → K → L) → (X → Y → Z) → (A × J × X) → (B × K × Y) → C × L × Z
×₂-map2 : {A B C : Set} → {J : SetΠ A} → {K : SetΠ B} → {L : SetΠ C} →
          {X : SetΠ₂ A J} → {Y : SetΠ₂ B K} → {Z : SetΠ₂ C L} →
          (f : A → B → C) → (g : {a : A} → {b : B} → J a → K b → L (f a b)) →
          (h : {a : A} → {b : B} → {j : J a} → {k : K b} → X a j → Y b k → Z (f a b) (g j k)) →
          Σ₂ A J X → Σ₂ B K Y → Σ₂ C L Z
×₂-map2 f g h = ×-map2 f (×-map2 g h)



fork : {A : Set} → A → A × A
fork a = a , a

dup : {A : Set} → A → A × A
dup = fork

swap : {A B : Set} → A × B → B × A
swap (a , b) = b , a

××-swap : {A B C D : Set} → (A × B) × (C × D) → (A × C) × (B × D)
××-swap = _,_ ∥₂ _,_


×-split : {A B C : Set} → (A → B × C) → (A → B) × (A → C)
×-split f = fst ∘ f , snd ∘ f

×-split2 : {A B C D : Set} → (A → B → C × D) → (A → B → C) × (A → B → D)
×-split2 f = fst ∘₂ f , snd ∘₂ f

×-split3 : {A B C D E : Set} → (A → B → C → D × E) → (A → B → C → D) × (A → B → C → E)
×-split3 f = fst ∘₃ f , snd ∘₃ f

×-split' : {A : Set} → {B C : SetΠ A} → ((a : A) → B a × C a) → Π A B × Π A C
×-split' f = fst ∘' f , snd ∘' f

×-split2' : {A : Set} → {B : SetΠ A} → {C D : SetΠ₂ A B} → ((a : A) → (b : B a) → C a b × D a b) → Π₂ A B C × Π₂ A B D
×-split2' f = fst ∘₂' f , snd ∘₂' f

×-split3' : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D E : SetΠ₃ A B C} → ((a : A) → (b : B a) → (c : C a b) → D a b c × E a b c) → Π₃ A B C D × Π₃ A B C E
×-split3' f = fst ∘₃' f , snd ∘₃' f


×-assocR : {A B C : Set} → (A × B) × C → A × B × C
×-assocR ((a , b) , c) = a , b , c

×-assocL : {A B C : Set} → A × B × C → (A × B) × C
×-assocL (a , b , c) = (a , b) , c

×-inj : {A B : Set} → Injective2 {A} {B} _,_
×-inj refl = refl , refl

×-inj2 : {A B C : Set} → {b₁ b₂ : B} → {c₁ c₂ : C} → (A → Eq (B × C) (b₁ , c₁) (b₂ , c₂)) → (A → b₁ ≡ b₂) × (A → c₁ ≡ c₂)
×-inj2 p = ×-split (result ×-inj p)

×-inj3 : {A B C D : Set} → {c₁ c₂ : C} → {d₁ d₂ : D} → (A → B → Eq (C × D) (c₁ , d₁) (c₂ , d₂)) → (A → B → c₁ ≡ c₂) × (A → B → d₁ ≡ d₂)
×-inj3 p = ×-split2 (result2 ×-inj p)

×-inj2' : {A : Set} → {B C : SetΠ A} → {f₁ f₂ : Π A B} → {g₁ g₂ : Π A C} → ((a : A) → Eq (B a × C a) (f₁ a , g₁ a) (f₂ a , g₂ a)) → ((a : A) → f₁ a ≡ f₂ a) × ((a : A) → g₁ a ≡ g₂ a)
×-inj2' p = ×-split' (result' ×-inj p)

×-inj3' : {A : Set} → {B : SetΠ A} → {C D : SetΠ₂ A B} → {f₁ f₂ : Π₂ A B C} → {g₁ g₂ : Π₂ A B D} → ((a : A) → (b : B a) → Eq (C a b × D a b) (f₁ a b , g₁ a b) (f₂ a b , g₂ a b)) → ((a : A) → (b : B a) → f₁ a b ≡ f₂ a b) × ((a : A) → (b : B a) → g₁ a b ≡ g₂ a b)
×-inj3' p = ×-split2' (result2' ×-inj p)


fstSnd-inj : {A B : Set} → {ab ab' : A × B} → fst ab ≡ fst ab' → snd ab ≡ snd ab' → ab ≡ ab'
fstSnd-inj {_} {_} {_ , _} {._ , ._} refl refl = refl

×-cong : {A B : Set} → Congruent2 {A} {B} _,_
×-cong = cong2 _,_

×-congL : {A B : Set} → Congruent2L {A} {B} _,_
×-congL = cong2L _,_

×-congR : {A B : Set} → Congruent2R {A} {B} _,_
×-congR = cong2R _,_

×-cong2 : {A : Set} → {B C : SetΠ A} → {f₁ f₂ : Π A B} → {g₁ g₂ : Π A C} 
          → ((a : A) → f₁ a ≡ f₂ a) 
          → ((a : A) → g₁ a ≡ g₂ a) 
          → (a : A) → Eq (B a × C a) (f₁ a , g₁ a) (f₂ a , g₂ a)
×-cong2 f g a = ×-cong (f a) (g a)

×-cong2L : {A : Set} → {B C : SetΠ A} → {f : Π A B} → {g₁ g₂ : Π A C}
          → ((a : A) → g₁ a ≡ g₂ a) 
          → (a : A) → Eq (B a × C a) (f a , g₁ a) (f a , g₂ a)
×-cong2L g a = ×-congL (g a)

×-cong2R : {A : Set} → {B C : SetΠ A} → {f₁ f₂ : Π A B} → {g : Π A C} 
          → ((a : A) → f₁ a ≡ f₂ a)
          → (a : A) → Eq (B a × C a) (f₁ a , g a) (f₂ a , g a)
×-cong2R f a = ×-congR (f a)

×-cong3 : {A : Set} → {B : SetΠ A} → {C D : SetΠ₂ A B} → {f₁ f₂ : Π₂ A B C} → {g₁ g₂ : Π₂ A B D} 
          → ((a : A) → (b : B a) → f₁ a b ≡ f₂ a b) 
          → ((a : A) → (b : B a) → g₁ a b ≡ g₂ a b) 
          → (a : A) → (b : B a) → Eq (C a b × D a b) (f₁ a b , g₁ a b) (f₂ a b , g₂ a b)
×-cong3 f g a = ×-cong2 (f a) (g a)

×-cong3L : {A : Set} → {B : SetΠ A} → {C D : SetΠ₂ A B} → {f : Π₂ A B C} → {g₁ g₂ : Π₂ A B D}
           → ((a : A) → (b : B a) → g₁ a b ≡ g₂ a b) 
           → (a : A) → (b : B a) → Eq (C a b × D a b) (f a b , g₁ a b) (f a b , g₂ a b)
×-cong3L g a = ×-cong2L (g a)

×-cong3R : {A : Set} → {B : SetΠ A} → {C D : SetΠ₂ A B} → {f₁ f₂ : Π₂ A B C} → {g : Π₂ A B D} 
           → ((a : A) → (b : B a) → f₁ a b ≡ f₂ a b)
           → (a : A) → (b : B a) → Eq (C a b × D a b) (f₁ a b , g a b) (f₂ a b , g a b)
×-cong3R f a = ×-cong2R (f a)

×-cong4 : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D E : SetΠ₃ A B C} → {f₁ f₂ : Π₃ A B C D} → {g₁ g₂ : Π₃ A B C E} 
          → ((a : A) → (b : B a) → (c : C a b) → f₁ a b c ≡ f₂ a b c) 
          → ((a : A) → (b : B a) → (c : C a b) → g₁ a b c ≡ g₂ a b c) 
          → (a : A) → (b : B a) → (c : C a b) → Eq (D a b c × E a b c) (f₁ a b c , g₁ a b c) (f₂ a b c , g₂ a b c)
×-cong4 f g a = ×-cong3 (f a) (g a)

×-cong4L : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D E : SetΠ₃ A B C} → {f : Π₃ A B C D} → {g₁ g₂ : Π₃ A B C E}
          → ((a : A) → (b : B a) → (c : C a b) → g₁ a b c ≡ g₂ a b c) 
          → (a : A) → (b : B a) → (c : C a b) → Eq (D a b c × E a b c) (f a b c , g₁ a b c) (f a b c , g₂ a b c)
×-cong4L g a = ×-cong3L (g a)

×-cong4R : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D E : SetΠ₃ A B C} → {f₁ f₂ : Π₃ A B C D} → {g : Π₃ A B C E} 
          → ((a : A) → (b : B a) → (c : C a b) → f₁ a b c ≡ f₂ a b c)
          → (a : A) → (b : B a) → (c : C a b) → Eq (D a b c × E a b c) (f₁ a b c , g a b c) (f₂ a b c , g a b c)
×-cong4R f a = ×-cong3R (f a)

×-cong5 : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C} → {E F : SetΠ₄ A B C D} → {f₁ f₂ : Π₄ A B C D E} → {g₁ g₂ : Π₄ A B C D F} 
          → ((a : A) → (b : B a) → (c : C a b) → (d : D a b c) → f₁ a b c d ≡ f₂ a b c d) 
          → ((a : A) → (b : B a) → (c : C a b) → (d : D a b c) → g₁ a b c d ≡ g₂ a b c d) 
          →  (a : A) → (b : B a) → (c : C a b) → (d : D a b c) → Eq (E a b c d × F a b c d) (f₁ a b c d , g₁ a b c d) (f₂ a b c d , g₂ a b c d)
×-cong5 f g a = ×-cong4 (f a) (g a)

×-cong5L : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C} → {E F : SetΠ₄ A B C D} → {f : Π₄ A B C D E} → {g₁ g₂ : Π₄ A B C D F}
          → ((a : A) → (b : B a) → (c : C a b) → (d : D a b c) → g₁ a b c d ≡ g₂ a b c d) 
          →  (a : A) → (b : B a) → (c : C a b) → (d : D a b c) → Eq (E a b c d × F a b c d) (f a b c d , g₁ a b c d) (f a b c d , g₂ a b c d)
×-cong5L g a = ×-cong4L (g a)

×-cong5R : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C} → {E F : SetΠ₄ A B C D} → {f₁ f₂ : Π₄ A B C D E} → {g : Π₄ A B C D F} 
          → ((a : A) → (b : B a) → (c : C a b) → (d : D a b c) → f₁ a b c d ≡ f₂ a b c d)
          →  (a : A) → (b : B a) → (c : C a b) → (d : D a b c) → Eq (E a b c d × F a b c d) (f₁ a b c d , g a b c d) (f₂ a b c d , g a b c d)
×-cong5R f a = ×-cong4R (f a)


×-cong6 : {A : Set} → {B : SetΠ A} → {C : SetΠ₂ A B} → {D : SetΠ₃ A B C} → {E : SetΠ₄ A B C D} → {F G : SetΠ₅ A B C D E} → {f₁ f₂ : Π₅ A B C D E F} → {g₁ g₂ : Π₅ A B C D E G} 
          → ((a : A) → (b : B a) → (c : C a b) → (d : D a b c) → (e : E a b c d) → f₁ a b c d e ≡ f₂ a b c d e) 
          → ((a : A) → (b : B a) → (c : C a b) → (d : D a b c) → (e : E a b c d) → g₁ a b c d e ≡ g₂ a b c d e) 
          →  (a : A) → (b : B a) → (c : C a b) → (d : D a b c) → (e : E a b c d) → Eq (F a b c d e × G a b c d e) (f₁ a b c d e , g₁ a b c d e) (f₂ a b c d e , g₂ a b c d e)
×-cong6 f g a = ×-cong5 (f a) (g a)


-- Sum Type Combinators

case : {A B C : Set} → (A → C) → (B → C) → A ⊎ B → C
case f g (inl a) = f a
case f g (inr b) = g b

map-⊎ : {A B C D : Set} → (A → C) → (B → D) → A ⊎ B → C ⊎ D
map-⊎ f g = case (inl ∘ f) (inr ∘ g)

left : {A B C : Set} → (A → C) → A ⊎ B → C ⊎ B
left f = case (inl ∘ f) inr

right : {A B C : Set} → (B → C) → A ⊎ B → A ⊎ C
right f = case inl (inr ∘ f)

exchange : {A B : Set} → A ⊎ B → B ⊎ A
exchange = case inr inl

inll : {A B C : Set} → A → (A ⊎ B) ⊎ C
inll = inl ∘ inl

inlr : {A B C : Set} → B → (A ⊎ B) ⊎ C
inlr = inl ∘ inr

inrl : {A B C : Set} → B → A ⊎ B ⊎ C
inrl = inr ∘ inl

inrr : {A B C : Set} → C → A ⊎ B ⊎ C
inrr = inr ∘ inr


Or : Set → Set → Set
Or A B = A × B ⊎ A ⊎ B

orcases : {A B C : Set} → (A → B → C) → (A → C) → (B → C) → Or A B → C
orcases fab fa fb = case (uncurry fab) (case fa fb)

trichcases : {A B C D : Set} → (A → Not B → Not C → D) → (Not A → B → Not C → D) → (Not A → Not B → C → D) → Trichotomy A B C → D
trichcases f _ _ (inl (a , b , c)) = f a b c
trichcases _ f _ (inr (inl (a , b , c))) = f a b c
trichcases _ _ f (inr (inr (a , b , c))) = f a b c


-- Isomorphism and Extensionality -----------------------------------------------

-- I could defined isomorphism in a more point free fashion (f ∘ f⁻¹ ≡ id)
-- Another alternative would be to have a and b become explicit arguments

_↔_ : Set → Set → Set
A ↔ B = (A → B) × (B → A)

_≅_ : Set → Set → Set
A ≅ B = Σ (A ↔ B) (uncurry (λ f f⁻¹ → ({b : B} → f (f⁻¹ b) ≡ b) × ({a : A} → f⁻¹ (f a) ≡ a)))

Extensionality : Set
Extensionality = {A : Set} → {B : SetΠ A} → {f g : Π A B} → ((a : A) → f a ≡ g a) → f ≡ g

function : {A : Set} → {B : SetΠ A} → {f g : Π A B} → {a : A} → f ≡ g → f a ≡ g a
function refl = refl

-- Uniqueness of Identity Proofs
uip : {A : Set} → {a b : A} → (p q : a ≡ b) → p ≡ q
uip refl refl = refl

-- Comparisons ---------------------------------------

data CompareEq {A : Set} : A → A → Set where
  refl : {a : A}           → CompareEq a a
  neq  : {a b : A} → a ≢ b → CompareEq a b

data SimpleCompare {A : Set} : A → A → Set where
  refl : {a : A}   → SimpleCompare a a
  neq  : {a b : A} → SimpleCompare a b

------------------------------------------------------------------------
