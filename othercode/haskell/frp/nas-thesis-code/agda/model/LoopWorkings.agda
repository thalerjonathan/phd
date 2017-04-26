{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import RealTime
open import FixedPoint

module LoopWorkings where

import TemporalFunction
open TemporalFunction Time _<ℜ₀_ <ℜ₀-irreflexive <ℜ₀-trans

-------------------------------------

Signal : Set → Set
Signal A = TVal A

SF : Set → Set → Set
SF A B = TFun A B

-------------------------------------

unfold' : {A : Set} → (f : SF A A) → Always (fix f ≐ f (fix f))
unfold' f _ = function (unfold f)

lem-unfold-eq : {A : Set} → {f g : SF A A} → Always (fix f ≐ fix g ⇒ f (fix f) ≐ g (fix g))
lem-unfold-eq t eq = trans2 (sym (unfold' _ _)) eq (unfold' _ _)

-- postulate lem-optimistic : {A : Set} → {f g : SF A A} → (fix f ≡ fix g → f (fix f) ≡ g (fix g)) → fix f ≡ fix g

-- lem-optimistic' : {A : Set} → {f g : SF A A} → Always ((fix f ≐ fix g ⇒ f (fix f) ≐ g (fix g)) ⇒ fix f ≐ fix g)
-- lem-optimistic' t p = {!!}

-- lem-oops : {A : Set} → (f g : SF A A) → Always (fix f ≐ fix g)
-- lem-oops f g t = lem-optimistic' t (λ eq → trans2 (sym (unfold' f t)) eq (unfold' g t))

-- lem-optimistic'' : {A : Set} → {f g : SF A A} → Always (Hʳ (fix f ≐ fix g ⇒ f (fix f) ≐ g (fix g)) ⇒ (fix f ≐ fix g))
-- lem-optimistic'' t Heq = lem-optimistic' t (fst Heq)

-- lem-optimistic''' : {A : Set} → {f g : SF A A} → Always ((Hʳ (fix f ≐ fix g) ⇒ f (fix f) ≐ g (fix g)) ⇒ (fix f ≐ fix g))
-- lem-optimistic''' t f = lem-optimistic'' t (lem-unfold-eq t , λ t' _ → lem-unfold-eq t')

----------------------------------------------------------------------------------------------------

Causal : {A B : Set} → SF A B → Set
Causal = NonExpansive

Decoupled : {A B : Set} → SF A B → Set
Decoupled = Contractive

loop₁ : {A B : Set} → SF (A × B) B → SF A B
loop₁ sf a = fix (λ b → sf (a & b))

loop₂ : {A B C : Set} → SF (A × C) B → SF B C → SF A B
loop₂ sff sfb a = fix (λ b → sff (a & sfb b))

lem-2 : {A : Set} → {t : Time} → (f g : SF A A) → Decoupled f → Decoupled g → ((s : Signal A) → f s t ≡ g s t) → fix f t ≡ fix g t
lem-2 {_} {t} f g = {!!}

lem-1 : {A : Set} → {t : Time} → (f g : SF A A) → Decoupled f → Decoupled g → ((s : Signal A) → Hʳ (f s ≐ g s) t) → (fix f ≐ fix g) t
lem-1 f g decf decg h = lem-2 f g decf decg (result' fst h)

lem-cau₁ : {A B : Set} → (sf : SF (A × B) B) → Decoupled sf → Causal (loop₁ sf)
lem-cau₁ {A} {B} sf dec a₁ a₂ t Heq with lem-cont→ne dec
... | cau = lem-1 (λ b → sf (a₁ & b))
            (λ b → sf (a₂ & b))
            (lem-con-×-congL dec a₁)
            (lem-con-×-congL dec a₂)
            (λ a → lem-ne-×-congR-Hʳ cau a₁ a₂ a t Heq)

lem-cau₂ : {A B C : Set} → (sff : SF (A × C) B) → (sfb : SF B C) → Causal sff → Decoupled sfb → Causal (loop₂ sff sfb)
lem-cau₂ sff sfb cauf dec a₁ a₂ t Heq with lem-cont→ne dec
... | caub = lem-1 (λ b → sff (a₁ & sfb b))
             (λ b → sff (a₂ & sfb b))
             (lem-ne-con-×-congL-congR cauf dec a₁)
             (lem-ne-con-×-congL-congR cauf dec a₂)
             (λ a → lem-ne-×-congR-Hʳ cauf a₁ a₂ (sfb a) t Heq)
