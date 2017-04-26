open import Coinduction

module CoNat where

data Coℕ : Set where
  Z : Coℕ
  S : ∞ Coℕ → Coℕ

_+_ : Coℕ → Coℕ → Coℕ
Z   + n = n
S m + n = S (♯ (♭ m + n))

inf : Coℕ
inf = S (♯ inf)

data _≈_ : Coℕ → Coℕ → Set where
  refl-Z : Z ≈ Z
  refl-S : {m n : ∞ Coℕ} → ∞ (♭ m ≈ ♭ n) → (S m) ≈ (S n)

-- It is recommended not to use coinductive constructors in a constructor's index expressions
-- So use the previous definition, not the following one:

-- data _≈_ : Coℕ → Coℕ → Set where
--   refl-Z : Z ≈ Z
--   refl-S : {m n : Coℕ} → ∞ (m ≈ n) → (S (♯ m)) ≈ (S (♯ n))
