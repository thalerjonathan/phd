module Coinduction where

-- you can think of:
--  ♯ as delay  (\sharp)
--  ♭ as force  (\flat)

---------------------------------------------

-- New definition, to add when upgrade to latest Agda

-- infix 1000 ♯_

-- postulate
--   ∞  : (A : Set) → Set
--   ♯_ : {A : Set} → A → ∞ A
--   ♭  : {A : Set} → ∞ A → A

-- {-# BUILTIN INFINITY ∞  #-}
-- {-# BUILTIN SHARP    ♯_ #-}
-- {-# BUILTIN FLAT     ♭  #-}

---------------------------------------------


infix 10 ♯_

codata ∞ (A : Set) : Set where
  ♯_ : A → ∞ A

♭ : {A : Set} → ∞ A → A
♭ (♯ a) = a
