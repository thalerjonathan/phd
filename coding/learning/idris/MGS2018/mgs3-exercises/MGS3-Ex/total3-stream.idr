{- Productivity: streams -}

{- Stream is defined in the Prelude:

data Stream : Type -> Type where
  (::) : (value : elem) -> Inf (Stream elem) -> Stream elem

-}

count : Nat -> Stream Nat

firstn : Nat -> Stream Nat -> List Nat

labelWith : Stream a -> List b -> List (a, b)

label : List a -> List (Nat, a)
