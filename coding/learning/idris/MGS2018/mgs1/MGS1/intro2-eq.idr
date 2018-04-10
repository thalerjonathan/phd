
data EqNat : Nat -> Nat -> Type where
     SameNat : (num : Nat) -> EqNat num num

smallProofEq : EqNat (2 + 2) 4



successorEq : EqNat x y -> EqNat (S x) (S y)

{- Built in equality type:

data (=) : a -> b -> Type where
     Refl : x = x

-}

smallProof : 2 + 2 = 4


notTrue : 2 + 2 = 5


{- Equality in action! -}

{-
data Nat = Z | S Nat

(+) : Nat -> Nat -> Nat
Z     + y = y
(S x) + y = S (x + y)
-}

data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a

%name Vect xs, ys, zs


append : Vect n a -> Vect m a -> Vect (n + m) a






zip : Vect n a -> Vect n b -> Vect n (a, b)





tryZip : Vect n a -> Vect m b -> Maybe (Vect n (a, b))
