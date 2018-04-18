import Data.Vect

%default total

data EqNat : Nat -> Nat -> Type where
     SameNat : EqNat num num

smallProofEq : EqNat (2 + 2) 4
smallProofEq = SameNat


successorEq : (x : Nat) -> (y : Nat) -> EqNat x y -> EqNat (S x) (S y)
successorEq y y SameNat = SameNat


{- Built in equality type:

data (=) : a -> b -> Type where
     Refl : x = x

-}

smallProof : 2 + 2 = 4
smallProof = Refl



notTrue : 2 + 2 = 5 -> Void
notTrue Refl impossible



plusZleft : (n : Nat) -> n = 0 + n
plusZleft n = Refl


plusZright : (n : Nat) -> n = n + 0
plusZright Z = Refl
plusZright (S k) = rewrite plusZright k in Refl


plusSleft : (n : Nat) -> (m : Nat) -> plus (S n) m = S (plus n m)
plusSleft n m = Refl


plusSright : (n : Nat) -> (m : Nat) -> plus n (S m) = S (plus n m)
plusSright Z m = Refl
plusSright (S k) m = rewrite plusSright k m in Refl


plusComm : (m : Nat) -> (n : Nat) -> m + n = n + m
plusComm Z n = plusZright n
plusComm (S k) n = rewrite plusComm k n in rewrite plusSright n k in Refl


{- Equality in action! -}

fixit : Vect (plus len 1) a -> Vect (S len) a
fixit {len} xs = rewrite plusComm 1 len in xs

myReverse : Vect n a -> Vect n a
myReverse [] = []
myReverse (x :: xs) = fixit (myReverse xs ++ [x])



myAppend : Vect n a -> Vect m a -> Vect (m + n) a



checkEqNat : (n : Nat) -> (m : Nat) -> Maybe (n = m)



tryZip : Vect n a -> Vect m b -> Maybe (Vect n (a, b))
