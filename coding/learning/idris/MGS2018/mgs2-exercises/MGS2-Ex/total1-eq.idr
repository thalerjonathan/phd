import Data.Vect

%default total

{- Built in equality type:

data (=) : a -> b -> Type where
     Refl : x = x

-}

successorEq : (x : Nat) -> (y : Nat) -> x = y -> S x = S y


smallProof : 2 + 2 = 4



notTrue : 2 + 2 = 5 -> Void



plusZleft : (n : Nat) -> n = 0 + n
plusZleft Z = Refl
plusZleft (S k) = Refl

plusZright : (n : Nat) -> n = n + 0
plusZright Z = Refl
plusZright (S k) = rewrite plusZright k in
                           Refl

plusSleft : (n : Nat) -> (m : Nat) -> plus (S n) m = S (plus n m)
plusSleft Z m = Refl
plusSleft (S k) m = Refl

plusSright : (n : Nat) -> (m : Nat) -> plus n (S m) = S (plus n m)
plusSright Z m = Refl
plusSright (S k) m = rewrite plusSright k m in
                             Refl

plusComm : (m : Nat) -> (n : Nat) -> m + n = n + m
plusComm Z n = plusZright n
plusComm (S k) n = rewrite plusComm k n in ?plusComm_rhs
--rewrite plusComm k n in
--                   rewrite plusSright n k in ?bla

plusAssoc : (x : Nat) -> (y : Nat) -> (z : Nat) -> x + (y + z) = (x + y) + z
plusAssoc Z y z = Refl
plusAssoc (S k) y z = rewrite plusAssoc k y z in ?plusAssoc_rhs_2
  -- rewrite plusAssoc k y z in Refl

{- Equality in action! -}

-- A better way to reverse a list than that shown in the lecture, but there
-- are gaps: fill them in
myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where
    reverse' : Vect n a -> Vect m a -> Vect (n + m) a
    reverse' acc [] = ?reversePrfNil acc
    reverse' acc (x :: ys) = ?reversePrfCons (reverse' (x :: acc) ys)


myAppend : Vect n a -> Vect m a -> Vect (m + n) a



checkEqNat : (n : Nat) -> (m : Nat) -> Maybe (n = m)



