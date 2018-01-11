import Data.Vect

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m     = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m =
  let indHyp = myPlusCommutes k m in
      rewrite myPlusCommutes k m in
      rewrite plusSuccRightSucc m k in
      Refl

reverseProof_nil : (acc : Vect n a) -> Vect (plus n 0) a
reverseProof_nil acc = ?reverseProof_nil_rhs

reverseProof_xs : Vect ((S n) + len) a -> Vect (plus n (S len)) a
reverseProof_xs xs = ?reverseProof_xs_rhs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where
    reverse' : Vect n a -> Vect m a -> Vect (n+m) a
    reverse' acc [] = reverseProof_nil acc
    reverse' acc (x :: xs) 
                    = reverseProof_xs (reverse' (x :: acc) xs)