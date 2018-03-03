import Data.Vect

myReverseWithProof : Vect n a -> Vect n a 
myReverseWithProof [] = []
myReverseWithProof {n = S k} (x :: xs) 
  = let result = myReverseWithProof xs ++ [x] in
        rewrite plusCommutative 1 k in result

myReverse : Vect n a -> Vect n a 
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
  where
    reverseProof : Vect (k + 1) a -> Vect (S k) a
    reverseProof {k} result = rewrite plusCommutative 1 k in result