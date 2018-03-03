import Data.Vect

total
transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate _ []
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         zipWith (::) x xsTrans

total
addVec : Num a => (x : Vect m a) -> (y : Vect m a) -> Vect m a
addVec [] [] = []
addVec (x :: xs) (y :: ys) = (x + y) :: addVec xs ys

total
addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = addVec x y :: addMatrix xs ys

-- multMatrix [[1,2], [3,4], [5,6]] [[7,8,9,10], [11,12,13,14]]
total
multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [] [] = []
multMatrix xs ys = let ysTrans = transposeMat ys in
                   multMatrixAux xs ysTrans
  where
    total
    multVec : Num a => (x : Vect m a) -> (y : Vect m a) -> a
    multVec [] [] = 0
    multVec (x :: xs) (y :: ys) = (x * y) + multVec xs ys

    -- multRowWithTMat [1, 2] (transposeMat [[7,8,9,10], [11,12,13,14]])
    total
    multRowWithTMat : Num a =>
                      (x : Vect m a) ->
                      (ysTrans : Vect p (Vect m a)) -> 
                      Vect p a
    multRowWithTMat x [] = []
    multRowWithTMat x (y :: ys) = multVec x y :: multRowWithTMat x ys

    total
    multMatrixAux : Num a => 
                    (xs : Vect n (Vect m a)) -> 
                    (ysTrans : Vect p (Vect m a)) -> 
                    Vect n (Vect p a)
    multMatrixAux [] ys = []
    multMatrixAux (x :: xs) ys = multRowWithTMat x ys :: multMatrixAux xs ys

