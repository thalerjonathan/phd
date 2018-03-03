-- taken from ch4/Vect.idr
data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

-- including n forces the two vects to be of equal length
-- and rejects different sized vectors already at compile time
Eq e => Eq (Vect n e) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys

Foldable (Vect n) where
  foldr f acc []        = acc
  foldr f acc (x :: xs) = f x (foldr f acc xs)
