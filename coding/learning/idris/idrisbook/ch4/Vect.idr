data Vect : Nat -> Type -> Type where
     Nil : Vect Z a
     (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

total
append : Vect n e -> Vect m e -> Vect (n + m) e
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

total
zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] ys = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys