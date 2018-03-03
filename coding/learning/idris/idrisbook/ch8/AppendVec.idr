data Vect : Nat -> Type -> Type where
     Nil : Vect Z a
     (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

append_nil : Vect m e -> Vect (plus m 0) e
append_nil {m} xs = rewrite plusZeroRightNeutral m in xs

append_xs : Vect (S (m + k)) e -> Vect (plus m (S k)) e
append_xs {m} {k} xs = rewrite sym (plusSuccRightSucc m k) in xs

append : Vect n e -> Vect m e -> Vect (m + n) e
append [] ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)