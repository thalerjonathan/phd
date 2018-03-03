data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

headUnequal : DecEq a 
            => {xs : Vect n a} -> {ys : Vect n a} 
            -> (contra : (x = y) -> Void) 
            -> ((x :: xs) = (y :: ys)) 
            -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a 
            => {xs : Vect n a} -> {ys : Vect n a} 
            -> (contra : (xs = ys) -> Void) 
            -> ((x :: xs) = (y :: ys)) 
            -> Void
tailUnequal contra Refl = contra Refl

DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) 
    = case decEq x y of
           No contra => No (headUnequal contra)
           Yes Refl  => case decEq xs ys of
                             Yes Refl  => Yes Refl
                             No contra => No (tailUnequal contra)
           
