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
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

zipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWith f [] [] = []
zipWith f (x :: xs) (y :: ys) = f x y :: zipWith f xs ys

insert :  Ord a
             => (x : a)
             -> (xs_sorted : Vect k a) 
             -> Vect (S k) a
insert x [] = [x]
insert x (y :: xs) 
  = case compare x y of
      LT => x :: y :: xs
      EQ => x :: y :: xs
      GT => y :: insert x xs

ins_sort : Ord a => Vect n a -> Vect n a
ins_sort []        = []
ins_sort (x :: xs) = insert x (ins_sort xs)

transpose_mat : Vect n (Vect m elem) -> Vect m (Vect n elem)















------- A main program to read dimensions, generate and tranpose a vector

Functor (Vect m) where
    map m [] = []
    map m (x :: xs) = m x :: map m xs

Show a => Show (Vect m a) where
    show x = show (toList x)
      where
        toList : Vect m a -> List a
        toList [] = []
        toList (y :: xs) = y :: toList xs

countTo : (m : Nat) -> Vect m Int
countTo Z = []
countTo (S k) = 0 :: map (+1) (countTo k)

mkVect : (n, m : Nat) -> Vect n (Vect m Int)
mkVect Z m = []
mkVect (S k) m = countTo m :: map (map (+ cast m)) (mkVect k m)

main : IO ()
main = do putStr "Rows: "
          let r : Nat = cast (cast {to=Int} !getLine)
          putStr "Columns: "
          let c : Nat = cast (cast {to=Int} !getLine)
          printLn (mkVect r c)
          putStrLn "Transposed:"
          printLn (transpose_mat (mkVect r c))
