data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Neg num, Integral num, Abs num) => Expr num -> num
eval (Val x)   = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x)   = abs (eval x)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-)      = Sub

Abs ty => Abs (Expr ty) where
  abs = Abs

Show ty => Show (Expr ty) where
  show (Val x)   = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Abs x)   = "|" ++ show x ++ "|"

(Eq ty, Neg ty, Integral ty, Abs ty) => Eq (Expr ty) where
  (==) x y = let valx = eval x 
                 valy = eval y in
                 valx == valy

(Neg ty, Integral ty, Abs ty) => Cast (Expr ty) ty where
  cast orig = eval orig

testCast : Integer 
testCast = let x : Expr Integer = 6 * 3 + 12 in 
           the Integer (cast x)