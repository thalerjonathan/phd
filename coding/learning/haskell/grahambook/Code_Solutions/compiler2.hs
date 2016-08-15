-- Compiler example from chapter 17 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y

type Stack = [Int]

data Code = HALT | PUSH Int Code | ADD Code
            deriving Show

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n)   c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

exec :: Code -> Stack -> Stack
exec HALT       s            =  s
exec (PUSH n c) s            =  exec c (n : s)
exec (ADD c)    (m : n : s)  =  exec c (n+m : s)


