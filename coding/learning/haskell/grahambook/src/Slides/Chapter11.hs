-- Solutions for the exercises of the SLIDES "Programming In Haskell" by Graham Hutton: CHAPTER 11 (Lazy Evaluation)

{- (1) Define a program fibs :: [Integer] that generates the infinite Fibonacci sequence using the following simple procedure:

a) the first two numbers are 0 and 1
b) the next is the sum of the previous two
c) return to step b

Hint: make use of the library functions zip and tail. Note that numbers in the Fibonacci sequence quickly become large, hence the use of type Integer of arbitrary-precision integers above.
-}

fibs :: [Integer]
fibs = [0,1] ++ [ x+y | (x,y) <- zip fibs (tail fibs) ]
           
-- (2) Define a function fib :: Int -> Integer that calculates the nth Fibonacci number
fib :: Int -> Integer
fib n = last $ take n fibs
