-- Prime numbers example from chapter 15 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
