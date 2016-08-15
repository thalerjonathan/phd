-- Solutions for the exercises of the SLIDES "Programming In Haskell" by Graham Hutton: CHAPTER 5 (List Comprehensions)

{-- (1)
A triple (x,y,z) of positive integers is called pythagorean if x^2 + y^2 = z^2.  Using a list comprehension, define a function

pyths :: Int -> [(Int,Int,Int)]

that maps an integer n to all such triples with components in [1..n].  For example:

> pyths 5
[(3,4,5),(4,3,5)]
--}

pyths :: Int -> [(Int,Int,Int)]
pyths n = [ (x,y,n) | x <- [1..n], y <- [1..n], x^2 + y^2 == n^2 ]

allPyths :: Int -> [(Int,Int,Int)]
allPyths n = [ p | x <- [1..n], p <- pyths x]

{-- (2)
A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.  Using a list comprehension, define a function

perfects :: Int -> [Int]

that returns the list of all perfect numbers up to a given limit.  For example:

> perfects 500

[6,28,496]
--}

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum (init (factors x)) == x] -- NOTE: using init to remove last element of factors which is the value x itself because x is always a factor of x (x*1)

{-- (3)
The scalar product of two lists of integers xs and ys of length n is give by the sum of the products of the corresponding integers:

i = 0 to n-1: (xs_i * ys_i)

Using a list comprehension, define a function that returns the scalar product of two lists.
--}

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [ x*y | (x, y) <- zip xs ys ] -- NOTE: need to zip because need to iterate over both by pairs. if first iterating over x and then over y then we would use a nested loop which is not what we wanted
