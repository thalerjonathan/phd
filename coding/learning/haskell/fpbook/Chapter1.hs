main :: IO ()
main = 
    do
        print $ multInt 10 10

        print $ exponentInt 2 16
        print $ exponentFract 2 (-16)

        print $ addition 10000 10000

        print $ addition' (-1) (2)

        print $ division 9 3


multInt :: Int -> Int -> Int
multInt _ 0 = 0
multInt n m = n + multInt n (m-1)

-- Exercise 1.6 define recursively, in terms of multiplication, exponentiation to a non-negative integer power
exponentInt :: Int -> Int -> Int
exponentInt _ 0 = 1
exponentInt n m = multInt n (exponentInt n (m-1))

-- Exercise 1.7 define exponentiation (of nonnegative numbers) to an arbitraty integer power
exponentFract :: Int -> Int -> Double
exponentFract n m 
    | m < 0 = 1 / fromIntegral (exponentInt n (-m))
    | otherwise = fromIntegral $ exponentInt n m

-- Exercise 1.8 define recursively, in terms of the successor and predecessor functions, addition to a nonnegative integer
addition :: Int -> Int -> Int
addition n 0 = n
addition n m = addition (succ n) (pred m)

-- Exercise 1.9 define addition for abitrary integers
addition' :: Int -> Int -> Int
addition' n m 
    | m > 0 = addition n m
    | m < 0 = addition' (pred n) (succ m)
    | m == 0 = n

-- Exercise 1.10 define recursively, in terms of subtraction and less-than, division by a positive integer
division :: Int -> Int -> Int
division n m 
    | n < m = 0
    | n >= m = 1 + (division (n-m) m)