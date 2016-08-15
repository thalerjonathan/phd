-- Solutions for the exercises of the SLIDES "Programming In Haskell" by Graham Hutton: CHAPTER 6 (Recursive Functions)

{--
(1)
Without looking at the standard prelude, define the following library functions using recursion:

Decide if all logical values in a list are true:
and :: [Bool] -> Bool

Concatenate a list of lists:
concat :: [[a]] -> [a]

Produce a list with n identical elements:
replicate :: Int -> a -> [a]

Select the nth element of a list:
(!!) :: [a] -> Int -> a

Decide if a value is an element of a list:
elem :: Eq a => a -> [a] -> Bool
--}

and' :: [Bool] -> Bool
and' [] = True
and' (True:bs) = and' bs
and' (False:bs) = False

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x

idx :: [a] -> Int -> a
idx (x:xs) 1 = x
idx (x:xs) i = idx xs (i-1) 

elem' :: (Eq a) => a -> [a] -> Bool
elem' e [] = False
--elem' e (x:xs) = if e == x then True else elem' e xs
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs

{-- (2)
Define a recursive function
merge :: (Ord a) => [a] -> [a] -> [a]

that merges two sorted lists of values to give a single sorted list.  For example:

> merge [2,5,6] [1,3,4]

[1,2,3,4,5,6]
--}

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

{-- (3)
Define a recursive function

msort :: (Ord a) => [a] -> [a]

that implements merge sort, which can be specified by the following two rules:

Lists of length <= 1 are already sorted;

Other lists can be sorted by sorting the two halves and merging the resulting lists. 
--}

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort (x:xs)
  | null xs = [x]
  | otherwise = merge (msort l) (msort r) where
      l = take n (x:xs)
      r = drop n (x:xs)
      n = floor( toRational(length (x:xs)) / 2)
