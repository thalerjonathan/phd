import Data.List
import System.Random
import Control.Monad (replicateM)

selfLearning = do
  putStrLn "Login: what's your name?"
  name <- getLine
  putStrLn ("Welcome " ++ name ++ ", you are logged in!")

-- solutions to 99 haskell problems
-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

-- Problem 1: Find the last element of a list
myLast :: [a] -> a
myLast (a:[]) = a
myLast (_:as) = myLast as

-- Problem 2: Find the last but one element of a list
myButLast :: [a] -> a
myButLast (a1:a2:[]) = a1
myButLast (_:as) = myButLast as

-- Problem 3: Find the kth element of a list (starting with index 1)
elementAt :: [a] -> Int -> a
elementAt (a:_) 1 = a
elementAt (_:as) idx = elementAt as (idx - 1)

-- Problem 4: Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:as) = 1 + (myLength as)

-- Problem 5: Reverse a list
myReverse :: [a] -> [a]
myReverse (a:[]) = [a]
myReverse (a:as) = (myReverse as) ++ [a]

-- Problem 6: Find out whether a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (a:[]) = True
isPalindrome (a:as)
  | a == t = isPalindrome ts
  | otherwise = False
  where
    t = last as
    ts = init as

-- Problem 7: Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (a:as)) = (flatten a) ++ flatten (List as) 

-- Problem 8: Eliminate consecutive duplicates of list elements
compress :: Eq a => [a] -> [a]
compress (a:[]) = [a]
compress (a:as) 
  | nextHead == a = compress as
  | nextHead /= a = [a] ++ compress as
    where
      nextHead = head as
    
-- Problem 9: Pack consecutive duplicates of list elements into sublists
pack :: Eq a => [a] -> [[a]]
pack as = map (\gr -> gr) (group as)

-- Problem 10: Run-length encoding of Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode as = map (\gr -> (length gr, head gr)) (group as)

-- Problem 11: Modified run-length encoding
data EncodingData a = Single a | Multiple Int a deriving (Show)
encodeModified :: Eq a => [a] -> [EncodingData a]
encodeModified as = map (\gr -> if (length gr) == 1
                                    then (Single (head gr))
                                    else (Multiple (length gr) (head gr))) (group as)

-- Problem 12: Decode a run-length encoded list
decodeModified :: Eq a => [EncodingData a] -> [a]
decodeModified es = foldl decodeHelper [] es
  where
    decodeHelper state (Single a) = state ++ [a]
    decodeHelper state (Multiple n a) = state ++ replicate n a
    
-- Problem 13:
{- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X. -}
-- TODO

-- Problem 14: Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (a:as) = a:a:dupli as

-- Problem 15: Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli as n = concatMap repliHelper as
  where
    repliHelper a = repliInternal n a
      where
        repliInternal 1 a = [a]
        repliInternal n a = a:repliInternal (n-1) a

-- Problem 16: Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery as n = dropEveryHelper as n
  where
    dropEveryHelper [] _ = []
    dropEveryHelper (a:t) 1 = dropEveryHelper t n
    dropEveryHelper (a:t) x = [a] ++ dropEveryHelper t (x-1)
    
-- Problem 17: Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split xs n = (splitFront xs n, splitTail xs n)
  where
    splitFront (x:xs) k
      | k == 1 = [x]
      | otherwise = [x] ++ splitFront xs (k-1) 
    splitTail (x:xs) k
      | k == 1 = xs
      | otherwise = splitTail xs (k-1)


-- Problem 18:  Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice as i j = sliceHelper 1 as
  where
    sliceHelper n (x:xs)
      | n >= length as = []
      | n >= i && n <= j = [x] ++ (sliceHelper (n+1) xs)
      | otherwise = sliceHelper (n+1) xs

-- Problem 19: Rotate a list N places to the left.
-- TODO

-- Problem 20: Remove the K'th element from a list.
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt 1 (x:xs) = (Just x, xs)
removeAt n (x:xs) = let (a,r) = removeAt (n-1) xs in (a, x:r)

-- Problem 21: Insert an element at a given position into a list
-- TODO
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = insertAtHelper 1 xs
  where
    insertAtHelper idx (x':xs')
      | idx == n = x:x':xs'
      | otherwise = [x'] ++ (insertAtHelper (idx + 1) xs')


-- Problem 22: Create a list containing all integers within a given range.
range :: Integer -> Integer -> [Integer]
range i j = rangeHelper i j []
  where
    rangeHelper curr max as
      | curr == max = as ++ [curr]
      | curr > max = []
      | otherwise = rangeHelper (curr + 1) max ( as ++ [curr])

-- Problem 23: Extract a given number of randomly selected elements from a list.
-- TODO
-- rnd_select :: [a] -> Integer -> [a]

-- Problem 31: Determine whether a given integer number is prime.
isPrime :: Integer -> Bool
isPrime x = isPrimeHelper 2
  where
    isPrimeHelper divider
      | divider == x = True
      | (mod x divider) == 0 = False
      | otherwise = isPrimeHelper (divider + 1)
