-- Solutions for the exercises of the SLIDES "Programming In Haskell" by Graham Hutton: CHAPTER 2 (First Steps)

-- (2) Fix the syntax errors in the program below, and test your solution using GHCi.
{--
erroneousFunc :: Int
erroneousFunc  = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]
--}

-- NOTE: seems to be working correct anyway?
correctedFunc :: Int
correctedFunc = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

-- (3) Show how the library function last that selects the last element of a list can be defined using the functions introduced in this lecture.

last' :: [a] -> a
last' xs = head $ reverse xs

-- (4) Can you think of another possible definition?
last'' xs = xs !! ((length xs) - 1)

-- (5) Similarly, show how the library function init that removes the last element from a list can be defined in two different ways.
init' :: [a] -> [a]
init' xs = take (length xs - 1) xs
init'' xs = reverse . tail $ reverse xs
