-- Solutions for the exercises of the SLIDES "Programming In Haskell" by Graham Hutton: CHAPTER 7 (Higher-Order Functions)

-- (1) What are higher-order functions that return functions as results better known as?
-- First Order Citizens?

-- (2) Express the comprehension [f x | x <- xs, p x] using the functions map and filter.
-- map f $ filter p xs
-- e.g.  map (*2) $ filter even [1..10]

-- (3) Redefine map f and filter p using foldr.
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs   -- NOTE: foldr takes as first argument a function, as second the initial starting value of the accumulator and as the third the list to iterate over. As a function a lambda-expression is used which has 2 input parameters: 1st is the current list-item, 2nd is the current accumulator. The return-value should be the new accumulator.

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x acc -> if p x then x : acc else acc ) [] xs -- NOTE: again start with empty list as accumulator and add only items which satisfy the predicate
