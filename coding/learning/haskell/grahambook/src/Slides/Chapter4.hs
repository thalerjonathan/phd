-- Solutions for the exercises of the SLIDES "Programming In Haskell" by Graham Hutton: CHAPTER 4 (Defining Functions)

{-- (1) Consider a function safetail that behaves in the same way as tail, except that safetail maps the empty list to the empty list, whereas tail gives an error in this case.  Define safetail using:

  (a)	a conditional expression;
  (b)	guarded equations;
  (c)	pattern matching.

Hint: the library function null :: [a] -> Bool can be used to test if a list is empty.
--}

-- the type of the function
safetail :: [a] -> [a]

-- (a) conditional expression
safetail xs = if null xs then [] else tail xs

-- (b) guarded equations
safetail' xs
  | null xs = []
  | otherwise = tail xs

-- (c) using pattern matching
safetail'' [] = []
safetail'' (_:xs) = xs


-- (2) Give three possible definitions for the logical or operator (||) using pattern matching.
or' :: Bool -> Bool -> Bool
or' _ True = True
or' True _ = True
or' False False = False

-- (3) Redefine the following version of (&&) using conditionals rather than patterns:
-- True && True = True
-- _    && _    = False
and' :: Bool -> Bool -> Bool
and' x y = if x == True then (if y == True then True else False) else False 

-- (4) Do the same for the following version:
-- True  && b = b
-- False && _ = False
and'' :: Bool -> Bool -> Bool
and'' x y = if x == True then y else False
