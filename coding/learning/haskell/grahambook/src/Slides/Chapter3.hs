-- Solutions for the exercises of the SLIDES "Programming In Haskell" by Graham Hutton: CHAPTER 6 (Types and classes)

-- (1) What are the types of the following values?
{--
[’a’,’b’,’c’]
Solution: [Char]
In Words: A List of Characters

(’a’,’b’,’c’)
Solution: (Char, Char, Char)
In Wrods: A Triple of Characters

[(False,’0’),(True,’1’)]
Solution: [(Bool, Char)]
In Words: A List of tuples where in each tuple the first part is Bool and the second is Char

([False,True],[’0’,’1’])
Solution: ([Bool], [Char])
In Words: A tuple with the first part is a list of booleans and the second part is a list of chars

[tail,init,reverse]
Solution: [([a] -> [a])]
In Words: Its a list of function which take a list and return a list
--}

-- (2) What are the types of the following functions?
{--
second xs     = head (tail xs)
Solution: second :: [a] -> a

swap (x,y)    = (y,x)
Solution: swap :: (a,b) -> (b,a)

pair x y      = (x,y)
Solution: pair :: a -> b -> (a,b) 

double x      = x*2
Solution: double :: a -> a

palindrome xs = reverse xs == xs
Solution: palindrome :: (Eq a) => [a] -> Bool

twice f x     = f (f x) 
Solution: twice :: (a -> a) -> a -> a
--}
