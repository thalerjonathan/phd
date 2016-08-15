-- http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html

import System.Random

dbgFSquare :: Float -> (Float,String)
dbgFSquare x = (x*x, "called dbgFSquare ")

dbgFDouble :: Float -> (Float,String)
dbgFDouble x = (2*x, "called dbgFDouble ")

bind :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
bind f (v,s) = (v2, s++s2) where
  (v2, s2) = (f v)

unit :: Float -> (Float,String)
unit x = (x, "")

-- NOTE: fake-implementations, just for sake of exercise
sqrt',cbrt' :: Float -> [Float]
sqrt' x = x':x':[] where
  x' = (sqrt x)
cbrt' x = 0:(sqrt' x)

bind' :: (Float -> [Float]) -> ([Float] -> [Float])
bind' f = (\xs -> [ y | x <- xs, y <- f x ])
bind'' f x = concat (map f x)

test :: Float -> [Float]
test x = (bind' sqrt' . sqrt')  x 

unit' :: Float -> [Float]
unit' x = [x]

bindRand :: (a -> StdGen -> (b,StdGen)) -> (StdGen -> (a,StdGen)) -> (StdGen -> (b,StdGen))
-- NOTE: I was confused by the complex function-signature and too focused on concrete random-implementations (see below) and coulnd't find the simple solution. This is the solution from the tutorial explained by me
{--
The function takes 2 functions as arguments and returns a function. The first argument (function) takes a type and a seed and returns a tuple of type b with an (updated) seed. The second argument (function) takes just a seed and returns a tuple of type a with an (updated) seed. The output is a function itself which takes a seed and returns a tuple of type b with (an updated) seed.

Although we have only 2 inputs we can actually pass 3 where the 3rd is then the input to the function which is returned from bindRand - note that the 3rd argument then has to be of type StdGen. Thus we implement bindRand with 3 arguments: f, g, seed where f and g are functions with the given signatures and seed of type StdGen. This is very important otherwise we cannot start any computation yet using these functions.

What bindRand should do is the following: invoke the function g (2nd parameter!) using the seed thus returning a tuple of type (a, StdGen). fst of tuple of type a has then to go into function f together with the updated seed'. This will produce a new tuple (b, StdGen) which is then returned.
--}
bindRand f g seed = let (x',seed') =     -- pattern-match on tuple to extract (a, StdGen)
                          g seed         -- computation starts here by function application returning (a, StdGen)
                    in f x' seed'        -- invoke first argument function using the previously extracted value x' and changed seed'

unitRand :: a -> (StdGen -> (a,StdGen))
unitRand x = (\seed -> (x, seed))
unitRand' x seed = (x, seed)
  
initChain :: (RandomGen g) => g -> (Int, g)
initChain g = next g

nextChain :: (RandomGen g) => (Int, g) -> (Int, g)
nextChain (v, g) = next g

getPureStdGen :: StdGen
getPureStdGen = mkStdGen 42
