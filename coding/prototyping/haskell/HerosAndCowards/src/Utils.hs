module Utils (
    drawRandomIgnoring
  ) where

import System.Random
import Data.List

import Debug.Trace

{- NOTE: for testing-purposes
main :: IO ()
main = do
    let g = mkStdGen 42
    drawRandomTest 10 g

drawRandomTest :: (RandomGen g) => Int -> g -> IO ()
drawRandomTest steps g = do
    let (x, g') = Utils.drawRandomIgnoring g [0..10] [0,2,4,6,8,10]
    putStrLn (show x)
    if steps == 0 then
        putStrLn "finished"
            else
                drawRandomTest (steps-1) g'
-}

-- NOTE: when passing in the RandomGen then split it before to obtain a unique RandomGen, to achieve unique shufflings,
--       because changed RandomGen will not be returned.
--       Returns undefined when no solution can be found
drawRandomIgnoring :: (RandomGen g, Eq a) => g -> [a] -> [a] -> (a, g)
drawRandomIgnoring g xs is
    | null validXs = undefined
    | otherwise = (firstValidX, g')
    where
        (xs', g') = shuffle xs g
        validXs = filter (not . (\x -> any (==x) is)) xs'
        firstValidX = head validXs

-- NOTE: using simple version of shuffle-algorithm because random-shuffle lib will resist compiling for profiling
shuffle :: (RandomGen g) => [a] -> g -> ([a], g)
shuffle xs g = if length xs < 2 then
                (xs, g)
                    else
                        ((xs!!i : r), g'')
    where
        (i, g') = randomR(0, length xs - 1) g
        (r, g'') = shuffle (take i xs ++ drop (i+1) xs) g'

-- NOTE: this solution will recur forever if there are no possible solutions
{-
drawRandomIgnoring' :: (RandomGen g) => g -> Int -> Int -> [Int] -> (Int, g)
drawRandomIgnoring' g lowerRange upperRange ignore
    | any (==randInt) ignore = drawRandomIgnoring g' lowerRange upperRange ignore
    | otherwise = (randInt, g')
        where
            (randInt, g') = randomR(lowerRange, upperRange - 1) g
-}