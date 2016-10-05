module Main where
import System.Random
import System.IO.Unsafe
import Data.Ratio
import Data.Time.Clock.POSIX

xCells :: Int
xCells = 10

yCells :: Int
yCells = 10

cellProb :: Float
cellProb = 0.5

ones = 1 : ones
fib             = 1 : 1 : [ a+b | (a,b) <- zip fib (tail fib) ]

conway = initRandomField

initRandomField :: [Bool]
initRandomField = fst (randomBools (xCells * yCells) cellProb (mkStdGen getCurrentMillis))

randomBools :: (RandomGen g) => Int -> Float -> g -> ([Bool], g)
randomBools 0 p g = ([b], g)
  where rb = randomBool p g
        b = fst rb
        g' = snd rb
randomBools n p g = ([b] ++ recBools, recGen)
                     where rb = randomBool p g
                           b = fst rb
                           g' = snd rb
                           recCall = (randomBools (n-1) p g')
                           recBools = fst recCall
                           recGen = snd recCall

randomBool :: RandomGen g => Float -> g -> (Bool, g)
randomBool p g = (b, g')
  where r = randomR (0.0, 1.0) g
        value = fst r
        g' = snd r
        b = value > p

{-
randomBools' :: Int -> Float -> [Bool]
randomBools' n p = bs
  where g = mkStdGen 42
        rs = randomR (0.0, 1.0) g
        bs = map (\r -> True) [rs]
-}

diffSelect :: Int -> IO [Float]
diffSelect n = do
  gen <- getStdGen
  return . take n $ randomRs (0.0, 1.0) gen

{-
floatToBool :: IO [Float] -> Float -> IO [Bool]
floatToBool fs p =  map (\f -> True) 
-}

getCurrentMillis :: Int
getCurrentMillis = fromIntegral (unsafePerformIO timeInMillis) -- NOTE: will only result in an update after recompilation...

timeInMicros :: IO Integer
timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime

timeInMillis :: IO Integer
timeInMillis = (`div` 1000) <$> timeInMicros

timeInSeconds :: IO Integer
timeInSeconds = (`div` 1000) <$> timeInMillis

timeInSeconds' :: IO Double
timeInSeconds' = (/ 1000000) . fromIntegral <$> timeInMicros
