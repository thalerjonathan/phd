module JavaRandomGen where

import System.Random

-- need a deterministic RNG: starts with 1 and in each next-call returns next natural int up until maxBound :: Int
data DeterministicGen = DeterministicGen Int

instance RandomGen DeterministicGen where
  next = detNext
  genRange = detRange
  split = detSplit

detNext :: DeterministicGen -> (Int, DeterministicGen)
detNext (DeterministicGen n) = (n, DeterministicGen (n+1))

detRange :: DeterministicGen -> (Int, Int)
detRange g = (1, maxBound :: Int)

detSplit :: DeterministicGen -> (DeterministicGen, DeterministicGen)
detSplit g = (g, g)

mkDetGen :: DeterministicGen
mkDetGen = DeterministicGen 1

genDetRands :: (RandomGen g) => Int -> g -> (Int, Int) -> ([Int], g)
genDetRands 0 rng ip = ([], rng)
genDetRands n rng ip = (x : xs, rng'')
  where
    (xs, rng'') = genDetRands (n-1) rng' ip
    (x, rng') = testRand rng ip

testRand :: (RandomGen g) => g -> (Int, Int) -> (Int, g)
testRand rng ip = (ri, rng')
  where
    (ri, rng') = randomR ip rng