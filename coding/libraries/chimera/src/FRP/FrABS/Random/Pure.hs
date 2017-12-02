module FRP.FrABS.Random.Pure 
  (
    fisherYatesShuffle
  , randomBool
  , randomExp
  , randomShuffle
  ) where

import Control.Monad.Random
import qualified Data.Map as Map

import FRP.FrABS.Random.Monadic 

-------------------------------------------------------------------------------
-- PURE RANDOM
-------------------------------------------------------------------------------
randomBool :: RandomGen g => g -> Double -> (Bool, g)
randomBool g p = runRand (randomBoolM p) g

randomExp :: RandomGen g => g -> Double -> (Double, g)
randomExp g lambda = runRand (randomExpM lambda) g

randomShuffle :: RandomGen g => g -> [a] -> ([a], g)
randomShuffle = fisherYatesShuffle

-- Taken from https://wiki.haskell.org/Random_shuffle
-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
fisherYatesShuffle :: RandomGen g => g -> [a] -> ([a], g)
fisherYatesShuffle gen [] = ([], gen)
fisherYatesShuffle gen l = toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)

    fisherYatesStep :: RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
    fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
      where
        (j, gen') = randomR (0, i) gen