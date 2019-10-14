module Random
  (
    randomBoolM
  , randomExpM
  , randomElemM
  , randomShuffleM
  , avoidM
  ) where

import            System.Random

import            Control.Monad.Random
import            Control.Monad.State.Strict
import qualified  Data.Map as Map

randomBoolM :: MonadRandom m => Double -> m Bool
randomBoolM p = getRandomR (0.0, 1.0) >>= (\r -> return $ p >= r)

-- NOTE: THIS CODE INSPIRED BY Euterpea-1.0.0 (I didn't want to create dependencies and their implementation seems neat and tidy)
randomExpM :: MonadRandom m => Double -> m Double
randomExpM lambda = avoidM 0 >>= (\r -> return $ (-log r) / lambda)

randomElemM :: MonadRandom m => [a] -> m a
randomElemM as = do
  let len = length as
  idx <- getRandomR (0, len - 1) 
  return (as !! idx)

-- NOTE: THIS CODE INSPIRED BY Euterpea-1.0.0 (I didn't want to create dependencies and their implementation seems neat and tidy)
avoidM :: (Random a, Eq a, MonadRandom m) => a -> m a
avoidM x = do
  r <- getRandom
  if r == x
    then avoidM x
    else return r

randomShuffleM :: (MonadState g m, RandomGen g, MonadRandom m) => [a] -> m [a]
randomShuffleM as = do
  g <- get
  let (as', g') = fisherYatesShuffle g as
  put g'
  return as'

-- Taken from https://wiki.haskell.org/Random_shuffle
-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
fisherYatesShuffle :: RandomGen g => g -> [a] -> ([a], g)
fisherYatesShuffle gen0 [] = ([], gen0)
fisherYatesShuffle gen0 l = toElems $ foldl fisherYatesStep (initial (head l) gen0) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen' = (Map.singleton 0 x, gen')

    fisherYatesStep :: RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
    fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
      where
        (j, gen') = randomR (0, i) gen

{-
randomShuffleM :: (RandomGen g) => [a] -> Rand g [a]
randomShuffleM _as = do
  g <- get
  let (as', g') = fisherYatesShuffle g as
  put g'
  return as'-}