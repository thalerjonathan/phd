module SugarScape.Core.Random
  ( randomBoolM
  , randomExpM
  , randomElemM
  , randomElemsM
  , randomShuffleM
  , fisherYatesShuffle
  , fisherYatesShuffleM
  , avoidM

  , rngSplits
  ) where

import System.Random

import Control.Monad.Random
import Control.Monad.State.Strict
import qualified Data.Map as Map

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

randomElemsM :: MonadRandom m => Int -> [a] -> m [a]
randomElemsM n as = mapM (const $ randomElemM as) [1..n]

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

fisherYatesShuffleM :: MonadRandom m
                    => [a] 
                    -> m [a]
fisherYatesShuffleM [] = return []
fisherYatesShuffleM l = do
    lMap <- foldM fisherYatesStep (Map.singleton 0 (head l)) (numerate (tail l))
    return $ Map.elems lMap
  where
    numerate = zip [1..]

    fisherYatesStep :: MonadRandom m
                    => Map.Map Int a 
                    -> (Int, a) 
                    -> m (Map.Map Int a)
    fisherYatesStep m (i, x) = do
        j <- getRandomR (0, i)
        return ((Map.insert j x . Map.insert i (m Map.! j)) m)

rngSplits :: RandomGen g => g -> Int -> ([g], g)
rngSplits g0 n0 = rngSplitsAux g0 n0 []
  where
    rngSplitsAux :: RandomGen g => g -> Int -> [g] -> ([g], g)
    rngSplitsAux g 0 acc = (acc, g)
    rngSplitsAux g n acc = rngSplitsAux g'' (n - 1) (g' : acc)
      where
        (g', g'') = split g