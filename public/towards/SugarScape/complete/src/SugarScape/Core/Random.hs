{-# LANGUAGE Strict #-}
module SugarScape.Core.Random
  ( randomBoolM
  , randomExpM
  , randomElemM
  , randomElemsM

  , randomShuffle
  , randomShuffleM

  , avoidM

  , rngSplits
  ) where

import System.Random

import Control.Monad.Random
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

randomShuffleM :: MonadRandom m
               => [a] 
               -> m [a]
randomShuffleM = _fisherYatesShuffleM

randomShuffle :: RandomGen g => g -> [a] -> ([a], g)
randomShuffle = _fisherYatesShuffle

rngSplits :: RandomGen g => Int -> g -> ([g], g)
rngSplits n0 g0  = rngSplitsAux n0 g0 []
  where
    rngSplitsAux :: RandomGen g => Int -> g -> [g] -> ([g], g)
    rngSplitsAux 0 g acc = (acc, g)
    rngSplitsAux n g acc = rngSplitsAux (n - 1) g'' (g' : acc)
      where
        (g', g'') = split g

----------------------------------------------------------------------
{-}
naiveShufle :: RandomGen g => g -> [a] -> ([a], g)
naiveShufle g xs 
    | n < 2     = (xs, g)
    | otherwise = (xs!!i : xs', g'')
  where
    n          = length xs
    (i, g')    = randomR (0, n - 1) g
    (xs', g'') = naiveShufle g' (take i xs ++ drop (i+1) xs) -- NOTE: cant use strict!

naiveShufleM :: MonadRandom m
             => [a] 
             -> m [a]
naiveShufleM xs 
    | n < 2     = return xs
    | otherwise = do
      i <- getRandomR (0, n - 1) 
      xs' <- naiveShufleM (take i xs ++ drop (i+1) xs)
      return $ xs!!i : xs'
  where
    n = length xs
-}

-- Taken from https://wiki.haskell.org/Random_shuffle
-- | Randomly shuffle a list without the IO Monad
--   /O(N log N)/
_fisherYatesShuffle :: RandomGen g => g -> [a] -> ([a], g)
_fisherYatesShuffle gen0 [] = ([], gen0)
_fisherYatesShuffle gen0 l = toElems $ foldl fisherYatesStep (initial (head l) gen0) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen' = (Map.singleton 0 x, gen')

    fisherYatesStep :: RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
    fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
      where
        (j, gen') = randomR (0, i) gen

_fisherYatesShuffleM :: MonadRandom m
                    => [a] 
                    -> m [a]
_fisherYatesShuffleM [] = return []
_fisherYatesShuffleM l = do
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