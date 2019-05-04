module Utils 
  ( replaceElem
  , updateElem
  , randomElem
  , randomShuffleM
  , validateAmount
  ) where

import Control.Monad.Random
import qualified Data.Map as Map

replaceElem :: Int -> a -> [a] -> [a]
replaceElem _ _ []   = []
replaceElem idx a as = as'
  where
    (front, back) = splitAt idx as
    as' = front ++ [a] ++ tail back

updateElem :: Int -> (a -> a) -> [a] -> [a]
updateElem _ _ [] = []
updateElem idx f as = replaceElem idx a' as
  where
    a  = as !! idx
    a' = f a



validateAmount :: Double -> Double
validateAmount amount
  | amount < 0 = error $ "Negative amount: " ++ show amount
  | otherwise  = if amount < 0.01 then 0.0 else amount

randomElem :: MonadRandom m
           => [a]
           -> m a
randomElem as = do
  let n = length as
  ridx <- getRandomR (0, n - 1)
  return $ as !! ridx

randomShuffleM :: MonadRandom m
               => [a] 
               -> m [a]
randomShuffleM = _fisherYatesShuffleM

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