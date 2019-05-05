module Utils 
  ( replaceElem
  , updateElem
  , findElem

  , randomElemM
  , randomShuffleM
  , randomPermM
  
  , validateAmount
  ) where

import Control.Monad.Random
import Data.List
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

findElem :: Eq a => a -> [[a]] -> Maybe (Int, Int)
findElem = findElemAux 0
  where
    findElemAux :: Eq a => Int -> a -> [[a]] -> Maybe (Int, Int)
    findElemAux _ _ [] = Nothing
    findElemAux n a (as:ass) =
        case idxMay of
          Nothing -> findElemAux (n+1) a ass
          (Just idx) -> Just (n, idx)
      where
        idxMay = elemIndex a as 

validateAmount :: Double -> Double
validateAmount amount
  | amount < 0 = error $ "Negative amount: " ++ show amount
  | otherwise  = if amount < 0.01 then 0.0 else amount

randomElemM :: MonadRandom m
            => [a]
            -> m a
randomElemM as = do
  let n = length as
  ridx <- getRandomR (0, n - 1)
  return $ as !! ridx

-- Creates a random permutation of indices 0, ..., size - 1
randomPermM :: MonadRandom m
            => Int
            -> m [Int]
randomPermM n = randomShuffleM [0..n-1]

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