module Utils 
  ( replaceElem
  , randomElem
  , validateAmount
  ) where

import Control.Monad.Random

replaceElem :: Int -> a -> [a] -> [a]
replaceElem _ _ []   = []
replaceElem idx a as = as'
  where
    (front, back) = splitAt idx as
    as' = front ++ [a] ++ tail back

randomElem :: RandomGen g
           => [a]
           -> Rand g a
randomElem as = do
  let n = length as
  ridx <- getRandomR (0, n - 1)
  return $ as !! ridx

validateAmount :: Double -> Double
validateAmount amount
  | amount < 0 = error $ "Negative amount: " ++ show amount
  | otherwise  = if amount < 0.01 then 0.0 else amount