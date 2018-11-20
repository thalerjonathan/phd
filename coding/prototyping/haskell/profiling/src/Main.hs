{-# LANGUAGE Strict #-}

module Main where

import Data.Foldable

import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST

main :: IO ()
main = print $ runST testMArray

testMArray :: ST s [((Int, Int), Int)]
testMArray = do
  arr <- createMArray
  let i = 1000000 :: Int
  forM_ [1..i] (const $ mutateMArray arr)
  getAssocs arr

createMArray :: ST s (STArray s (Int, Int) Int)
createMArray = do
  let xDim   = 5 :: Int
      yDim   = 5 :: Int
      n      = xDim * yDim
      es     = replicate n (0 :: Int)
      coords = [(x, y) | x <- [0..xDim-1], y <- [0..yDim-1]]
      ec     = zip coords es
    
  arr <- newArray_ ((0, 0), (xDim - 1, yDim - 1)) :: ST s (STArray s (Int, Int) Int)
  forM_ ec (uncurry $ writeArray arr) 
  return arr

mutateMArray :: STArray s (Int, Int) Int -> ST s ()
mutateMArray arr = do
  as <- getAssocs arr
  mapM_ (\(coord, c) -> writeArray arr coord (seq (c+1) (c+1))) as

testIArray :: IO ()
testIArray = do
  let xDim   = 5 :: Int
      yDim   = 5 :: Int
      n      = xDim * yDim
      es     = replicate n (0 :: Int)
      coords = [(x, y) | x <- [0..xDim-1], y <- [0..yDim-1]]

      arr = array ((0, 0), (xDim - 1, yDim - 1)) (zip coords es)

  let arr' = foldr' updateElement arr coords
  
  print arr'

updateElement :: (Int, Int)
              -> Array (Int, Int) Int
              -> Array (Int, Int) Int
updateElement coord arr = arr'
  where
    e    = arr ! coord
    e'   = e + 1
    arr' = arr // [(coord, e')]