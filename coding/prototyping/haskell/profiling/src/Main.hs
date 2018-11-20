{-# LANGUAGE Strict #-}
{-# LANGUAGE Arrows #-}
module Main where

import Data.Foldable

import Control.Monad.Random
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore

import Utils

type TestMSF g = MSF (Rand g) Int Int

main :: IO ()
main = testIArray -- print $ runST testMArray

-- MARRAY IS FUCKING SLOW AND BLOWS AWAYS MEMORY LIKE HELL !!!!!!!!!!!!!!!!!!!!!!!
testMArray :: ST s [((Int, Int), Int)]
testMArray = do
  a <- createMArray
  let i = 1000000 :: Int
  forM_ [1..i] (const $ mutateMArray a)
  getAssocs a

createMArray :: ST s (STArray s (Int, Int) Int)
createMArray = do
  let xDim   = 5 :: Int
      yDim   = 5 :: Int
      n      = xDim * yDim
      es     = replicate n (0 :: Int)
      coords = [(x, y) | x <- [0..xDim-1], y <- [0..yDim-1]]
      ec     = zip coords es
    
  a <- newArray_ ((0, 0), (xDim - 1, yDim - 1)) :: ST s (STArray s (Int, Int) Int)
  forM_ ec (uncurry $ writeArray a) 
  return a

mutateMArray :: STArray s (Int, Int) Int -> ST s ()
mutateMArray a = do
  as <- getAssocs a
  mapM_ (\(coord, c) -> writeArray a coord (seq (c+1) (c+1))) as

testIArray :: IO ()
testIArray = do
  let xDim   = 5 :: Int
      yDim   = 5 :: Int
      n      = xDim * yDim
      es     = replicate n (0 :: Int)
      coords = [(x, y) | x <- [0..xDim-1], y <- [0..yDim-1]]

      a = array ((0, 0), (xDim - 1, yDim - 1)) (zip coords es)

  let i = 1000000 :: Int
  print $ foldr' (\_ acc -> foldr' updateElement acc coords) a [1..i]

updateElement :: (Int, Int)
              -> Array (Int, Int) Int
              -> Array (Int, Int) Int
updateElement coord a = a'
  where
    e  = a ! coord
    e' = e + 1
    a' = a // [(coord, e')]
---------------------------------------------------------------------------

-- NOTE: not using {-# LANGUAGE Strict #-} leads to memory leak in this example
testMsfSwitching :: IO ()
testMsfSwitching = do
    let rng = mkStdGen 42

    runMSF 100000 0 switchingMsf rng
  where
    runMSF :: RandomGen g
           => Int
           -> Int
           -> TestMSF g
           -> g
           -> IO ()
    runMSF 0 out _ _ = print out
    runMSF n out msf g = do
      let sfRand             = unMSF msf out
          ((out', msf'), g') = runRand sfRand g

      runMSF (n - 1) out' msf' g'

switchingMsf :: RandomGen g
             => TestMSF g
switchingMsf 
    = continueWithAfter randMsf
  where
    randMsf :: RandomGen g
            => MSF (Rand g) Int (Int, Maybe (TestMSF g))
    randMsf = proc i -> do
      r <- constM getRandom -< ()
      returnA -< (i + r, Just switchingMsf)