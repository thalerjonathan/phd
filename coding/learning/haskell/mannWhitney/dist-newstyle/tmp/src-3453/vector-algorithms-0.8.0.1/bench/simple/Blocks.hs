{-# LANGUAGE Rank2Types #-}

module Blocks where

import Control.Monad
import Control.Monad.ST

import Data.Vector.Unboxed.Mutable

import System.CPUTime

import System.Random.MWC (GenIO, Variate(..))

-- Some conveniences for doing evil stuff in the ST monad.
-- All the tests get run in IO, but uvector stuff happens
-- in ST, so we temporarily coerce.
clock :: IO Integer
clock = getCPUTime

-- Strategies for filling the initial arrays
rand :: Variate e => GenIO -> Int -> IO e
rand g _ = uniform g

ascend :: Num e => Int -> IO e
ascend = return . fromIntegral

descend :: Num e => e -> Int -> IO e
descend m n = return $ m - fromIntegral n

modulo :: Integral e => e -> Int -> IO e
modulo m n = return $ fromIntegral n `mod` m

-- This is the worst case for the median-of-three quicksort
-- used in the introsort implementation.
medianKiller :: Integral e => e -> Int -> IO e
medianKiller m n'
  | n < k     = return $ if even n then n + 1 else n + k
  | otherwise = return $ (n - k + 1) * 2
 where
 n = fromIntegral n'
 k = m `div` 2
{-# INLINE medianKiller #-}

initialize :: (Unbox e) => MVector RealWorld e -> Int -> (Int -> IO e) -> IO ()
initialize arr len fill = initial $ len - 1
 where initial n = fill n >>= unsafeWrite arr n >> when (n > 0) (initial $ n - 1)
{-# INLINE initialize #-}

speedTest :: (Unbox e) => MVector RealWorld e
                       -> Int
                       -> (Int -> IO e)
                       -> (MVector RealWorld e -> IO ())
                       -> IO Integer
speedTest arr n fill algo = do
  initialize arr n fill
  t0 <- clock
  algo arr
  t1 <- clock
  return $ t1 - t0
{-# INLINE speedTest #-}


