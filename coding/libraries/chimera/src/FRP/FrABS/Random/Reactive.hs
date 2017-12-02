{-# LANGUAGE Arrows #-}
module FRP.FrABS.Random.Reactive 
  (
    randomSF
  , randomBoolSF
  , drawRandomElemSF
  ) where

import Control.Monad.Random

import FRP.Yampa

randomSF :: RandomGen g => g -> SF (Rand g a) a
randomSF initRng = proc f -> do
  rec
    g' <- iPre initRng -< g
    let (a, g) = runRand f g'

  returnA -< a

randomBoolSF :: (RandomGen g) => g -> Double -> SF () Bool
randomBoolSF g p = proc _ -> do
  r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
  returnA -< (r <= p)

drawRandomElemSF :: (RandomGen g, Show a) => g -> SF [a] a
drawRandomElemSF g = proc as -> do
  r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
  let len = length as
  let idx = (fromIntegral $ len) * r
  let a =  as !! (floor idx)
  returnA -< a