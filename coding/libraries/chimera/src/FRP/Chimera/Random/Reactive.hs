{-# LANGUAGE Arrows #-}
module FRP.Chimera.Random.Reactive 
  (
    randomBoolS
  , randomElemS
  ) where

import Control.Monad.Random
import Control.Monad.Trans.MSF.Random 

import FRP.BearRiver

randomBoolS :: MonadRandom m => Double -> MSF m a Bool
randomBoolS p = proc _ -> do
  r <- getRandomRS ((0, 1) :: (Double, Double)) -< ()
  returnA -< r <= p

randomElemS :: MonadRandom m => MSF m [a] a
randomElemS = proc as -> do
  let len = length as
  idx <- getRandomRS_ -< (0, len - 1) 
  returnA -< (as !! idx)