module FRP.Chimera.Random.Monadic 
  (
    randomBoolM
  , randomExpM
  , avoidM
  ) where

import Control.Monad.Random

randomBoolM :: RandomGen g => Double -> Rand g Bool
randomBoolM p = getRandomR (0.0, 1.0) >>= (\r -> return $ p >= r)

-- NOTE: THIS CODE INSPIRED BY Euterpea-1.0.0 (I didn't want to create dependencies and their implementation seems neat and tidy)
randomExpM :: RandomGen g => Double -> Rand g Double
randomExpM lambda = avoidM 0 >>= (\r -> return $ ((-log r) / lambda))
--randomExpM lambda = avoid 0 >>= (\r -> 1 - exp (-(dt/t_avg)))

-- NOTE: THIS CODE INSPIRED BY Euterpea-1.0.0 (I didn't want to create dependencies and their implementation seems neat and tidy)
avoidM :: (Random a, Eq a, RandomGen g) => a -> Rand g a
avoidM x = do
  r <- getRandom
  if (r == x) 
    then avoidM x
    else return r