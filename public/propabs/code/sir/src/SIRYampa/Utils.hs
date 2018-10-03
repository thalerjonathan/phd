module Utils where

import Control.Monad.Random

rngSplits :: RandomGen g => g -> Int -> [g] -> ([g], g)
rngSplits g 0 acc = (acc, g)
rngSplits g n acc = rngSplits g'' (n - 1) (g' : acc)
  where
    (g', g'') = split g
