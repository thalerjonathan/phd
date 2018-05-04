module RandomUtils

import Effects
import Effect.Random

%access export

randomDouble : Eff Double [RND]
randomDouble = do
  ri <- rndInt 1 100000
  let r = cast ri / 100000
  pure r

randomExp : Double -> Eff Double [RND]
randomExp lambda = do
  r <- randomDouble
  pure $ ((-log r) / lambda)

randomBool : Double -> Eff Bool [RND]
randomBool p = do
  r <- randomDouble
  pure (p >= r)