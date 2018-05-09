module RandomUtils

import Data.Fin

import Effects
import Effect.Random

%access export
%default total

randomDouble : Eff Double [RND]
randomDouble = do
  -- TODO: why is rndInt not total????
  ri <- assert_total $ rndInt 1 100000
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