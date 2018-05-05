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

{-}
-- TODO: if we use a Vect (S n) a then it is guaranteed that it
-- has at least one element (it is not empty) => we can omit
-- Maybe
randomElem : RandomStream -> Vect n a -> (Maybe a, RandomStream)
randomElem {n} (r :: rs) xs
  = let randIx     = r * cast n
        mrandIxFin = integerToFin (cast randIx) n
    in  case mrandIxFin of
          (Just randIxFin) => (Just $ index randIxFin xs, rs)
          Nothing          => (Nothing, rs)
-}