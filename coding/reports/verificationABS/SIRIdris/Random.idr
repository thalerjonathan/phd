module Random

import Data.Vect

precision : Int
precision = 10000

export
RandomStream : Type
RandomStream = Stream Double

export
randoms : Int -> RandomStream
randoms seed = let seed'  = 1664525 * seed + 1013904223 
                   r      = seed' `shiftR` 2
                   rLimit = mod r precision
                   rNorm  = cast rLimit / cast precision
                in rNorm :: randoms seed'

export
split : RandomStream -> (RandomStream, RandomStream)
split (r1 :: r2 :: rr) 
  = let seed1 = cast r1 * precision
        seed2 = cast r2 * precision
    in  (randoms seed1, randoms seed2)

export
randomBool : Double -> RandomStream -> (Bool, RandomStream)
randomBool p (r :: rs) = (r <= p, rs)

export
randomExp : Double -> RandomStream -> (Double, RandomStream)
randomExp lambda (r :: rs) = (((-log r) / lambda), rs)

namespace List
  export
  -- TODO: proof of non-empty list to get rid of Maybe
  randomElem : List a -> RandomStream -> (Maybe a, RandomStream)

namespace Vector
  export
  -- TODO: proof of non-empty vector to get rid of Maybe
  randomElem : Vect n a -> RandomStream -> (Maybe a, RandomStream)