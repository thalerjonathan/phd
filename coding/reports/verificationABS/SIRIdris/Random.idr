module Random

import Data.Vect
import Debug.Trace

precision : Int
precision = 1013904223

export
RandomStream : Type
RandomStream = Stream Double

export
randoms : Int -> RandomStream
randoms seed = let seed'  = 1664525 * seed + 1013904223 
                   r      = seed' `shiftR` 2
                   rLimit = mod r precision
                   rNorm  = abs $ cast rLimit / cast precision
                in (rNorm :: randoms seed')

export
split : RandomStream -> (RandomStream, RandomStream)
split (r1 :: r2 :: rr) 
  = let seed1 = cast (r1 * cast precision)
        seed2 = cast (r2 * cast precision)
    in  (randoms seed1, randoms seed2)

export
randomBool : RandomStream -> Double -> (Bool, RandomStream)
randomBool (r :: rs) p = (r <= p, rs)

avoidZero : RandomStream -> Nat -> (Double, RandomStream)
avoidZero rs Z = (0, rs)
avoidZero (r :: rs) (S k) 
  = if (r == 0)
      then avoidZero rs k
      else (r, rs)

export
randomExp : RandomStream -> Double -> (Double, RandomStream)
randomExp (r :: rs) lambda 
    = let (r', rs') = (avoidZero rs 100)
      in  (((-log r') / lambda), rs')

namespace List
  export
  randomElem : RandomStream -> List a -> (Maybe a, RandomStream)

namespace Vector
  export
  randomElem : RandomStream -> Vect n a -> (Maybe a, RandomStream)