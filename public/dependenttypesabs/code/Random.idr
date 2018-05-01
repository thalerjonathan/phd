module Random

import Data.Vect
import Debug.Trace

%default total

maxInt : Int
maxInt = 2147483648 -- = 2^31

export
RandomStream : Type
RandomStream = Stream Double

export
randoms : Int -> RandomStream
randoms seed = let seed'  = 1664525 * seed + 1013904223 
                   r      = seed' `shiftR` 2
                   rNorm  = abs $ cast r / cast maxInt
                in rNorm :: randoms seed'

export
split : RandomStream -> (RandomStream, RandomStream)
split (r1 :: r2 :: rr) 
  = let seed1 = cast (r1 * cast maxInt)
        seed2 = cast (r2 * cast maxInt)
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
  randomElem (r :: rs) xs 
    = let n         = length xs
          randIx    = r * cast n
          randIxNat = fromIntegerNat $ cast randIx
          inListPrf = inBounds randIxNat xs
      in  case inListPrf of
            (Yes prf)   => (Just $ index randIxNat xs, rs) 
            (No contra) => (Nothing, rs)

namespace Vector
  -- TODO: if we use a Vect (S n) a then it is guaranteed that it
  -- has at least one element (it is not empty) => we can omit
  -- Maybe
  export
  randomElem : RandomStream -> Vect n a -> (Maybe a, RandomStream)
  randomElem {n} (r :: rs) xs
    = let randIx     = r * cast n
          mrandIxFin = integerToFin (cast randIx) n
      in  case mrandIxFin of
            (Just randIxFin) => (Just $ index randIxFin xs, rs)
            Nothing          => (Nothing, rs)
