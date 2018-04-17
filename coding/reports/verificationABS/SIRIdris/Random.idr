module Random

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