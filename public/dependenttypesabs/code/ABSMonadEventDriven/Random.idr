module Random

import Data.Vect
import Debug.Trace

%default total

export
RandomStream : Type
RandomStream = Stream Integer

export
randoms : Integer -> RandomStream
randoms seed = let seed'  = assert_total ((1664525 * seed + 1013904223) `prim__sremBigInt` (pow 2 32))
                   --r      = seed' `shiftR` 2
                in seed' :: randoms seed'

export
split : RandomStream -> (RandomStream, RandomStream)
split (r1 :: r2 :: rr) = (randoms r1, randoms r2)

export
randomRange : RandomStream -> 
              (lower : Integer) ->
              (upper : Integer) ->
              (Integer, RandomStream)
randomRange (r :: rs) lower upper 
  = let r' = assert_total (((divBigInt r 2) `prim__sremBigInt` (upper - lower)) + lower)
    in  (r', rs)

export
randomDouble : RandomStream -> (Double, RandomStream)
randomDouble rs
  = let upper    = 10000
        (r, rs') = randomRange rs 0 10000
        rd       = cast r / cast upper
    in  (rd, rs')

export
randomBool : RandomStream -> Double -> (Bool, RandomStream)
randomBool rs p 
  = let (r, rs') = randomDouble rs
    in  (r <= p, rs')

avoidZero : RandomStream -> Nat -> (Double, RandomStream)
avoidZero rs Z = (0, rs)
avoidZero rs (S k) 
  = let (r, rs') = randomDouble rs
    in if (r == 0)
        then avoidZero rs' k
        else (r, rs')

export
randomExp : RandomStream -> Double -> (Double, RandomStream)
randomExp rs lambda 
    = let (r', rs') = (avoidZero rs 100)
      in  (((-log r') / lambda), rs')

export
randomFin : RandomStream -> (k : Nat) -> (Fin (S k), RandomStream)
randomFin rs k
  = let (r, rs')   = randomDouble rs
        randIx     = r * cast k
        mrandIxFin = integerToFin (cast randIx) (S k)
    in  case mrandIxFin of
          (Just randIxFin) => (randIxFin, rs')
          Nothing          => (FZ, rs')

namespace List
  export
  randomElem : RandomStream -> List a -> (Maybe a, RandomStream)
  randomElem rs xs 
    = let n         = length xs
          (r, rs')  = randomDouble rs
          randIx    = r * cast n
          randIxNat = fromIntegerNat $ cast randIx
          inListPrf = inBounds randIxNat xs
      in  case inListPrf of
            (Yes prf)   => (Just $ index randIxNat xs, rs') 
            (No contra) => (Nothing, rs')
    
namespace Vector
  export
  randomElem : RandomStream -> Vect (S n) a -> (a, RandomStream)
  randomElem {n} rs xs
    = let (randIdx, rs') = randomFin rs n 
          randElem = index randIdx xs
      in (randElem, rs')
    
  export
  randomElem' : RandomStream -> Vect n a -> (Maybe a, RandomStream)
  randomElem' {n} rs xs
    = let (r, rs')   = randomDouble rs
          randIx     = r * cast n
          mrandIxFin = integerToFin (cast randIx) n
      in  case mrandIxFin of
            (Just randIxFin) => (Just $ index randIxFin xs, rs')
            Nothing          => (Nothing, rs')
    
  testVect : Vect 7 Nat
  testVect = [1, 2, 3, 4, 5, 6, 7]

  testRandomVectElem : IO ()
  testRandomVectElem = do
    let rs0 = randoms 66
    
    let (r0, rs1) = randomElem rs0 testVect
    putStrLn $ show r0

    let (r1, rs2) = randomElem rs1 testVect
    putStrLn $ show r1


testRandomDouble : IO ()
testRandomDouble = do
  let rs0 = randoms 42
  let (r0, rs1) = randomDouble rs0
  putStrLn $ show r0
  let (r1, rs2) = randomDouble rs1
  putStrLn $ show r1
  let (r2, _) = randomDouble rs2
  putStrLn $ show r2

testRandomRange : IO ()
testRandomRange = do
  let rs0 = randoms 42
  let (r0, rs1) = randomRange rs0 1 10
  putStrLn $ show r0
  let (r1, rs2) = randomRange rs1 1 10
  putStrLn $ show r1
  let (r2, _) = randomRange rs2 1 10
  putStrLn $ show r2