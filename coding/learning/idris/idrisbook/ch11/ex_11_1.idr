import Data.Primitives.Views

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'
every_other : Stream a -> Stream a
every_other (x :: y :: xs) = y :: every_other xs

-- Functor for InfList is implemented in InfList.idr

data Face = Heads | Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips Z xs = []
coinFlips (S k) (value :: xs) with (divides value 2)
  coinFlips (S k) (((2 * div) + rem) :: xs) | (DivBy prf) 
    = case rem == 0 of
           False => Heads :: coinFlips k xs
           True  => Tails :: coinFlips k xs

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = let next = (approx + (number / approx)) / 2 in
                                       approx :: square_root_approx number next 

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound Z number bound (value :: xs) = value
square_root_bound (S k) number bound (value :: xs) 
  = if (value * value - number) < bound
      then value
      else square_root_bound k number bound xs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.000000001 (square_root_approx number number)