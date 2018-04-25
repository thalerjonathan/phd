module SIRSd

import Data.Vect

%default total

contactRate : Double
contactRate = 5.0

infectivity : Double
infectivity = 0.05

illnessDuration : Double
illnessDuration = 15.0

infectionRate : (s : Double) -> (i : Double) -> (r : Double) -> Double
infectionRate s i r = (s * contactRate * i * infectivity) / (s + i + r)

recoveryRate : (i : Double) -> Double
recoveryRate i = i / illnessDuration

data Singleton : Double -> Type where
  S : Singleton val

-- TODO: The goal is to express the positive and negative feedback loops in the types:
-- We need to produce somehow an ir = infectionRate s i r and a rr = recoveryRate i
-- which we have access to in the output types and then we can express the feedback loops:
-- the new susceptibles: s' = s - ir
-- the new infected:     i' = i + ir - rr
-- the new recovered:    r' = r + rr
sir : (s : Double) -> (i : Double) -> (r : Double) -> 
      (x : Singleton s ** Singleton s)
      --(ir : Double ** Singleton (s - ir'), ir : Double ** Singleton (i + s' - i'), r' : Double ** Singleton (r + r'))
sir s i r = ?sir_rhs

{-
Tuple : Nat -> Type -> Type
Tuple Z t = ()
Tuple (S k) t = (t, Tuple k t)

Tuple : Double -> Double -> Double -> Type
Tuple s i r = (Double, Double, Double)
  where
    ir : Double
    ir = infectionRate s i r

    rr : Double
    rr = recoveryRate i

sir : (s : Double) -> (i : Double) -> (r : Double) -> Tuple s i r 
-}