module SIRSd

import Data.Vect

%default total

contactRate : Double
contactRate = 5.0

infectivity : Double
infectivity = 0.05

illnessDuration : Double
illnessDuration = 15.0

infections : (s : Double) -> (i : Double) -> (r : Double) -> Double
infections s i r = (s * contactRate * i * infectivity) / (s + i + r)

recoveries : (i : Double) -> Double
recoveries i = i / illnessDuration

data Singleton : Double -> Type where
  S : Singleton val

-- TODO: The goal is to express the positive and negative feedback loops in the types
sir : (s : Double) -> (i : Double) -> (r : Double) -> 
      (s' : Double ** Singleton (s - s'), i' : Double ** Singleton (i + s' - i'), r' : Double ** Singleton (r + r'))
sir s i r = ?sir_rhs