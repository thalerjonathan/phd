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
infectionRate s i r = i * contactRate * s / (s + i + r) * infectivity --(s * contactRate * i * infectivity) / (s + i + r)

recoveryRate : (i : Double) -> Double
recoveryRate i = i / illnessDuration

data Triple : Double -> Double -> Double -> Type where
  MkTriple : (fst : Double) -> (snd : Double) -> (trd : Double) -> Triple v1 v2 v3

-- TODO: The goal is to express the positive and negative feedback loops in the types:
-- We need to produce somehow an ir = infectionRate s i r and a rr = recoveryRate i
-- which we have access to in the output types and then we can express the feedback loops:
-- the new susceptibles: s' = s - ir
-- the new infected:     i' = i + ir - rr
-- the new recovered:    r' = r + rr
sirStep : (dt : Double) -> (s : Double) -> (i : Double) -> (r : Double) -> 
          (ir : Double ** Triple (s - ir) (i + ir) r)
sirStep dt s i r = (ir ** (MkTriple (s - ir) (i + ir - rr) (r + rr)))
  where
    ir : Double
    ir = dt * infectionRate s i r

    rr : Double
    rr = dt * recoveryRate i

extrTri : Triple a1 a2 a3 -> (Double, Double, Double)
extrTri (MkTriple fst snd trd) = (fst, snd, trd)

runSir : IO ()
runSir = do
    let steps = fromIntegerNat $ cast (t / dt)
    runSirAux steps 10000 1 0
  where
    dt : Double
    dt = 0.001

    t : Double
    t = 250

    runSirAux : Nat -> (s : Double) -> (i : Double) -> (r : Double) -> IO ()
    runSirAux Z s i r = do
      putStrLn "Simulation run for all steps"
      putStrLn $ "s = " ++ show s
      putStrLn $ "i = " ++ show i
      putStrLn $ "r = " ++ show r
    runSirAux (S n) s i r = 
      if i < 1
        then do
          putStrLn $ "Simulation reached equilibrium and left " ++ show n ++ " steps"
          putStrLn $ "s = " ++ show s
          putStrLn $ "i = " ++ show i
          putStrLn $ "r = " ++ show r

        else do
          let (ir ** tri) = sirStep dt s i r
          let (s', i', r') = extrTri tri
{-
          putStrLn $ "ir = " ++ show ir
          putStrLn $ "s' = " ++ show s'
          putStrLn $ "i' = " ++ show i'
          putStrLn $ "r' = " ++ show r'
    -}
          runSirAux n s' i' r'
