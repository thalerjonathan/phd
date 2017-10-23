module SysDynSIR.Run 
    ( 
      runSysDynSIRStepsAndWriteToFile
    ) where

import Data.List

import FRP.Yampa
import FRP.FrABS

import SysDynSIR.Init
import SysDynSIR.Model
import Utils.Sirs

dt :: DTime
dt = 0.05

t :: DTime
t = 150

runSysDynSIRStepsAndWriteToFile :: IO ()
runSysDynSIRStepsAndWriteToFile = writeSirsDynamicsFile fileName dt 0 dynamics
  where
    sdDefs = createSysDynSIR
    
    sdObs = runSD sdDefs dt t         
    dynamics = map calculateDynamics sdObs

    fileName = "sysDynSIRDynamics_" 
                    ++ show totalPopulation ++ "population_"
                    ++ show t ++ "time_" 
                    ++ show dt ++ "dt.m"

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
-- NOTE: here we rely on the fact the we have exactly three stocks and sort them by their id to access them
--          stock id 0: Susceptible
--          stock id 1: Infectious
--          stock id 2: Recovered
--          the remaining items are the flows
calculateDynamics :: (Time, [SDObservable]) -> (Time, Double, Double, Double)
calculateDynamics (t, unsortedStocks) = (t, susceptibleCount, infectedCount, recoveredCount) 
  where
    stocks = sortBy (\s1 s2 -> compare (fst s1) (fst s2)) unsortedStocks
    ((_, susceptibleCount) : (_, infectedCount) : (_, recoveredCount) : _) = stocks