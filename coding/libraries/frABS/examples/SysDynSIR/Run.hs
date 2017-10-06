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

samplingTimeDelta :: DTime
samplingTimeDelta = 0.05

steps :: Int
steps = 3000

runSysDynSIRStepsAndWriteToFile :: IO ()
runSysDynSIRStepsAndWriteToFile = writeSirsDynamicsFile fileName steps samplingTimeDelta 0 dynamics
  where
    sdDefs = createSysDynSIR
    
    sdObs = runSD sdDefs samplingTimeDelta steps         
    dynamics = map calculateDynamics sdObs

    fileName = "sysDynSIRDynamics_" 
                    ++ show totalPopulation ++ "population_"
                    ++ show steps ++ "steps_" 
                    ++ show samplingTimeDelta ++ "dt.m"

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
-- NOTE: here we rely on the fact the we have exactly three stocks and sort them by their id to access them
--          stock id 0: Susceptible
--          stock id 1: Infectious
--          stock id 2: Recovered
--          the remaining items are the flows
calculateDynamics :: [SDObservable] -> (Double, Double, Double)
calculateDynamics unsortedStocks = (susceptibleCount, infectedCount, recoveredCount) 
  where
    stocks = sortBy (\s1 s2 -> compare (fst s1) (fst s2)) unsortedStocks
    ((_, susceptibleCount) : (_, infectedCount) : (_, recoveredCount) : _) = stocks