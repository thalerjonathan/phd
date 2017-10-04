module SysDynSIR.Run ( 
    runSysDynSIRStepsAndWriteToFile
  ) where

import SysDynSIR.Init
import SysDynSIR.Model

import Utils.Sirs

import FRP.FrABS

import Data.List

samplingTimeDelta = 0.1
steps = 1500

runSysDynSIRStepsAndWriteToFile :: IO ()
runSysDynSIRStepsAndWriteToFile =
    do
        let sdDefs = createSysDynSIR
        
        let sdObs = runSD 
                        sdDefs 
                        samplingTimeDelta 
                        steps
                        
        let dynamics = map calculateDynamics sdObs
        let fileName = "sysDynSIRDynamics_" 
                        ++ show totalPopulation ++ "population_"
                        ++ show steps ++ "steps_" 
                        ++ show samplingTimeDelta ++ "dt.m"

        writeSirsDynamicsFile fileName steps samplingTimeDelta 0 dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
-- NOTE: here we rely on the fact the we have exactly three stocks and sort them by their id to access them
--          stock id 0: Susceptible
--          stock id 1: Infectious
--          stock id 2: Recovered
--          the remaining items are the flows
calculateDynamics :: [SDObservable] -> (Double, Double, Double)
calculateDynamics unsortedStocks = (susceptibleCount, infectedCount, recoveredCount) -- (susceptibleRatio, infectedRatio, recoveredRatio)
    where
        stocks = sortBy (\s1 s2 -> compare (fst s1) (fst s2)) unsortedStocks
        (susceptibleStock : infectiousStock : recoveredStock : remainingFlows) = stocks

        susceptibleCount = snd susceptibleStock
        infectedCount = snd infectiousStock
        recoveredCount = snd recoveredStock

        totalCount = susceptibleCount + infectedCount + recoveredCount 

        susceptibleRatio = susceptibleCount / totalCount
        infectedRatio = infectedCount / totalCount 
        recoveredRatio = recoveredCount / totalCount