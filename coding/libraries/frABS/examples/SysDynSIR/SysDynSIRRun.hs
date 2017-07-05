module SysDynSIR.SysDynSIRRun ( 
    runSysDynSIRStepsAndWriteToFile
  ) where

import SysDynSIR.SysDynSIRInit
import SysDynSIR.SysDynSIRModel

import Utils.Utils

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.SimulationUtils
import FrABS.Rendering.GlossSimulator

import Data.Ord
import Data.List

import Debug.Trace 

samplingTimeDelta = 1.0
steps = 100

runSysDynSIRStepsAndWriteToFile :: IO ()
runSysDynSIRStepsAndWriteToFile =
    do
        -- SystemDynamics MUST NOT rely on RNGs at all, so no need to initialize it 
        -- _ <- initRng rngSeed
        (initAdefs, initEnv) <- createSysDynSIR
        -- SystemDynamics MUST ABSOLUTELY only run Parllel and there is no need to shuffle the agents (=stocks)
        params <- initSimParams Parallel Nothing False

        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps
        let dynamics = map (calculateDynamics . fst) asenv
        let fileName = "sysDynSIRDynamics_" ++ show steps ++ "steps_" ++ show samplingTimeDelta ++ "_dt.m"

        writeSirsDynamicsFile fileName steps samplingTimeDelta 0 dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
-- NOTE: here we rely on the fact the we have exactly three stocks and sort them by their id to access them
--          stock id 0: Susceptible
--          stock id 1: Infectious
--          stock id 2: Recovered
--          the remaining items are the flows
calculateDynamics :: [SysDynSIROut] -> (Double, Double, Double)
calculateDynamics unsortedStocks = (susceptibleCount, infectedCount, recoveredCount) -- (susceptibleRatio, infectedRatio, recoveredRatio)
    where
        stocks = sortBy (\s1 s2 -> compare (aoId s1) (aoId s2)) unsortedStocks
        (susceptibleStock : infectiousStock : recoveredStock : remainingFlows) = stocks

        susceptibleCount = aoState susceptibleStock
        infectedCount = aoState infectiousStock
        recoveredCount = aoState recoveredStock

        totalCount = susceptibleCount + infectedCount + recoveredCount 

        susceptibleRatio = susceptibleCount / totalCount
        infectedRatio = infectedCount / totalCount 
        recoveredRatio = recoveredCount / totalCount