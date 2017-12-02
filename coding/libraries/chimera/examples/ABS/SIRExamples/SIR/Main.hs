module Main 
  (
    runSIRStepsAndWriteToFile,
  )  where

import FRP.Chimera
import FRP.Yampa

import Init
import Model
import Sir

updateStrat :: UpdateStrategy
updateStrat = Parallel

shuffleAgents :: Bool
shuffleAgents = False

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 1.0

t :: DTime
t = 150

agentCount :: Int
agentCount = 10000

numInfected :: Int
numInfected = 10

main :: IO ()
main = runSIRStepsAndWriteToFile

-- TODO: repair

runSIRStepsAndWriteToFile :: IO ()
runSIRStepsAndWriteToFile = do
  params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
  (initAdefs, initEnv) <- createSIRNumInfected agentCount numInfected

  let dynamics = simulateAggregateTime initAdefs initEnv params dt t aggregate
  let fileName = "sirDynamics_" 
                  ++ show agentCount ++ "agents_" 
                  ++ show t ++ "time_"
                  ++ show dt ++ "dt_"
                  ++ show updateStrat ++ ".m"

  writeSirDynamicsFile fileName dt 0 dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
aggregate :: (Time, [SIRAgentObservable], SIREnvironment) -> (Time, Double, Double, Double)
aggregate (t, aobs, _) = (t, susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = fromIntegral $ length $ filter ((Susceptible==) . sirState . snd) aobs
    infectedCount = fromIntegral $ length $ filter ((Infected==) . sirState . snd) aobs
    recoveredCount = fromIntegral $ length $ filter ((Recovered==) . sirState . snd) aobs