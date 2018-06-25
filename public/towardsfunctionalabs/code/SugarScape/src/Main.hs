module Main where

import FRP.BearRiver
import FRP.Chimera

import Environment
import Exporter
import Init
-- import Renderer

winSize :: (Int, Int)
winSize = (800, 800)

winTitle :: String
winTitle = "SugarScape"

rngSeed :: Int
rngSeed = 42

agentCount :: Int
agentCount = 400

envSize :: (Int, Int)
envSize = (50, 50)

dt :: DTime
dt = 1.0     -- this model has discrete time-semantics with a step-with of 1.0 which is relevant for the aging of the agents

t :: Time
t = 200

frequency :: Int
frequency = 0

-- TODO: repair
-- BUG: all agents have id=0 because newAgentId seems not to hand out new ids...

main :: IO ()
main = runSugarScapeStepsAndExport

runSugarScapeStepsAndExport :: IO ()
runSugarScapeStepsAndExport = do
  
  (initAdefs, initEnv) <- createSugarScape agentCount envSize params
  
  let asenv = simulateTime initAdefs dt time
  _
  writeSugarscapeDynamics asenv

{-
-- TODO: rendering is not working ATM because broken in chimera
runSugarScapeWithRendering :: IO ()
runSugarScapeWithRendering = do
  params <- initSimulation updateStrat envBeh envCollapsing shuffleAgents (Just rngSeed)
  (initAdefs, initEnv) <- createSugarScape agentCount envSize params
  
  simulateAndRender initAdefs
                      initEnv
                      params
                      dt
                      frequency
                      winTitle
                      winSize
                      renderSugarScapeFrame
                      Nothing

-- TODO: rendering is not working ATM because broken in chimera
runSugarScapeStepsAndRender :: IO ()
runSugarScapeStepsAndRender = do
  params <- initSimulation updateStrat envBeh envCollapsing shuffleAgents (Just rngSeed)
  (initAdefs, initEnv) <- createSugarScape agentCount envSize params
  
  simulateStepsAndRender initAdefs
                          initEnv
                          params
                          dt
                          time
                          winTitle
                          winSize
                          renderSugarScapeFrame
-}
