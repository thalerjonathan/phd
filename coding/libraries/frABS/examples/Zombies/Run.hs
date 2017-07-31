module Zombies.Run (
    runZombiesWithRendering
  ) where

import           Zombies.Environment
import           Zombies.Init
import           Zombies.Renderer 

import           FRP.FrABS

winSize = (800, 800)
winTitle = "Zombies"
updateStrat = Sequential
shuffleAgents = True
rngSeed = 42
samplingTimeDelta = 1.0  -- NOTE: this model has no time-semantics (it does not matter if it is 1.0 or 0.1)
frequency = 0

runZombiesWithRendering :: IO ()
runZombiesWithRendering =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initZombies

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            renderZombiesFrame
                            Nothing
