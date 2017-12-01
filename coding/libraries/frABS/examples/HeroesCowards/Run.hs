module HeroesCowards.Run (
    runHeroesCowardsWithRendering
  ) where

import HeroesCowards.Init
import HeroesCowards.Model
import HeroesCowards.Renderer

import FRP.FrABS

winSize = (800, 800)
winTitle = "Heroes & Cowards"
frequency = 0
updateStrat = Sequential
shuffleAgents = True
rngSeed = 42
agentCount = 500
samplingTimeDelta = 1.0

-- TODO: repair

runHeroesCowardsWithRendering :: IO ()
runHeroesCowardsWithRendering =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        
        (initAdefs, initEnv) <- createHeroesCowards agentCount

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            renderHeroesCowardsFrame
                            Nothing