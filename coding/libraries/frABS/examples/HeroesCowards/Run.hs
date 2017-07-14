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
updateStrat = Parallel
shuffleAgents = False
rngSeed = 42
agentCount = 3
samplingTimeDelta = 1.0

{-
- use-case for continuous 2d-environment: implement Heroes & Cowards
        -> write Agend2DContinuous
            - continuous 2d env: just add a map of agentids with their positions to the env, agents can then update their continuous position (needs to remove itself when killed). problem: environment needs to know about agentid. but do we really need that? it would save us exchanging messages.
            - basically it would suffice to add another field: posCont and make the other posDisc. or can we distinguish by types the position: any num type
            - maybe distinguish between discrete agent and continuous agent
            - distinguish between cont and disc env
-}

runHeroesCowardsWithRendering :: IO ()
runHeroesCowardsWithRendering =
    do
        params <- initSimulation updateStrat Nothing shuffleAgents (Just rngSeed)
        
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