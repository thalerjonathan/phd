module Wildfire.WildfireRun where

module Wildfire.WildfireRun where

import Wildfire.WildfireModel
import Wildfire.WildfireInit
import Wildfire.WildfireRenderer as Renderer

import System.Random

import FrABS.Rendering.GlossSimulator

winSize = (800, 800)
winTitle = "Wildfire"

rngSeed = 42
envSize = (35, 35)
samplingTimeDelta = 0.2
frequency = 0 -- NOTE: when using LE 0 then simulation runs at max

runWildfireWithRendering :: IO ()
runWildfireWithRendering = 
    do
        initRng rngSeed
        (initAdefs, initEnv) <- initWildfire envSize
        params <- simParams

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize


initRng :: Int -> IO StdGen
initRng seed =
    do
        let g = mkStdGen seed
        setStdGen g
        return g


simParams :: IO (SimulationParams WildfireCell WildfireLinkLabel)
simParams = 
    do
        rng <- getSplit
        return SimulationParams {
            simStrategy = Parallel,   -- NOTE: wildfire should work both for parallel and sequential
            simEnvCollapse = Nothing,
            simShuffleAgents = False,   -- NOTE: don't forget to shuffle the agents in case of sequential
            simRng = rng
        }

