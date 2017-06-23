module Wildfire.WildfireRun where

import Wildfire.WildfireInit
import Wildfire.WildfireRenderer as Renderer

import FrABS.Simulation.Simulation
import FrABS.Simulation.SimulationUtils
import FrABS.Rendering.GlossSimulator

import System.Random

winSize = (800, 800)
winTitle = "Wildfire"
updStrat = Parallel
shuffleAgents = False
rngSeed = 42
envSize = (35, 35)
samplingTimeDelta = 0.1
frequency = 0

runWildfireWithRendering :: IO ()
runWildfireWithRendering = 
    do
        initRng rngSeed
        (initAdefs, initEnv) <- initWildfire envSize
        params <- initSimParams updStrat Nothing shuffleAgents

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            Renderer.renderFrame
                            Nothing
                            