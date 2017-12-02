module Run 
  (
    main
  ) where

import FRP.Chimera
import FRP.Yampa

import Init
import Renderer

winSize :: (Int, Int)
winSize = (800, 800)

winTitle :: String
winTitle = "Wildfire"

updateStrat :: UpdateStrategy
updateStrat = Parallel

shuffleAgents :: Bool
shuffleAgents = False

rngSeed :: Int
rngSeed = 42

envSize :: (Int, Int)
envSize = (59, 59)

dt :: DTime
dt = 0.1

frequency :: Int
frequency = 0

main :: IO ()
main = do
  params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
  (initAdefs, initEnv) <- initWildfire envSize
  
  simulateAndRender 
    initAdefs
    initEnv
    params
    samplingTimeDelta
    frequency
    winTitle
    winSize
    renderWildfireFrame
    Nothing