module SocialForce.Run (
    runSocialForce
  ) where

import FRP.FrABS
import FRP.Yampa
import Control.Monad.Random

import SocialForce.Init
import SocialForce.Renderer

winSize :: (Int, Int)
winSize = (1000, 600)

winTitle :: String
winTitle = "SocialForce"

frequency :: Int
frequency = 0

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 0.01

runSocialForce :: IO ()
runSocialForce = do
  params <- initSimulation Sequential Nothing Nothing False (Just rngSeed)
    
  (initAdefs, initEnv) <- evalRandIO initSocialForce

  simulateAndRender 
    initAdefs
    initEnv
    params
    dt
    frequency
    winTitle
    winSize
    renderSocialForceFrame
    Nothing --(Just (\_ asenv -> printAgentDynamics asenv))