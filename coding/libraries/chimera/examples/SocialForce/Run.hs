module SocialForce.Run (
    runSocialForce
  ) where

import Control.Monad.Random

import FRP.FrABS
import FRP.Yampa

import SocialForce.Init
import SocialForce.Renderer
import SocialForce.Model 

winSize :: (Int, Int)
winSize = (1000, 600)

winTitle :: String
winTitle = "SocialForce"

frequency :: Int
frequency = 0

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = unitTime -- 0.1

-- TODO: unfinished implementation

runSocialForce :: IO ()
runSocialForce = do
  params <- initSimulation Sequential Nothing Nothing True (Just rngSeed)
    
  (initAdefs, initEnv) <- evalRandIO $ initSocialForce params

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