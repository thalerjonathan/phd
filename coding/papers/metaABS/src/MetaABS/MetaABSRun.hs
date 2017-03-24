module MetaABS.MetaABSRun where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation

import MetaABS.MetaABSModel
import MetaABS.MetaABSInit

import FRP.Yampa
import qualified FrABS.Rendering.Agents2DDiscrete as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate

import Data.IORef
import System.IO
import System.Random

rngSeed = 42
agentCount = 1
parallelStrategyFlag = False

runMetaABSStepsAndPrint :: IO ()
runMetaABSStepsAndPrint = do
                            hSetBuffering stdout NoBuffering
                            initRng rngSeed
                            (as, env) <- createMetaABSAgentsAndEnv agentCount

                            let steps = 1
                            let ass = processSteps as env parallelStrategyFlag 1.0 steps
                            let as' = last ass

                            mapM (putStrLn . show . aoState) as'

                            return ()

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g