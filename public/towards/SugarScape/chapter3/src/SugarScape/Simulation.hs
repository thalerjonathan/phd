module SugarScape.Simulation
  ( SimulationState (..)
  , SimStepOut
  , AgentObservable

  , initSimulation
  , initSimulationOpt
  , initSimulationRng
  , initSimulationSeed

  , simulationStep
  , simStepSF

  , mkSimState
  , simulateUntil

  , sugarScapeTimeDelta
  ) where

import Data.Maybe
import System.Random

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict
import FRP.BearRiver

import SugarScape.AgentMonad
import SugarScape.Environment
import SugarScape.Model
import SugarScape.Init
import SugarScape.Random

type AgentObservable o = (AgentId, o)
type SimStepOut        = (Time, SugEnvironment, [AgentObservable SugAgentObservable])

-- NOTE: strictness on those fields, otherwise space-leak 
-- (memory consumption increases throughout run-time and execution gets slower and slower)
data SimulationState g = SimulationState 
  { simSf       :: SF (SugAgentMonadT g) () [AgentObservable SugAgentObservable]
  , simAbsState :: !ABSState 
  , simEnv      :: !SugEnvironment
  , simRng      :: !g
  , simSteps    :: !Int
  }

-- sugarscape is stepped with a time-delta of 1.0
sugarScapeTimeDelta :: DTime
sugarScapeTimeDelta = 1.0

initSimulation :: SugarScapeParams
               -> IO (SimulationState StdGen, SugEnvironment)
initSimulation params = do
  g0 <- newStdGen
  return $ initSimulationRng g0 params

initSimulationOpt :: Maybe Int
                  -> SugarScapeParams
                  -> IO (SimulationState StdGen, SugEnvironment)
initSimulationOpt Nothing     params = initSimulation params
initSimulationOpt (Just seed) params = return $ initSimulationSeed seed params

initSimulationSeed :: Int
                   -> SugarScapeParams
                   -> (SimulationState StdGen, SugEnvironment)
initSimulationSeed seed = initSimulationRng g0
  where
    g0 = mkStdGen seed

initSimulationRng :: RandomGen g
                   => g
                   -> SugarScapeParams
                   -> (SimulationState g, SugEnvironment)
initSimulationRng g0 params = (initSimState, initEnv)
  where
    -- split
    (gSim, shuffleRng)         = split g0
    -- initial agents and environment data
    ((initAs, initEnv), gSim') = runRand (createSugarScape params) gSim
    -- initial simulation state
    (initAis, initSfs)     = unzip initAs
    -- initial simulation state
    initSimState           = mkSimState 
                              (simStepSF initAis initSfs (sugEnvironment params) shuffleRng) 
                              (mkAbsState $ maximum initAis) initEnv gSim' 0

simulateUntil :: RandomGen g
              => Time
              -> SimulationState g
              -> [SimStepOut]
simulateUntil tMax ss0 = simulateUntilAux ss0 []
  where
    simulateUntilAux :: SimulationState g
                     -> [SimStepOut]
                     -> [SimStepOut]
    simulateUntilAux ss acc
        | t < tMax  = simulateUntilAux ss' acc'
        | otherwise = reverse acc'
      where
        (ss', so@(t, _, _)) = simulationStep ss
        acc' = so : acc

simulationStep :: SimulationState g
               -> (SimulationState g, SimStepOut)
simulationStep ss = (ss', (t, env', out))
  where
    sf       = simSf ss
    absState = simAbsState ss
    env      = simEnv ss
    g        = simRng ss
    steps    = simSteps ss

    sfReader   = unMSF sf ()
    sfAbsState = runReaderT sfReader sugarScapeTimeDelta
    sfEnvState = runStateT sfAbsState absState
    sfRand     = runStateT sfEnvState env
    ((((out, sf'), absState'), env'), g') = runRand sfRand g

    t          = absTime absState 
    absState'' = absState' { absTime = t + sugarScapeTimeDelta }
    ss'        = mkSimState sf' absState'' env' g' (steps + 1)

simStepSF :: RandomGen g
          => [AgentId]
          -> [SugAgent g]
          -> SugAgent g
          -> g
          -> SF (SugAgentMonadT g) () [AgentObservable SugAgentObservable]
simStepSF ais0 sfs0 envSf0 shuffleRng = MSF $ \_ -> do
  let (sfsAis, shuffleRng') = fisherYatesShuffle shuffleRng (zip sfs0 ais0)
      (sfs, ais)            = unzip sfsAis

  ret         <- mapM (`unMSF` TimeStep) sfs
  -- NOTE: run environment separately after all agents
  (_, envSf') <- unMSF envSf0 TimeStep 

  let adefs = concatMap (\(ao, _) -> aoCreate ao) ret
      newSfs = map adBeh adefs
      newAis = map adId adefs

      obs = foldr (\((ao, _), aid) acc -> 
        if isObservable ao 
          then (aid, fromJust $ aoObservable ao) : acc  
          else acc) [] (zip ret ais)

      (sfs', ais') = foldr (\((ao, sf), aid) acc@(accSf, accAid) -> 
        if isDead ao 
          then acc 
          else (sf : accSf, aid : accAid)) ([], []) (zip ret ais)

      ct = simStepSF (newAis ++ ais') (newSfs ++ sfs') envSf' shuffleRng'

  return (obs, ct)

mkSimState :: SF (SugAgentMonadT g) () [AgentObservable SugAgentObservable]
           -> ABSState
           -> SugEnvironment
           -> g
           -> Int
           -> SimulationState g
mkSimState sf absState env g steps = SimulationState 
  { simSf       = sf
  , simAbsState = absState 
  , simEnv      = env
  , simRng      = g
  , simSteps    = steps
  }