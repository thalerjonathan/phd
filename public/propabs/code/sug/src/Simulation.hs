module Simulation
  ( SimulationState (..)
  , SimStepOut

  , simulationStep
  , simStepSF

  , mkSimState
  , simulateUntil

  , sugarScapeTimeDelta
  ) where

import            Data.Maybe
import            System.Random

import            Control.Monad.Random
import            Control.Monad.Reader
import            Control.Monad.State.Strict
import            FRP.BearRiver

import            AgentMonad
import            Model
import            Random
import            Renderer

type SimStepOut = (Time, SugEnvironment, [AgentObservable SugAgentObservable])

data SimulationState g = SimulationState 
  { simSf       :: SF (SugAgentMonadT g) () [AgentObservable SugAgentObservable]
  , simAbsState :: ABSState 
  , simEnv      :: SugEnvironment
  , simRng      :: g
  , simSteps    :: Int
  }

-- sugarscape is stepped with a time-delta of 1.0
sugarScapeTimeDelta :: DTime
sugarScapeTimeDelta = 1.0

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
          -> g
          -> SF (SugAgentMonadT g) () [AgentObservable SugAgentObservable]
simStepSF ais0 sfs0 shuffleRng = MSF $ \_ -> do
  let (sfsAis, shuffleRng') = fisherYatesShuffle shuffleRng (zip sfs0 ais0)
      (sfs, ais)            = unzip sfsAis

  res <- mapM (`unMSF` AgentIn) sfs

  let adefs = concatMap (\(ao, _) -> aoCreate ao) res
      newSfs = map adBeh adefs
      newAis = map adId adefs

      obs = foldr (\((ao, _), aid) acc -> 
        if isObservable ao 
          then (aid, fromJust $ aoObservable ao) : acc  
          else acc) [] (zip res ais)

      (sfs', ais') = foldr (\((ao, sf), aid) acc@(accSf, accAid) -> 
        if isDead ao 
          then acc 
          else (sf : accSf, aid : accAid)) ([], []) (zip res ais)

      ct = simStepSF (newAis ++ ais') (newSfs ++ sfs') shuffleRng'

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