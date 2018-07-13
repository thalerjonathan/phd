module Simulation
  (
    SimulationState (..)
  , SimStepOut

  , simulationStep
  , simStepSF

  , mkSimState
  , checkTime
  ) where

import            Data.Maybe
import            System.Random

import            Control.Monad.Random
import            Control.Monad.Reader
import            Control.Monad.State.Strict
import            Data.Time.Clock
import            FRP.BearRiver

import            AgentMonad
import            Model
import            Renderer

type SimStepOut = (Time, SugEnvironment, [AgentObservable SugAgentObservable])

data SimulationState g = SimulationState 
  { simSf       :: SF (SugAgentMonadT g) () [AgentObservable SugAgentObservable]
  , simAbsState :: ABSState 
  , simEnv      :: SugEnvironment
  , simRng      :: g
  , simStart    :: UTCTime
  , simSteps    :: Int
  }

simulationStep :: RandomGen g
               => DTime
               -> SimulationState g
               -> IO (SimulationState g, SimStepOut)
simulationStep dt ss = do
  let sf       = simSf ss
      absState = simAbsState ss
      env      = simEnv ss
      g        = simRng ss
      start    = simStart ss
      steps    = simSteps ss

  let sfReader   = unMSF sf ()
      sfAbsState = runReaderT sfReader dt
      sfEnvState = runStateT sfAbsState absState
      sfRand     = runStateT sfEnvState env
      ((((out, sf'), absState'), env'), g') = runRand sfRand g

  let t          = absTime absState 
      absState'' = absState' { absTime = t + dt }

  let ss'        = mkSimState sf' absState'' env' g' start (steps + 1)

  return (ss', (t, env', out))

simStepSF :: RandomGen g
          => [AgentId]
          -> [SugAgent g]
          -> SF (SugAgentMonadT g) () [AgentObservable SugAgentObservable]
simStepSF ais sfs = MSF $ \_ -> do
  -- TODO: shuffle agent sfs
  
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

      ct = simStepSF (newAis ++ ais') (newSfs ++ sfs')

  return (obs, ct)

mkSimState :: RandomGen g
           => SF (SugAgentMonadT g) () [AgentObservable SugAgentObservable]
           -> ABSState
           -> SugEnvironment
           -> g
           -> UTCTime
           -> Int
           -> SimulationState g
mkSimState sf absState env g start steps = SimulationState 
  { simSf       = sf
  , simAbsState = absState 
  , simEnv      = env
  , simRng      = g
  , simStart    = start
  , simSteps    = steps
  }

checkTime :: RandomGen g
          => Double
          -> SimulationState g
          -> IO Bool
checkTime durSecs ss = do
  nowT <- getCurrentTime

  let start = simStart ss
  let dtStart = realToFrac $ diffUTCTime nowT start

  if dtStart > durSecs
    then (do 
      let steps      = simSteps ss
          stepsRatio = (fromIntegral steps / durSecs) :: Double

      putStrLn $ show steps ++ " steps after " ++ show durSecs ++ " sec. is a ratio of " ++ show stepsRatio
      return True)
    else (do
      let secsLeft = durSecs - dtStart

      putStrLn $ show secsLeft ++ " secs left..."
      return False)