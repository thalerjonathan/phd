module SugarScape.Simulation
  ( SimulationState (..)
  , SimStepOut
  , AgentObservable

  , initSimulation
  , initSimulationOpt
  , initSimulationRng
  , initSimulationSeed

  , simulationStep

  , mkSimState
  , simulateUntil

  , sugarScapeTimeDelta
  ) where

import Data.Maybe
import qualified Data.IntMap.Strict as Map -- better performance than normal Map
import System.Random

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict
import FRP.BearRiver

import SugarScape.AgentMonad
import SugarScape.Model
import SugarScape.Init
import SugarScape.Random

type AgentMap g = Map.IntMap (SugAgent g, Maybe (AgentObservable SugAgentObservable))
type EventList  = [(AgentId, ABSEvent SugEvent)]

type AgentObservable o = (AgentId, o)
type SimStepOut        = (Time, SugEnvironment, [AgentObservable SugAgentObservable])

-- NOTE: strictness on those fields, otherwise space-leak 
-- (memory consumption increases throughout run-time and execution gets slower and slower)
data SimulationState g = SimulationState 
  { simAgentMap :: !(AgentMap g)
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
    -- initial agents and environment data
    ((initAs, initEnv), g') = runRand (createSugarScape params) g0
    -- initial agent map
    agentMap                = foldr (\(aid, asf) am' -> Map.insert aid (asf, Nothing) am') Map.empty initAs
    (initAis, _)            = unzip initAs
    -- initial simulation state
    initSimState            = mkSimState agentMap (mkAbsState $ maximum initAis) initEnv g' 0

simulateUntil :: RandomGen g
              => Time
              -> SimulationState g
              -> [SimStepOut]
simulateUntil tMax ss0 = simulateUntilAux ss0 []
  where
    simulateUntilAux :: RandomGen g
                     => SimulationState g
                     -> [SimStepOut]
                     -> [SimStepOut]
    simulateUntilAux ss acc
        | t < tMax  = simulateUntilAux ss' acc'
        | otherwise = reverse acc'
      where
        (ss', so@(t, _, _)) = simulationStep ss
        acc' = so : acc

simulationStep :: RandomGen g
               => SimulationState g
               -> (SimulationState g, SimStepOut)
simulationStep ss0 = (ssFinal, sao)
  where
    am0  = simAgentMap ss0
    ais0 = Map.keys am0
    g0   = simRng ss0
    (aisShuffled, gShuff) = fisherYatesShuffle g0 ais0
    -- schedule TimeStep messages in random order by generating event-list from shuffled agent-ids
    el  = zip aisShuffled (repeat TimeStep)

    ssFinal = simulationStepAux el (ss0 { simRng = gShuff })
    sao     = simStepOutFromSimState ssFinal

    simStepOutFromSimState :: SimulationState g
                           -> SimStepOut
    simStepOutFromSimState ss = (t, env, aos)
      where
        t   = absTime $ simAbsState ss
        env = simEnv ss
        aos = mapMaybe snd (Map.elems $ simAgentMap ss)

    simulationStepAux :: EventList
                      -> SimulationState g 
                      -> SimulationState g
    simulationStepAux [] ss         = ss --(ss', (t, env', out))
    simulationStepAux ((aid, evt) : es) ss 
        | isNothing mayAgent = simulationStepAux es ss
        | otherwise          = simulationStepAux es ss'
      where
        am       = simAgentMap ss
        absState = simAbsState ss
        env      = simEnv ss
        g        = simRng ss
        steps    = simSteps ss

        mayAgent = Map.lookup aid am
        (asf, _ao) = fromJust mayAgent

        dt = case evt of
              TimeStep -> sugarScapeTimeDelta
              _        -> 0.0

        sfReader   = unMSF asf evt
        sfAbsState = runReaderT sfReader dt
        sfEnvState = runStateT sfAbsState absState
        sfRand     = runStateT sfEnvState env
        ((((ao', asf'), absState'), env'), g') = runRand sfRand g

        -- TODO: update am: delete agent or update agentout in case of TimeStep event
        -- TODO: schedule new events

        am' = Map.insert aid (asf', aoObservable ao') am

        t          = absTime absState 
        absState'' = absState' { absTime = t + dt }
        ss'        = mkSimState am' absState'' env' g' (steps + 1)

mkSimState :: AgentMap g
           -> ABSState
           -> SugEnvironment
           -> g
           -> Int
           -> SimulationState g
mkSimState am absState env g steps = SimulationState 
  { simAgentMap = am
  , simAbsState = absState 
  , simEnv      = env
  , simRng      = g
  , simSteps    = steps
  }

{-
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
-}

