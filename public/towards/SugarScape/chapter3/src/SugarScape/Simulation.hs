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

  , runAgentSF
  , runEnvSF
  ) where

import Data.Maybe
import qualified Data.IntMap.Strict as Map -- better performance than normal Map
import System.Random

import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict
import FRP.BearRiver

import SugarScape.Agent.Interface
import SugarScape.Environment
import SugarScape.Model
import SugarScape.Init
import SugarScape.Random

type AgentMap g = Map.IntMap (SugAgentSF g, Maybe SugAgentObservable)
type EventList  = [(AgentId, ABSEvent SugEvent)]  -- from, to, event

type AgentObservable o = (AgentId, o)
type SimStepOut        = (Time, Int, SugEnvironment, [AgentObservable SugAgentObservable])

-- NOTE: strictness on those fields, otherwise space-leak 
-- (memory consumption increases throughout run-time and execution gets slower and slower)
data SimulationState g = SimulationState 
  { simAgentMap :: !(AgentMap g)
  , simAbsState :: !ABSState 
  , simEnvState :: !SugEnvironment
  , simEnvSF    :: SugEnvironmentSF
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
    initSimState            = mkSimState agentMap (mkAbsState $ maximum initAis) initEnv (sugEnvironmentSf params) g' 0

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
        (ss', so@(t, _, _, _)) = simulationStep ss
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
    el = zip aisShuffled (repeat TimeStep)
    -- process all events
    ssSteps = processEvents el (ss0 { simRng = gShuff })
    -- run the environment
    (envFinal, envSfFinal) = runEnvSF (simEnvState ssSteps) (simEnvSF ssSteps)
    ssSteps'               = incrementTime ssSteps
    ssFinal                = ssSteps' { simEnvState = envFinal
                                      , simEnvSF    = envSfFinal }
    -- produce final output of this step
    sao = simStepOutFromSimState ssFinal

    incrementTime :: SimulationState g 
                  -> SimulationState g
    incrementTime ss = ss { simAbsState = absState' }
      where
        absState  = simAbsState ss
        absState' = absState { absTime = absTime absState + sugarScapeTimeDelta }

    simStepOutFromSimState :: SimulationState g
                           -> SimStepOut
    simStepOutFromSimState ss = (t, steps, env, aos)
      where
        t     = absTime $ simAbsState ss
        steps = simSteps ss
        env   = simEnvState ss
        aos   = foldr (\(aid, (_, mao)) acc -> maybe acc (\ao -> (aid, ao) : acc) mao) [] (Map.assocs $ simAgentMap ss)

    processEvents :: RandomGen g
                  => EventList
                  -> SimulationState g 
                  -> SimulationState g
    processEvents [] ss         = ss
    processEvents ((aid, evt) : es) ss 
        | isNothing mayAgent = processEvents es ss
        | otherwise          = processEvents es'' ss'
      where
        am       = simAgentMap ss
        absState = simAbsState ss
        env      = simEnvState ss
        g        = simRng ss
        steps    = simSteps ss

        mayAgent = Map.lookup aid am
        (asf, _) = fromJust mayAgent

        dt = case evt of
              TimeStep -> sugarScapeTimeDelta
              _        -> 0.0

        (ao, asf', absState', env', g') = runAgentSF asf evt dt absState env g

         -- schedule events of the agent: will always be put infront of the list, thus processed immediately
        es' = map (\(receiver, domEvt) -> (receiver, DomainEvent (aid, domEvt))) (aoEvents ao) ++ es

        -- TODO: process event-with-continuation
        (es'', asf'') 
          = maybe 
              (es', asf') 
              (\(receiver, domEvt, cont) -> ((receiver, DomainEvent (aid, domEvt)) : es', cont)) 
              (aoEventWCont ao)

        -- update observable
        am' = if isObservable ao
                then Map.insert aid (asf'', aoObservable ao) am
                else am

        -- agent is dead, remove from set (technically its a map) of agents
        am'' = if isDead ao
                then Map.delete aid am'
                else am'

        -- add newly created agents
        am''' = foldr (\ad acc -> Map.insert (adId ad) (adSf ad, Nothing) acc) am'' (aoCreate ao)

        ss' = ss { simAgentMap = am'''
                 , simAbsState = absState'
                 , simEnvState = env'
                 , simRng      = g'
                 , simSteps    = steps + 1 }

runEnvSF :: SugEnvironment
         -> SugEnvironmentSF
         -> (SugEnvironment, SugEnvironmentSF)
runEnvSF env envSf = runIdentity sfId
  where
    sfReader = unMSF envSf env
    sfId     = runReaderT sfReader sugarScapeTimeDelta

runAgentSF :: RandomGen g
           => SugAgentSF g
           -> ABSEvent SugEvent
           -> DTime
           -> ABSState
           -> SugEnvironment
           -> g
           -> (SugAgentOut g, SugAgentSF g, ABSState, SugEnvironment, g)
runAgentSF sf evt dt absState env g
    = (out, sf', absState', env', g') 
  where
    sfReader   = unMSF sf evt
    sfAbsState = runReaderT sfReader dt
    sfEnvState = runStateT sfAbsState absState
    sfRand     = runStateT sfEnvState env
    ((((out, sf'), absState'), env'), g') = runRand sfRand g

mkSimState :: AgentMap g
           -> ABSState
           -> SugEnvironment
           -> SugEnvironmentSF
           -> g
           -> Int
           -> SimulationState g
mkSimState am absState env envSf g steps = SimulationState 
  { simAgentMap = am
  , simAbsState = absState 
  , simEnvState = env
  , simEnvSF    = envSf
  , simRng      = g
  , simSteps    = steps
  }