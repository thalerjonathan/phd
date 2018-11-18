module SugarScape.Core.Simulation
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
  , simulateUntilLast

  , sugarScapeTimeDelta

  , runAgentSF
  , runEnv
  ) where

import Data.Maybe
import System.Random

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.State.Strict
import Data.MonadicStreamFunction.InternalCore
import qualified Data.IntMap.Strict as Map -- better performance than normal Map according to hackage page

import SugarScape.Agent.Interface
import SugarScape.Core.Common
import SugarScape.Core.Environment
import SugarScape.Core.Model
import SugarScape.Core.Init
import SugarScape.Core.Random
import SugarScape.Core.Scenario

type AgentMap g = Map.IntMap (SugAgentMSF g, SugAgentObservable)
type EventList  = [(AgentId, ABSEvent SugEvent)]  -- from, to, event

type AgentObservable o = (AgentId, o)
type SimStepOut        = (Time, Int, SugEnvironment, [AgentObservable SugAgentObservable])

-- NOTE: strictness on those fields, otherwise space-leak 
-- (memory consumption increases throughout run-time and execution gets slower and slower)
data SimulationState g = SimulationState 
  { simAgentMap :: !(AgentMap g)
  , simAbsCtx   :: !ABSCtx 
  , simEnvState :: !SugEnvironment
  , simEnvBeh   :: SugEnvBehaviour
  , simRng      :: !g
  , simSteps    :: !Int
  }

-- sugarscape is stepped with a time-delta of 1.0
sugarScapeTimeDelta :: DTime
sugarScapeTimeDelta = 1

initSimulation :: SugarScapeScenario
               -> IO (SimulationState StdGen, SugEnvironment, SugarScapeScenario)
initSimulation params = do
  g0 <- newStdGen
  return $ initSimulationRng g0 params

initSimulationOpt :: Maybe Int
                  -> SugarScapeScenario
                  -> IO (SimulationState StdGen, SugEnvironment, SugarScapeScenario)
initSimulationOpt Nothing     params = initSimulation params
initSimulationOpt (Just seed) params = return $ initSimulationSeed seed params

initSimulationSeed :: Int
                   -> SugarScapeScenario
                   -> (SimulationState StdGen, SugEnvironment, SugarScapeScenario)
initSimulationSeed seed = initSimulationRng g0
  where
    g0 = mkStdGen seed

initSimulationRng :: RandomGen g
                  => g
                  -> SugarScapeScenario
                  -> (SimulationState g, SugEnvironment, SugarScapeScenario)
initSimulationRng g0 params = (initSimState, initEnv, params')
  where
    -- initial agents and environment data
    ((initAs, initEnv, params'), g') = runRand (createSugarScape params) g0
    -- initial agent map
    agentMap        = foldr (\(aid, obs, asf) am' -> Map.insert aid (asf, obs) am') Map.empty initAs
    (initAis, _, _) = unzip3 initAs
    -- initial simulation state
    initSimState = mkSimState agentMap (mkabsCtx $ maximum initAis) initEnv (sugEnvBehaviour params') g' 0

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

simulateUntilLast :: RandomGen g
                  => Time
                  -> SimulationState g
                  -> SimStepOut
simulateUntilLast tMax ss
    | t < tMax  = simulateUntilLast tMax ss'
    | otherwise = so
  where
    (ss', so@(t, _, _, _)) = simulationStep ss

simulationStep :: RandomGen g
               => SimulationState g
               -> (SimulationState g, SimStepOut)
simulationStep ss0 = (ssFinal, sao)
  where
    am0  = simAgentMap ss0
    ais0 = Map.keys am0
    g0   = simRng ss0
    (aisShuffled, gShuff) = fisherYatesShuffle g0 ais0
    -- schedule Tick messages in random order by generating event-list from shuffled agent-ids
    el = zip aisShuffled (repeat Tick)
    -- process all events
    ssSteps = processEvents el (ss0 { simRng = gShuff })
    -- run the environment
    envFinal = runEnv ssSteps
    ssSteps' = incrementTime ssSteps
    ssFinal  = ssSteps' { simEnvState = envFinal }

    -- produce final output of this step
    sao = simStepOutFromSimState ssFinal

    incrementTime :: SimulationState g 
                  -> SimulationState g
    incrementTime ss = ss { simAbsCtx = absCtx' }
      where
        absCtx  = simAbsCtx ss
        absCtx' = absCtx { absCtxTime = absCtxTime absCtx + sugarScapeTimeDelta }

    simStepOutFromSimState :: SimulationState g
                           -> SimStepOut
    simStepOutFromSimState ss = (t, steps, env, aos)
      where
        t     = absTime $ simAbsCtx ss
        steps = simSteps ss
        env   = simEnvState ss
        aos   = map (\(aid, (_, ao)) -> (aid, ao)) (Map.assocs $ simAgentMap ss)

    processEvents :: RandomGen g
                  => EventList
                  -> SimulationState g 
                  -> SimulationState g
    processEvents [] ss         = ss
    processEvents ((aid, evt) : es) ss 
        | isNothing mayAgent = processEvents es ss
        | otherwise          = processEvents es' ss'
      where
        am       = simAgentMap ss
        absCtx = simAbsCtx ss
        env      = simEnvState ss
        g        = simRng ss
        steps    = simSteps ss

        mayAgent = Map.lookup aid am
        (asf, _) = fromJust mayAgent
        
        (ao, asf', absCtx', env', g') = runAgentSF asf evt absCtx env g

        -- schedule events of the agent: will always be put infront of the list, thus processed immediately
        -- QUESTION: should an agent who isDead schedule events? ANSWER: yes, general solution
        es' = map (\(receiver, domEvt) -> (receiver, DomainEvent (aid, domEvt))) (aoEvents ao) ++ es

        -- update new signalfunction and agent-observable
        am' = Map.insert aid (asf', aoObservable ao) am

        -- agent is dead, remove from set (technically its a map) of agents
        am'' = if isDead ao
                then Map.delete aid am'
                else am'

        -- add newly created agents
        -- QUESTION: should an agent who isDead be allowed to create new ones? 
        -- ANSWER: depends on the model, we leave that to the model implementer to have a general solution
        --         in case of SugarScape when an agent dies it wont engage in mating, which is prevented
        --         in the agent implementation thus aoCreate will always be null in case of a isDead agent
        --         NOTE the exception (there is always an exception) is when the R (replacement) rule is active
        --              which replaces a dead agent by a random new-born, then aoCreate contains exactly 1 element
        am''' = foldr (\ad acc -> Map.insert (adId ad) (adSf ad, adInitObs ad) acc) am'' (aoCreate ao)

        ss' = ss { simAgentMap = am'''
                 , simAbsCtx   = absCtx'
                 , simEnvState = env'
                 , simRng      = g'
                 , simSteps    = steps + 1 }

runEnv :: SimulationState g
       -> SugEnvironment
runEnv ss = eb t env
  where
    eb  = simEnvBeh ss 
    env = simEnvState ss
    t   = absTime $ simabsCtx ss

runAgentSF :: RandomGen g
           => SugAgentMSF g
           -> ABSEvent SugEvent
           -> ABSCtx
           -> SugEnvironment
           -> g
           -> IO (SugAgentOut g, SugAgentMSF g, g)
runAgentSF sf evt absCtx env g = do
    let sfAbsCtx   = unMSF sf evt 
        sfEnvState = runReaderT sfAbsCtx absCtx
        sfRand     = runReaderT sfEnvState env
        sfSTM      = runRandT sfRand g

    ((out, sf'), g') <- atomically sfSTM
    return (out, sf', g') 
 
mkSimState :: AgentMap g
           -> ABSCtx
           -> SugEnvironment
           -> SugEnvBehaviour
           -> g
           -> Int
           -> SimulationState g
mkSimState am absCtx env eb g steps = SimulationState 
  { simAgentMap = am
  , simAbsCtx   = absCtx 
  , simEnvState = env
  , simEnvBeh   = eb
  , simRng      = g
  , simSteps    = steps
  }