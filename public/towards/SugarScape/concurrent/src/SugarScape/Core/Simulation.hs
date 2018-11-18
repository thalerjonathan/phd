module SugarScape.Core.Simulation
  ( SimulationState (..)
  , SimStepOut
  , AgentObservable

  , initSimulation
  , initSimulationOpt
  , initSimulationRng
  , initSimulationSeed

  , simulationStep

  , simulateUntil
  , simulateUntilLast

  , sugarScapeTimeDelta

  , runAgentSF
  , runEnv
  ) where

import Control.Concurrent.MVar
import System.Random

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.STM
import Data.MonadicStreamFunction.InternalCore

import SugarScape.Core.Common
import SugarScape.Core.Environment
import SugarScape.Core.Model
import SugarScape.Core.Init
import SugarScape.Core.Random
import SugarScape.Core.Scenario

type AgentObservable o    = (AgentId, o)
type SugarScapeObservable = AgentObservable SugAgentObservable
type SimStepOut           = (Time, SugEnvironment, [SugarScapeObservable])

-- NOTE: strictness on those fields, otherwise space-leak 
-- (memory consumption increases throughout run-time and execution gets slower and slower)
data SimulationState g = SimulationState 
  { simStAbsCtx   :: !(ABSCtx SugEvent)
  , simStEnv      :: !SugEnvironment
  , simStEnvBeh   :: SugEnvBehaviour
  , simStRng      :: !g
  , simStTickVars :: [MVar DTime]
  , simStOutVars  :: [MVar SugarScapeObservable]
  }

-- sugarscape is stepped with a time-delta of 1.0
sugarScapeTimeDelta :: DTime
sugarScapeTimeDelta = 1

initSimulation :: SugarScapeScenario
               -> IO (SimulationState StdGen, SimStepOut, SugarScapeScenario)
initSimulation params = do
  g0 <- newStdGen
  initSimulationRng g0 params

initSimulationOpt :: Maybe Int
                  -> SugarScapeScenario
                  -> IO (SimulationState StdGen, SimStepOut, SugarScapeScenario)
initSimulationOpt Nothing     params = initSimulation params
initSimulationOpt (Just seed) params = initSimulationSeed seed params

initSimulationSeed :: Int
                   -> SugarScapeScenario
                   -> IO (SimulationState StdGen, SimStepOut, SugarScapeScenario)
initSimulationSeed seed = initSimulationRng g0
  where
    g0 = mkStdGen seed

initSimulationRng :: StdGen
                  -> SugarScapeScenario
                  -> IO (SimulationState StdGen, SimStepOut, SugarScapeScenario)
initSimulationRng g0 params = do
  -- initial agents and environment data
  ((initAs, initEnv, params'), g) <- atomically $ runRandT (createSugarScape params) g0

  let (rngs, g')            = rngSplits (length initAs) g
      (initAis, initObs, _) = unzip3 initAs
      initOut               = (0, initEnv, zip initAis initObs)
      initEnvBeh            = sugEnvBehaviour params

  absCtx <- atomically $ mkAbsCtx $ maximum initAis

  (tickVars, outVars) <- unzip <$> zipWithM (\(aid, _, asf) ga -> 
              createAgentThread aid asf ga initEnv absCtx) initAs rngs

  let initSimState = mkSimState absCtx initEnv initEnvBeh g' tickVars outVars

  return (initSimState, initOut, params')

createAgentThread :: RandomGen g
                  => AgentId
                  -> SugAgentMSF g
                  -> StdGen
                  -> SugEnvironment
                  -> ABSCtx SugEvent
                  -> IO (MVar DTime, MVar SugarScapeObservable)
createAgentThread _aid _asf _g _env _ctx = do 
  tickVar <- newEmptyMVar
  outVar  <- newEmptyMVar

  return (tickVar, outVar)

simulateUntil :: RandomGen g
              => Time
              -> SimulationState g
              -> IO [SimStepOut]
simulateUntil tMax ss0 = simulateUntilAux ss0 []
  where
    simulateUntilAux :: RandomGen g
                     => SimulationState g
                     -> [SimStepOut]
                     -> IO [SimStepOut]
    simulateUntilAux ss acc = do
      (ss', so@(t, _, _)) <- simulationStep ss 
      let acc' = so : acc

      if t > tMax 
        then return $ reverse acc'
        else simulateUntilAux ss' acc'
      
simulateUntilLast :: RandomGen g
                  => Time
                  -> SimulationState g
                  -> IO SimStepOut
simulateUntilLast tMax ss = do
  (ss', so@(t, _, _)) <- simulationStep ss 

  if t > tMax 
    then return so
    else simulateUntilLast tMax ss'

simulationStep :: RandomGen g
               => SimulationState g
               -> IO (SimulationState g, SimStepOut)
simulationStep _ss0 = undefined {- (ssFinal, sao)
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
-}

runEnv :: SimulationState g -> IO ()
runEnv ss = atomically $ envBeh t env
  where
    env    = simStEnv ss
    envBeh = simStEnvBeh ss
    t      = absCtxTime $ simStAbsCtx ss

runAgentSF :: RandomGen g
           => SugAgentMSF g
           -> ABSEvent SugEvent
           -> ABSCtx SugEvent
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
 
mkSimState :: ABSCtx SugEvent
           -> SugEnvironment
           -> SugEnvBehaviour
           -> g
           -> [MVar DTime]
           -> [MVar SugarScapeObservable]
           -> SimulationState g
mkSimState absCtx env envBeh g tickVars outVars = SimulationState 
  { simStAbsCtx   = absCtx
  , simStEnv      = env
  , simStEnvBeh   = envBeh
  , simStRng      = g
  , simStTickVars = tickVars
  , simStOutVars  = outVars
  }