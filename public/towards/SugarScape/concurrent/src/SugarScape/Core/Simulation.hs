module SugarScape.Core.Simulation
  ( SimulationState (..)
  , SimTickOut
  , AgentObservable

  , initSimulation
  , initSimulationOpt
  , initSimulationRng
  , initSimulationSeed

  , simulationTick

  , simulateUntil
  , simulateUntilLast

  , runAgentMSF
  , runEnv
  ) where

import Data.Maybe
import Control.Concurrent
import System.Random

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.Stats
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.STM
import Data.MonadicStreamFunction.InternalCore
import qualified Data.IntMap.Strict as Map -- better performance than normal Map according to hackage page

import SugarScape.Agent.Interface
import SugarScape.Core.Common
import SugarScape.Core.Environment
import SugarScape.Core.Model
import SugarScape.Core.Init
import SugarScape.Core.Random
import SugarScape.Core.Scenario

type AgentObservable o    = (AgentId, o)
type SugarScapeObservable = AgentObservable SugAgentObservable
type SimTickOut           = (Time, SugEnvironment, [SugarScapeObservable])

-- NOTE: strictness on those fields, otherwise space-leak 
-- (memory consumption increases throughout run-time and execution gets slower and slower)
data SimulationState g = SimulationState 
  { simStAbsCtx   :: !(ABSCtx SugEvent)
  , simStEnv      :: !SugEnvironment
  , simStEnvBeh   :: SugEnvBehaviour
  , simStRng      :: !g
  , simStTickVars :: [MVar DTime]
  , simStOutVars  :: [MVar (AgentId, SugAgentOut g)]
  }

-- sugarscape is stepped with a time-delta of 1.0
sugarScapeTimeDelta :: DTime
sugarScapeTimeDelta = 1

initSimulation :: SugarScapeScenario
               -> IO (SimulationState StdGen, SimTickOut, SugarScapeScenario)
initSimulation params = do
  g0 <- newStdGen
  initSimulationRng g0 params

initSimulationOpt :: Maybe Int
                  -> SugarScapeScenario
                  -> IO (SimulationState StdGen, SimTickOut, SugarScapeScenario)
initSimulationOpt Nothing     params = initSimulation params
initSimulationOpt (Just seed) params = initSimulationSeed seed params

initSimulationSeed :: Int
                   -> SugarScapeScenario
                   -> IO (SimulationState StdGen, SimTickOut, SugarScapeScenario)
initSimulationSeed seed = initSimulationRng g0
  where
    g0 = mkStdGen seed

initSimulationRng :: StdGen
                  -> SugarScapeScenario
                  -> IO (SimulationState StdGen, SimTickOut, SugarScapeScenario)
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

simulateUntil :: Time
              -> SimulationState StdGen
              -> IO [SimTickOut]
simulateUntil tMax ss0 = simulateUntilAux ss0 []
  where
    simulateUntilAux :: SimulationState StdGen
                     -> [SimTickOut]
                     -> IO [SimTickOut]
    simulateUntilAux ss acc = do
      (ss', so@(t, _, _)) <- simulationTick ss 
      let acc' = so : acc

      if t > tMax 
        then return $ reverse acc'
        else simulateUntilAux ss' acc'
      
simulateUntilLast :: Time
                  -> SimulationState StdGen
                  -> IO SimTickOut
simulateUntilLast tMax ss = do
  (ss', so@(t, _, _)) <- simulationTick ss 

  if t > tMax 
    then return so
    else simulateUntilLast tMax ss'

simulationTick :: SimulationState StdGen
               -> IO (SimulationState StdGen, SimTickOut)
simulationTick ss0 = do
    let tickVars = simStTickVars ss0
        outVars  = simStOutVars ss0
        g        = simStRng ss0

    -- agents are blocking at that point
    --putStrLn "Sending agents next Tick..."
    -- send next tick to agents, they block on their mvar => will unblock them and all agents start working
    -- NOTE: no need to shuffle, concurrency will introduce randomness endogenously ;)
    mapM_ (`putMVar` sugarScapeTimeDelta) tickVars
    
    --putStrLn "Waiting for agents signal back..."
    -- wait for agents to finish
    aos <- mapM takeMVar outVars  

    -- agents have finished at that point and are blocking again

    -- run the environment
    runEnv ss0

    -- remove tick and out vars of killed agents
    let (_, tickVars', outVars') = unzip3 $ filter (\((_, ao), _, _) -> not $ isDead ao) (zip3 aos tickVars outVars)

    -- spawn new agents
    let newAdefs = concatMap (aoCreate . snd) aos
    let (rngs, g') = rngSplits (length newAdefs) g

    (newTickVars, newOutVars, newAobs) <- unzip3 <$> zipWithM (\ad ga -> do
      let aid  = adId ad
          asf  = adSf ad
          aobs = adInitObs ad

      (tv, ov) <- createAgentThread aid asf ga (simStEnv ss0) (simStAbsCtx ss0)
      return (tv, ov, (aid, aobs))) newAdefs rngs

    -- construct output for this tick and also add new agents observables to out
    let (tOut, evtOut, aobs) = simTickOutFromSimState ss0 aos
        out' = (tOut, evtOut, aobs ++ newAobs)

    -- update changed tick and out variables and rng
    let ss = ss0 { simStTickVars = tickVars' ++ newTickVars
                 , simStOutVars  = outVars'  ++ newOutVars
                 , simStRng      = g' }
    -- increment time in simulation state
    let ss' = incrementTime ss
    return (ss', out')

  where
    incrementTime :: SimulationState g 
                  -> SimulationState g
    incrementTime ss = ss { simStAbsCtx = absCtx' }
      where
        absCtx  = simStAbsCtx ss
        absCtx' = absCtx { absCtxTime = absCtxTime absCtx + sugarScapeTimeDelta }

    simTickOutFromSimState :: SimulationState g
                           -> [(AgentId, SugAgentOut g)]
                           -> SimTickOut
    simTickOutFromSimState ss aos = (t, env, aobs)
      where
        t     = absCtxTime $ simStAbsCtx ss
        env   = simStEnv ss
        aobs   = map (\(aid, ao) -> (aid, observable ao)) aos

createAgentThread :: AgentId
                  -> SugAgentMSF StdGen
                  -> StdGen
                  -> SugEnvironment
                  -> ABSCtx SugEvent
                  -> IO (MVar DTime, MVar (AgentId, SugAgentOut StdGen))
createAgentThread aid0 asf0 g0 env ctx0 = do 
    -- mvar for synchronising upon next tick sent by main thread
    tickVar <- newEmptyMVar 
    -- mvar for sending agent-out to main thread
    outVar <- newEmptyMVar
    -- message queue of this agent for receiving messages from others
    q <- atomically newTQueue
    -- add this queue to the map of all agent message queues
    insertAgentQueue aid0 q ctx0
    -- start thread
    _ <- forkIO $ agentThread asf0 g0 tickVar outVar q
    return (tickVar, outVar)
  where
    agentThread :: SugAgentMSF StdGen
                -> StdGen
                -> MVar DTime
                -> MVar (AgentId, SugAgentOut StdGen)
                -> TQueue (ABSEvent SugEvent)
                -> IO ()
    agentThread asf g tickVar outVar q = do
      --putStrLn $ "Agent " ++ show aid0 ++ ": waiting for next Tick..."
      -- wait for main-thread to send next tick time
      t <- takeMVar tickVar 
      --putStrLn $ "Agent " ++ show aid0 ++ ": received next Tick."
      -- next tick has arrived, write it to my queue
      writeTick t q
      -- get all queues, won't change until agents are finished with this tick
      allQs <- allAgentQueues ctx0

      -- while there are messages in ANY agent check this agents
      -- messages and process them if there are any (busy waiting...)
      (mao, asf', g') <- processWhileMessages Nothing asf g q allQs
      -- because we posted a Tick message (writeTick) to our queue, we will
      -- always return a Just with an AgentOut, thus this is safe!
      let ao = fromJust mao
        
      -- signal main-thread that this thread has finished 
      -- by posting ping-pong counter
      putMVar outVar (aid0, ao)

      -- NOTE: new agents are handled by main-thread

      -- check if agent is to be removed from simulation
      if not $ isDead ao
        -- not dead, continue with work
        then agentThread asf' g' tickVar outVar q
        -- agent is dead, remove queue and terminate thread
        else removeAgentQueue aid0 ctx0

    -- TODO: busy waiting, can we do better?
    processWhileMessages :: Maybe (SugAgentOut StdGen)
                         -> SugAgentMSF StdGen
                         -> StdGen
                         -> TQueue (ABSEvent SugEvent)
                         -> [TQueue (ABSEvent SugEvent)]
                         -> IO (Maybe (SugAgentOut StdGen), SugAgentMSF StdGen, StdGen)
    processWhileMessages mao asf g q allQs = do
      ret <- allQueuesEmpty allQs
      if ret
        then return (mao, asf, g)
        else do
          (mao', asf', g') <- trackSTM $ agentProcessMessages mao asf g q
          processWhileMessages mao' asf' g' q allQs

    agentProcessMessages :: Maybe (SugAgentOut StdGen)
                         -> SugAgentMSF StdGen
                         -> StdGen
                         -> TQueue (ABSEvent SugEvent)
                         -> STM (Maybe (SugAgentOut StdGen), SugAgentMSF StdGen, StdGen)
    agentProcessMessages mao asf g q = do
      -- TODO: does it work if we check queue in here?

      -- check this agents queue
      mmsg <- tryReadTQueue q 
      case mmsg of
        -- no message, continue with checking
        Nothing  -> return (mao, asf, g)
        -- message found, run agent 
        Just evt -> do
          (ao, asf', g') <- runAgentMSF asf evt ctx0 env g
          agentProcessMessages (Just ao) asf' g' q 

    allAgentQueues :: ABSCtx SugEvent
                   -> IO [TQueue (ABSEvent SugEvent)]
    allAgentQueues ctx = do
      let msgQsVar = absCtxMsgQueues ctx
      Map.elems <$> readTVarIO msgQsVar

    insertAgentQueue :: AgentId
                     -> TQueue (ABSEvent SugEvent)
                     -> ABSCtx SugEvent
                     -> IO ()
    insertAgentQueue aid q ctx = do
      let msgQsVar = absCtxMsgQueues ctx
      trackSTM $ modifyTVar msgQsVar (Map.insert aid q)

    removeAgentQueue :: AgentId
                     -> ABSCtx SugEvent
                     -> IO ()
    removeAgentQueue aid ctx = do
      let msgQsVar = absCtxMsgQueues ctx
      trackSTM $ modifyTVar msgQsVar (Map.delete aid)

    writeTick :: DTime -> TQueue (ABSEvent SugEvent) -> IO ()
    writeTick dt q = trackSTM $ writeTQueue q (Tick dt)

    allQueuesEmpty :: [TQueue (ABSEvent SugEvent)] -> IO Bool
    allQueuesEmpty allQs 
      = trackSTM $ foldM (\flag q -> (&&) flag <$> isEmptyTQueue q) True allQs

runAgentMSF :: SugAgentMSF StdGen
            -> ABSEvent SugEvent
            -> ABSCtx SugEvent
            -> SugEnvironment
            -> StdGen
            -> STM (SugAgentOut StdGen, SugAgentMSF StdGen, StdGen)
runAgentMSF sf evt absCtx env g = do
  let sfAbsCtx   = unMSF sf evt 
      sfEnvState = runReaderT sfAbsCtx absCtx
      sfRand     = runReaderT sfEnvState env
      sfSTM      = runRandT sfRand g

  ((out, sf'), g') <- sfSTM
  return (out, sf', g') 

runEnv :: SimulationState g -> IO ()
runEnv ss = trackSTM $ envBeh t env
  where
    env    = simStEnv ss
    envBeh = simStEnvBeh ss
    t      = absCtxTime $ simStAbsCtx ss
 
mkSimState :: ABSCtx SugEvent
           -> SugEnvironment
           -> SugEnvBehaviour
           -> g
           -> [MVar DTime]
           -> [MVar (AgentId, SugAgentOut g)]
           -> SimulationState g
mkSimState absCtx env envBeh g tickVars outVars = SimulationState 
  { simStAbsCtx   = absCtx
  , simStEnv      = env
  , simStEnvBeh   = envBeh
  , simStRng      = g
  , simStTickVars = tickVars
  , simStOutVars  = outVars
  }