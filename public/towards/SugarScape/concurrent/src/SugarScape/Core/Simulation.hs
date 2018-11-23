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
import Control.Concurrent.STM.TMVar
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
import SugarScape.Core.Utils 

import Debug.Trace as DBG

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
  , simStOutVars  :: ![MVar (AgentId, SugAgentOut g)]
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

initSimulationRng :: RandomGen g
                  => g
                  -> SugarScapeScenario
                  -> IO (SimulationState g, SimTickOut, SugarScapeScenario)
initSimulationRng g0 params = do
  -- initial agents and environment data
  ((initAs, initEnv, params'), g) <- executeSTM $ runRandT (createSugarScape params) g0

  let (rngs, g')            = rngSplits (length initAs) g
      (initAis, initObs, _) = unzip3 initAs
      initOut               = (0, initEnv, zip initAis initObs)
      initEnvBeh            = sugEnvBehaviour params

  absCtx <- executeSTM $ mkAbsCtx $ maximum initAis

  outVars <- zipWithM (\(aid, _, asf) ga -> 
              createAgentThread aid asf ga initEnv absCtx) initAs rngs

  let initSimState = mkSimState absCtx initEnv initEnvBeh g' outVars

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
    let outVars  = simStOutVars ss0
        g        = simStRng ss0
        ctx      = simStAbsCtx ss0

    allQs <- allAgentQueues ctx

    -- agents are blocking at that point because of a retry on their queue

    -- insert TickStart into each agents queue => will unblock them and all agents start working
    -- NOTE: no need to shuffle, concurrency will introduce randomness endogenously ;)
    mapM_ (writeTickStart sugarScapeTimeDelta) allQs

    -- wait until all agents have consumed their messages, which will
    -- leave them blocking on their queue because of retry
    -- TODO: this busy waiting causes a lot of retries, can we do it in a more clever way without causing (that many) retries?
    whileM (notM $ allQueuesEmpty allQs) (return ())

    -- notify agents that this tick has finished: send TickEnd which will
    -- unblock them all and terminate their message-checking function
    mapM_ writeTickEnd allQs

    -- after TickEnd each agent will signal its output back to the main thread
    aos <- mapM takeMVar outVars  

    -- agents have finished at that point and are blocking again

    -- run the environment
    runEnv ss0

    -- remove tick and out vars and queue of killed agents
    outVars' <- foldM (\acc ((aid, ao), ov) -> 
                        if not $ isDead ao
                          then return (ov : acc)
                          else do
                            removeAgentQueue aid ctx
                            return acc) [] (zip aos outVars)

    -- spawn new agents
    let newAdefs   = concatMap (aoCreate . snd) aos
        (rngs, g') = rngSplits (length newAdefs) g

    (newOutVars, newAobs) <- unzip <$> zipWithM (\ad ga -> do
      let aid  = adId ad
          asf  = adSf ad
          aobs = adInitObs ad

      ov <- createAgentThread aid asf ga (simStEnv ss0) ctx
      return (ov, (aid, aobs))) newAdefs rngs

    -- construct output for this tick and also add new agents observables to out
    let (tOut, evtOut, aobs) = simTickOutFromSimState ss0 aos
        out' = (tOut, evtOut, aobs ++ newAobs)

    -- update changed tick and out variables and rng
    let ss = ss0 { simStOutVars  = outVars'  ++ newOutVars
                 , simStRng      = g' }
    -- increment time in simulation state
    let ss' = incrementTime ss

    return (ss', out')

  where
    allAgentQueues :: ABSCtx SugEvent
                   -> IO [TQueue (ABSEvent SugEvent)]
    allAgentQueues ctx = do
      let msgQsVar = absCtxMsgQueues ctx
      Map.elems <$> readTVarIO msgQsVar

    writeTickStart :: DTime -> TQueue (ABSEvent SugEvent) -> IO ()
    writeTickStart dt q = executeSTM $ writeTQueue q (TickStart dt)
    
    writeTickEnd :: TQueue (ABSEvent SugEvent) -> IO ()
    writeTickEnd q = executeSTM $ writeTQueue q TickEnd
    
    allQueuesEmpty :: [TQueue (ABSEvent SugEvent)] -> IO Bool
    allQueuesEmpty allQs 
      = executeSTM $ foldM (\flag q -> (&&) flag <$> isEmptyTQueue q) True allQs

    removeAgentQueue :: AgentId
                     -> ABSCtx SugEvent
                     -> IO ()
    removeAgentQueue aid ctx = do
      let msgQsVar = absCtxMsgQueues ctx
      executeSTM $ modifyTVar msgQsVar (Map.delete aid)

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
        aobs  = map (\(aid, ao) -> (aid, observable ao)) aos

executeSTM :: STM a -> IO a
executeSTM = trackSTM -- atomically

createAgentThread :: RandomGen g
                  => AgentId
                  -> SugAgentMSF g
                  -> g
                  -> SugEnvironment
                  -> ABSCtx SugEvent
                  -> IO (MVar (AgentId, SugAgentOut g))
createAgentThread aid0 asf0 g0 env ctx0 = do 
    -- mvar for sending agent-out to main thread
    outVar <- newEmptyMVar
    -- message queue of this agent for receiving messages from others (and main thread: TickStart / TickEnd)
    q <- executeSTM newTQueue
    -- add this queue to the map of all agent message queues
    insertAgentQueue aid0 q ctx0
    -- start thread
    _ <- forkIO $ agentThread asf0 g0 outVar q
    return outVar
  where
    agentThread :: RandomGen g
                => SugAgentMSF g
                -> g
                -> MVar (AgentId, SugAgentOut g)
                -> TQueue (ABSEvent SugEvent)
                -> IO ()
    agentThread asf g outVar q = do
      -- wait for main-thread to write TickStart into queue

      -- process messages while there are and wait for messages 
      (mao, asf', g') <- processWhileMessages Nothing asf g q Nothing
      -- because we posted a Tick message (writeTick) to our queue, we will
      -- always return a Just with an AgentOut, thus this is safe!
      let ao = fromJust mao

      -- signal main-thread that this thread has finished 
      -- by posting ping-pong counter
      putMVar outVar (aid0, ao)

      -- NOTE: new agents are handled by main thread

      -- check if agent is to be removed from simulation
      unless 
        (isDead ao)
        -- not dead, continue with work
        (agentThread asf' g' outVar q)
        
      -- agent is dead, terminate thread, removal of agent queue is handled by main thread

    processWhileMessages :: RandomGen g
                         => Maybe (SugAgentOut g)
                         -> SugAgentMSF g
                         -> g
                         -> TQueue (ABSEvent SugEvent)
                         -> Maybe (SugReplyChannel, SugReplyChannel)
                         -> IO (Maybe (SugAgentOut g), SugAgentMSF g, g)
    processWhileMessages mao asf g q mics = do
      --putStrLn $ "Agent " ++ show aid0 ++ ": processWhileMessages" 
      ret <- executeSTM $ agentProcessNextMessage asf g q mics
      case ret of 
        Nothing -> return (mao, asf, g)
        Just (ao, asf', g') -> do
          let mics' = aoInteractCh ao
          processWhileMessages (Just ao) asf' g' q mics'

    agentProcessNextMessage :: RandomGen g
                            => SugAgentMSF g
                            -> g
                            -> TQueue (ABSEvent SugEvent)
                            -> Maybe (SugReplyChannel, SugReplyChannel)
                            -> STM (Maybe (SugAgentOut g, SugAgentMSF g, g))
    agentProcessNextMessage asf g q Nothing = do
      -- wait for next message, will block with retry when no message there
      evt <- DBG.trace ("Agent " ++ show aid0 ++ ": checking message-queue for next message..") (readTQueue q)
      case evt of
        TickEnd -> -- DBG.trace ("Agent " ++ show aid0 ++ ": received TickEnd, finished with this tick") 
                    return Nothing -- finished, no output
        _       -> do
          (ao, asf', g') <- DBG.trace ("Agent " ++ show aid0 ++ ": got message on message-queue " ++ show evt) (runAgentMSF asf evt ctx0 env g)
          return $ Just (ao, asf', g')

    agentProcessNextMessage asf g _ (Just (receiveCh, replyCh)) = do
      -- wait for next message, will block with retry when no message there
      (senderId, evt) <- DBG.trace ("Agent " ++ show aid0 ++ ": checking receiving channel for next message..") (takeTMVar receiveCh)
      let absEvt = Reply senderId evt receiveCh replyCh
      (ao, asf', g') <- DBG.trace ("Agent " ++ show aid0 ++ ": got message on receiving channel " ++ show evt) (runAgentMSF asf absEvt ctx0 env g)
      return $ Just (ao, asf', g')

    insertAgentQueue :: AgentId
                     -> TQueue (ABSEvent SugEvent)
                     -> ABSCtx SugEvent
                     -> IO ()
    insertAgentQueue aid q ctx = do
      let msgQsVar = absCtxMsgQueues ctx
      executeSTM $ modifyTVar msgQsVar (Map.insert aid q)

runAgentMSF :: RandomGen g
            => SugAgentMSF g
            -> ABSEvent SugEvent
            -> ABSCtx SugEvent
            -> SugEnvironment
            -> g
            -> STM (SugAgentOut g, SugAgentMSF g, g)
runAgentMSF sf evt absCtx env g = do
  let sfAbsCtx   = unMSF sf evt 
      sfEnvState = runReaderT sfAbsCtx absCtx
      sfRand     = runReaderT sfEnvState env
      sfSTM      = runRandT sfRand g

  ((out, sf'), g') <- sfSTM
  return (out, sf', g') 

runEnv :: SimulationState g -> IO ()
runEnv ss = executeSTM $ envBeh t env
  where
    env    = simStEnv ss
    envBeh = simStEnvBeh ss
    t      = absCtxTime $ simStAbsCtx ss
 
mkSimState :: ABSCtx SugEvent
           -> SugEnvironment
           -> SugEnvBehaviour
           -> g
           -> [MVar (AgentId, SugAgentOut g)]
           -> SimulationState g
mkSimState absCtx env envBeh g outVars = SimulationState 
  { simStAbsCtx   = absCtx
  , simStEnv      = env
  , simStEnvBeh   = envBeh
  , simStRng      = g
  , simStOutVars  = outVars
  }