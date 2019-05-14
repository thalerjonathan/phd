{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module SIR.Impure
  ( runImpureSIR
  ) where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer

import Data.MonadicStreamFunction.InternalCore
import qualified Data.IntMap.Strict as Map 
import qualified Data.PQueue.Min as PQ

import SIR.Agent
import SIR.API
import SIR.Model

data QueueItem e  = QueueItem !AgentId !(Event e) !Time deriving Show
type EventQueue e = PQ.MinQueue (QueueItem e)

instance Eq (QueueItem e) where
  (==) (QueueItem _ _ t1) (QueueItem _ _ t2) = t1 == t2

instance Ord (QueueItem e) where
  compare (QueueItem _ _ t1) (QueueItem _ _ t2) = compare t1 t2

type SIRAgent          = MSF SIRAgentImpure SIREvent SIRState
type SIRAgentImpureMap = Map.IntMap (SIRAgent, SIRState)

newtype SimState = SimState { simStateAgents :: SIRAgentImpureMap }

runImpureSIR :: [SIRState]
             -> Int
             -> Double
             -> Double
             -> Int
             -> Double
             -> IO [(Time, (Int, Int, Int))]
runImpureSIR as cor inf ild maxEvents tLimit = do
  (ss0, eq) <- initSIRImpure as cor inf ild

  let ais  = Map.keys (simStateAgents ss0)
      sir0 = aggregateAgentMap (simStateAgents ss0)

  reverse <$> processQueue maxEvents tLimit ss0 eq ais [(0, sir0)]

aggregateAgentMap :: SIRAgentImpureMap -> (Int, Int, Int)
aggregateAgentMap = Prelude.foldr aggregateAgentMapAux (0,0,0)
  where
    aggregateAgentMapAux :: (MSF m SIREvent SIRState, SIRState)
                         -> (Int, Int, Int)
                         -> (Int, Int, Int)
    aggregateAgentMapAux (_, Susceptible) (s,i,r) = (s+1,i,r)
    aggregateAgentMapAux (_, Infected) (s,i,r)    = (s,i+1,r)
    aggregateAgentMapAux (_, Recovered) (s,i,r)   = (s,i,r+1)

initSIRImpure :: [SIRState]
              -> Int
              -> Double
              -> Double 
              -> IO (SimState, EventQueue SIREvent)
initSIRImpure as cor inf ild = do
    -- NOTE: sendSync does not work in initialisation sirAgent function (and
    -- also not required and would also violate semantics of the simulation as
    -- the simulation is not running yet and a send event would not make sense)
    -- Thus we use an empty map and ignore the returned (empty) map in
    -- createAgent. If an agent performs sendSync in the initialisation phase,
    -- it will always result in a Nothing because it 

    (amsfs, ess0) <- unzip <$> mapM createAgent (zip ais as)
        
    let es0  = concat ess0
        am   = Prelude.foldr 
                        (\(aid, a, s) acc -> Map.insert aid (a, s) acc) 
                        Map.empty 
                        (Prelude.zip3 ais amsfs as)
        eq    = foldr PQ.insert PQ.empty es0
        ss0' = ss0 { simStateAgents = am }

    return (ss0', eq) 
  where
    ais = [0..length as - 1]
    ss0 = SimState { simStateAgents = Map.empty }

    createAgent :: (AgentId, SIRState) -> IO (SIRAgent, [QueueItem SIREvent])
    createAgent (ai, s) = do
      (a, es, _) <- runSIRAgentImpure 0 ai ais ss0 (sirAgent cor inf ild s)
      return (a, es)

processQueue :: Int 
             -> Double
             -> SimState
             -> EventQueue SIREvent
             -> [AgentId]
             -> [(Time, (Int, Int, Int))]
             -> IO [(Time, (Int, Int, Int))]
processQueue 0 _ _ _ _ acc = return acc -- terminated by externals of simulation: hit event limit
processQueue n tLimit ss q ais acc 
    -- terminated by internals of simulation model: no more events
    | isNothing mayHead = return acc 
    -- terminated by externals of simulation: hit time limit
    | evtTime > tLimit = return acc 
    -- neither one, process next event
    | otherwise = do
      retMay <- processEvent ss ais evt
      case retMay of 
        -- event-receiver not found, next event
        Nothing -> processQueue (n-1) tLimit ss q' ais acc
        -- event receiver found
        (Just (ss', es)) -> do
          -- insert new events into queue
          let q'' = foldr PQ.insert q' es
              -- sample domain-state for current event
              (tPre, sirPre) = head acc
              sir = aggregateAgentMap (simStateAgents ss')

              acc' = if evtTime == tPre || sirPre == sir
                      then (evtTime, sir) : tail acc
                      else (evtTime, sir) : acc

          processQueue (n-1) tLimit ss' q'' ais acc'
  where
    mayHead = PQ.getMin q
    evt     = fromJust mayHead
    evtTime = eventTime evt
    q'      = PQ.drop 1 q

    eventTime :: QueueItem e -> Time
    eventTime (QueueItem _ _ et) = et

processEvent :: SimState
             -> [AgentId]
             -> QueueItem SIREvent
             -> IO (Maybe (SimState, [QueueItem SIREvent]))
processEvent ss ais (QueueItem receiverId (Event e) evtTime) 
    | isNothing receiverMay = return Nothing
    | otherwise = do
      let agentAct = unMSF receiver e
      ((ao, a'), es, ss') <- runSIRAgentImpure evtTime receiverId ais ss agentAct
      let ss'' = ss' { simStateAgents = Map.insert receiverId (a', ao) (simStateAgents ss') }
      return $ Just (ss'', es)
  where
    receiverMay  = Map.lookup receiverId (simStateAgents ss)
    (receiver,_) = fromJust receiverMay

--------------------------------------------------------------------------------
-- INTERPRETER
--------------------------------------------------------------------------------
runSIRAgentImpure :: Time
                  -> AgentId
                  -> [AgentId]
                  -> SimState
                  -> SIRAgentImpure a
                  -> IO (a, [QueueItem SIREvent], SimState)
runSIRAgentImpure t ai ais ss agentAct = do
    ((ret, es), ss') <- actIO
    return (ret, es, ss')
  where
    actEvtWriter = runReaderT (unSIRAgentImpure agentAct) (t, ai, ais)
    actState     = runWriterT actEvtWriter
    actIO        = runStateT actState ss

newtype SIRAgentImpure a = SIRAgentImpure 
  { unSIRAgentImpure :: ReaderT (Time, AgentId, [AgentId]) 
                        (WriterT [QueueItem SIREvent]
                          (StateT SimState IO)) a}
  deriving (Functor, Applicative, Monad, MonadIO,
            MonadWriter [QueueItem SIREvent],
            MonadReader (Time, AgentId, [AgentId]),
            MonadState SimState)

instance MonadAgent SIREvent SIRAgentImpure where
  randomBool = liftIO . evalRandIO . randomBoolM
  randomElem = liftIO . evalRandIO . randomElemM
  randomExp  = liftIO . evalRandIO . randomExpM
  
  -- schedEvent :: AgentId -> SIREvent -> Double -> m ()
  schedEvent receiver e dt = do
    t <- getTime
    let qe = QueueItem receiver (Event e) (t + dt)
    tell [qe]

  -- getAgentIds :: m [AgentId]
  getAgentIds = asks trd

  -- getTime :: m Time
  getTime = asks fst3

  getMyId = asks snd3

  -- NOTE: this schedules an event with dt = 0 to the receiver by running the 
  -- receiver and filtering the replies with dt = 0 to the sender
  sendSync receiverId evt = do
    -- get the id of the agent who initiates the synchronous send
    senderId <- getMyId

    -- don't allow to send to itself because this will cause problems
    when (receiverId == senderId) 
      (error "sendSync semantic error: receiver and sender cannot be the same!")

    -- look for the receiver 
    aMay <- Map.lookup receiverId <$> gets simStateAgents

    case aMay of
      -- not found, communicate to the initiating agent
      Nothing -> return Nothing
      -- receiver found, process
      (Just (aAct, _)) -> do
        -- get current time
        tNow <- getTime
        -- get all agent ids
        ais <- getAgentIds
        -- get sim state
        ss <- get

        -- IMPORTANT TODO
        -- TODO: can we engage in multiple sendSync? e.g. can agent A sendSync to 
        -- Agent B which sendSync to agent C?

        -- get the receivers MSF by sending the event
        let receiverAct = unMSF aAct evt
        -- run the receiver
        ((ao, aAct'), es, ss') <- liftIO $ runSIRAgentImpure tNow receiverId ais ss receiverAct

        -- update state
        put ss'

        -- update the agent MSF and update the State Monad with it
        modify' (\s -> s { simStateAgents = Map.insert receiverId (aAct', ao) (simStateAgents s)})

        -- filter the events to the initiator: agent id has to match AND 
        -- it has to be an instantaneous event with dt = 0/time to schedule is 
        -- the current time.
        let esToSender = map (\(QueueItem _ (Event e) _) -> e) $ filter 
              (\(QueueItem ai (Event _) t) -> ai == senderId && t == tNow) es

        -- NOTE: all other events not in this list have to be put into the
        -- queue... this is not directly possible but we can use a trick:
        -- we simply add it to the initiator event writer using tell ;)
        let esOthers = filter 
              (\(QueueItem ai (Event _) t) -> not (ai == senderId || t == tNow)) es
        -- schedule the other events through the event writer of the initiator
        tell esOthers

        return (Just esToSender)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd :: (a,b,c) -> c
trd (_,_,c) = c

randomElemM :: MonadRandom m => [a] -> m a
randomElemM es = do
  let len = length es
  idx <- getRandomR (0, len - 1)
  return $ es !! idx

randomBoolM :: MonadRandom m => Double -> m Bool
randomBoolM p = getRandomR (0, 1) >>= (\r -> return $ r <= p)

randomExpM :: MonadRandom m => Double -> m Double
randomExpM lambda = avoid 0 >>= (\r -> return ((-log r) / lambda))
  where
    avoid :: (MonadRandom m, Eq a, Random a) => a -> m a
    avoid x = do
      r <- getRandom
      if r == x
        then avoid x
        else return r