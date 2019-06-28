--{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module SIR.Pure
  ( runPureSIR
  ) where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State.Strict

import Data.MonadicStreamFunction.InternalCore
import qualified Data.IntMap.Strict as Map 
import qualified Data.PQueue.Min as PQ

import SIR.Agent
import SIR.API
import SIR.Model

data QueueItem e  = QueueItem !e !AgentId !Time deriving Show
type EventQueue e = PQ.MinQueue (QueueItem e)

instance Eq (QueueItem e) where
  (==) (QueueItem _ _ t1) (QueueItem _ _ t2) = t1 == t2

instance Ord (QueueItem e) where
  compare (QueueItem _ _ t1) (QueueItem _ _ t2) = compare t1 t2

type SIRAgent        = MSF SIRAgentPure SIREvent SIRState
type SIRAgentPureMap = Map.IntMap (SIRAgent, SIRState)

data SimState = SimState
  { simStateRng    :: !StdGen
  , simStateAgents :: !SIRAgentPureMap
  }

runPureSIR :: [SIRState]
           -> Int
           -> Double
           -> Double 
           -> Int
           -> Double    
           -> StdGen
           -> [(Time, (Int, Int, Int))]
runPureSIR as cor inf ild maxEvents tLimit rng
    = reverse $ processQueue maxEvents tLimit ss0 eq ais [(0, sir0)]
  where
    
    (ss0, eq) = initSIRPure as cor inf ild rng
    ais       = Map.keys (simStateAgents ss0)
    sir0      = aggregateAgentMap (simStateAgents ss0)
    
aggregateAgentMap :: SIRAgentPureMap -> (Int, Int, Int)
aggregateAgentMap = Prelude.foldr aggregateAgentMapAux (0,0,0)
  where
    aggregateAgentMapAux :: (MSF m SIREvent SIRState, SIRState)
                         -> (Int, Int, Int)
                         -> (Int, Int, Int)
    aggregateAgentMapAux (_, Susceptible) (s,i,r) = (s+1,i,r)
    aggregateAgentMapAux (_, Infected) (s,i,r)    = (s,i+1,r)
    aggregateAgentMapAux (_, Recovered) (s,i,r)   = (s,i,r+1)

initSIRPure :: [SIRState]
            -> Int
            -> Double
            -> Double 
            -> StdGen
            -> (SimState, EventQueue SIREvent)
initSIRPure as cor inf ild rng = (ss0'', eq) 
  where
    -- NOTE: sendSync does not work in initialisation sirAgent function (and
    -- also not required and would also violate semantics of the simulation as
    -- the simulation is not running yet and a send event would not make sense)
    -- Thus we use an empty map and ignore the returned (empty) map in
    -- createAgent. If an agent performs sendSync in the initialisation phase,
    -- it will always result in a Nothing because it 
    ais         = [0.. length as - 1]
    ss0         = SimState { simStateRng = rng, simStateAgents = Map.empty }
    (ss0', ret) = foldr createAgent (ss0, []) (zip ais as)
    (ags, ess0) = unzip ret
    es0         = concat ess0
    am          = Prelude.foldr 
                    (\(aid, a, s) acc -> Map.insert aid (a, s) acc) 
                    Map.empty 
                    (Prelude.zip3 ais ags as)
    eq          = foldr PQ.insert PQ.empty es0
    ss0''       = ss0' { simStateAgents = am }
  
    createAgent :: (AgentId, SIRState)
                -> (SimState, [(SIRAgent, [QueueItem SIREvent])])
                -> (SimState, [(SIRAgent, [QueueItem SIREvent])])
    createAgent (ai, s) (ss, acc) = (ss', (a, es) : acc)
      where
        (a, es, ss') = runSIRAgentPure 0 ai ais ss (sirAgent cor inf ild s)

processQueue :: Int 
             -> Double
             -> SimState
             -> EventQueue SIREvent
             -> [AgentId]
             -> [(Time, (Int, Int, Int))]
             -> [(Time, (Int, Int, Int))]
processQueue 0 _ _ _ _ acc = acc -- terminated by externals of simulation: hit event limit
processQueue n tLimit ss q ais acc 
    -- terminated by internals of simulation model: no more events
    | isNothing mayHead = acc 
    -- terminated by externals of simulation: hit time limit
    | evtTime > tLimit = acc 
    -- event-receiver not found, next event
    | isNothing retMay = processQueue (n-1) tLimit ss q' ais acc
    -- event receiver found
    | otherwise = processQueue (n-1) tLimit ss' q'' ais acc'
  where
    mayHead = PQ.getMin q
    evt     = fromJust mayHead
    evtTime = eventTime evt
    q'      = PQ.drop 1 q

    retMay    = processEvent ss ais evt
    (ss', es) = fromJust retMay
    -- insert new events into queue
    q'' = foldr PQ.insert q' es
          
    -- sample domain-state for current event
    (tPre, sirPre) = head acc
    sir = aggregateAgentMap (simStateAgents ss')

    acc' = if evtTime == tPre || sirPre == sir
            then (evtTime, sir) : tail acc
            else (evtTime, sir) : acc

    eventTime :: QueueItem e -> Time
    eventTime (QueueItem _ _ et) = et

processEvent :: SimState
             -> [AgentId]
             -> QueueItem SIREvent
             -> Maybe (SimState, [QueueItem SIREvent])
processEvent ss ais (QueueItem e receiver evtTime) = do
  (a,_) <- Map.lookup receiver (simStateAgents ss)
  let agentAct = unMSF a e
  let ((ao, a'), es, ss') = runSIRAgentPure evtTime receiver ais ss agentAct
  let ss'' = ss' { simStateAgents = Map.insert receiver (a', ao) (simStateAgents ss') }

  Just (ss'', es)

--------------------------------------------------------------------------------
-- INTERPRETER
--------------------------------------------------------------------------------
runSIRAgentPure :: Time
                -> AgentId
                -> [AgentId]
                -> SimState
                -> SIRAgentPure a
                -> (a, [QueueItem SIREvent], SimState)
runSIRAgentPure t ai ais ss agentAct = (ret, es, ss')
  where
    actEvtWriter     = runReaderT (unSirAgentPure agentAct) (t, ai, ais)
    actState         = runWriterT actEvtWriter
    ((ret, es), ss') = runState actState ss

newtype SIRAgentPure a = SIRAgentPure 
  { unSirAgentPure :: ReaderT (Time, AgentId, [AgentId]) 
                        (WriterT [QueueItem SIREvent]
                          (State SimState)) a}
  deriving (Functor, Applicative, Monad, 
            MonadWriter [QueueItem SIREvent],
            MonadReader (Time, AgentId, [AgentId]),
            MonadState SimState)

runRandWithSimState :: MonadState SimState m 
                    => Rand StdGen a 
                    -> m a
runRandWithSimState act = do
  g <- gets simStateRng
  let (ret, g') = runRand act g
  modify' (\s -> s { simStateRng = g' })
  return ret

instance MonadAgent SIREvent SIRAgentPure where
  randomBool = runRandWithSimState . randomBoolM
  randomElem = runRandWithSimState . randomElemM
  randomExp  = runRandWithSimState . randomExpM
  
  -- schedEvent :: AgentId -> SIREvent -> Double -> m ()
  schedEvent e receiver dt = do
    t <- getTime 
    tell [QueueItem e receiver (t + dt)]

  -- getAgentIds :: m [AgentId]
  getAgentIds = asks trd

  -- getTime :: m Time
  getTime = asks fst3

  getMyId = asks snd3

instance MonadAgentSync SIREvent SIRAgentPure where
  -- NOTE: this schedules an event with dt = 0 to the receiver by running the 
  -- receiver and filtering the replies with dt = 0 to the sender
  sendSync evt receiverId = do
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
        -- Engageing in multiple sendSync e.g. agent A sendSync to 
        -- Agent B which sendSync to agent C should work withouth problems BUT
        -- it is not allowed to sendSync to an agent which is already part
        -- of a sendSync "call stack": agent A sendSync -> agent B sendSync ->
        -- sendSync agent C -> agent A.
        -- This will result in wrong semantics: the last agent A will be
        -- overridden by the results of the first agent A, which is (proably)
        -- not what we want and (proably) results in erroneous / unexpected behaviour.

        -- get the receivers MSF by sending the event
        let receiverAct = unMSF aAct evt
        -- run the receiver
        let ((ao, aAct'), es, ss') = runSIRAgentPure tNow receiverId ais ss receiverAct

        -- update state
        put ss'

        -- update the agent MSF and update the State Monad with it
        modify' (\s -> s { simStateAgents = Map.insert receiverId (aAct', ao) (simStateAgents s)})

        -- filter the events to the initiator: agent id has to match AND 
        -- it has to be an instantaneous event with dt = 0/time to schedule is 
        -- the current time.
        let esToSender = map (\(QueueItem e _ _) -> e) $ filter 
              (\(QueueItem _ ai t) -> ai == senderId && t == tNow) es

        -- NOTE: all other events not in this list have to be put into the
        -- queue... this is not directly possible but we can use a trick:
        -- we simply add it to the initiator event writer using tell ;)
        let esOthers = filter 
              (\(QueueItem _ ai t) -> not (ai == senderId || t == tNow)) es
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