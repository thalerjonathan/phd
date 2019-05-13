{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FunctionalDependencies     #-}

module SIR.Event where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State.Strict
import Control.Monad.Trans.MSF.Except
import Data.MonadicStreamFunction.InternalCore
import Data.MonadicStreamFunction
import qualified Data.IntMap.Strict as Map 
import qualified Data.PQueue.Min as PQ

import SIR.Model

--import Debug.Trace

--------------------------------------------------------------------------------
-- GENERAL ABS TYPE DEFINITIONS 
--------------------------------------------------------------------------------
type EventId      = Integer
type Time         = Double
type AgentId      = Int
newtype Event e   = Event e deriving Show
data QueueItem e  = QueueItem !AgentId !(Event e) Time deriving Show
type EventQueue e = PQ.MinQueue (QueueItem e)

instance Eq (QueueItem e) where
  (==) (QueueItem _ _ t1) (QueueItem _ _ t2) = t1 == t2

instance Ord (QueueItem e) where
  compare (QueueItem _ _ t1) (QueueItem _ _ t2) = compare t1 t2

-- encapsulates the effectful API for agents
class Monad m => MonadAgent e m | m -> e where
  randomBool  :: Double -> m Bool
  randomExp   :: Double -> m Double
  randomElem  :: [a] -> m a
  getAgentIds :: m [AgentId]
  getTime     :: m Time
  getMyId     :: m AgentId
  schedEvent  :: AgentId -> e -> Double -> m ()
  sendSync    :: AgentId -> e -> m (Maybe [e])

--------------------------------------------------------------------------------
-- SIR TYPE DEFINITIONS 
--------------------------------------------------------------------------------
data SIREvent 
  = MakeContact
  | Contact AgentId SIRState
  | Recover 
  deriving (Show, Eq)

type SIRAgent        = MSF SIRAgentPure SIREvent SIRState
type SIRAgentPureMap = Map.IntMap (SIRAgent, SIRState)

--------------------------------------------------------------------------------
-- CONSTANTS 
--------------------------------------------------------------------------------
makeContactInterval :: Double
makeContactInterval = 1.0

--------------------------------------------------------------------------------
-- AGENT CONSTRUCTOR
--------------------------------------------------------------------------------
-- | A sir agent which is in one of three states
sirAgent :: MonadAgent SIREvent m
         => Int         -- ^ the contact rate
         -> Double      -- ^ the infectivity
         -> Double      -- ^ the illness duration
         -> SIRState    -- ^ the initial state of the agent
         -> m (MSF m SIREvent SIRState)
sirAgent cor inf ild Susceptible = do
  -- on start
  scheduleMakeContactM 
  return $ susceptibleAgent cor inf ild 
sirAgent _ _ ild Infected = do
  -- on start
  scheduleRecoveryM ild
  return infectedAgent 
sirAgent _ _ _ Recovered = 
  return recoveredAgent

--------------------------------------------------------------------------------
-- AGENTS
--------------------------------------------------------------------------------
susceptibleAgent :: MonadAgent SIREvent m
                 => Int
                 -> Double
                 -> Double
                 -> MSF m SIREvent SIRState
susceptibleAgent cor inf ild = 
    switch
      susceptibleAgentInfected
      (const infectedAgent)
  where
    susceptibleAgentInfected :: MonadAgent SIREvent m
                             => MSF m SIREvent (SIRState, Maybe ()) 
    susceptibleAgentInfected = proc e -> do
      ret <- arrM handleEvent -< e
      case ret of
        Nothing -> returnA -< (Susceptible, ret)
        _       -> returnA -< (Infected, ret)

    handleEvent :: MonadAgent SIREvent m
                => SIREvent 
                -> m (Maybe ())
    handleEvent (Contact _ Infected) = do
      r <- randomBool inf
      if r 
        then do
          scheduleRecoveryM ild
          return $ Just ()
        else return Nothing

    handleEvent MakeContact = do
      ais <- getAgentIds
      receivers <- forM [1..cor] (const $ randomElem ais)
      mapM_ makeContactWith receivers
      scheduleMakeContactM
      return Nothing

    handleEvent _ = return Nothing

    makeContactWith :: MonadAgent SIREvent m => AgentId -> m ()
    makeContactWith receiver = do
      ai <- getMyId
      schedEvent receiver (Contact ai Susceptible) 0.0

infectedAgent :: MonadAgent SIREvent m => MSF m SIREvent SIRState
infectedAgent = 
    switch 
      infectedAgentRecovered 
      (const recoveredAgent)
  where
    infectedAgentRecovered :: MonadAgent SIREvent m
                           => MSF m SIREvent (SIRState, Maybe ()) 
    infectedAgentRecovered = proc e -> do
      ret <- arrM handleEvent -< e
      case ret of
        Nothing -> returnA -< (Infected, ret)
        _       -> returnA -< (Recovered, ret)

    handleEvent :: MonadAgent SIREvent m => SIREvent -> m (Maybe ())
    handleEvent (Contact sender Susceptible) = do
      replyContact sender
      return Nothing
    handleEvent Recover = return $ Just ()
    handleEvent _ = return Nothing

    replyContact :: MonadAgent SIREvent m => AgentId -> m ()
    replyContact receiver = do
      ai <- getMyId
      schedEvent receiver (Contact ai Infected) 0.0

recoveredAgent :: MonadAgent SIREvent m => MSF m SIREvent SIRState
recoveredAgent = arr (const Recovered)

--------------------------------------------------------------------------------
-- AGENT UTILS
--------------------------------------------------------------------------------
scheduleMakeContactM :: MonadAgent SIREvent m => m ()
scheduleMakeContactM = do
  ai <- getMyId
  schedEvent ai MakeContact makeContactInterval

scheduleRecoveryM :: MonadAgent SIREvent m => Double -> m ()
scheduleRecoveryM ild = do
  dt <- randomExp (1 / ild)
  ai <- getMyId
  schedEvent ai Recover dt

--------------------------------------------------------------------------------
-- SIMULATION KERNEL
--------------------------------------------------------------------------------
runEventSIR :: [SIRState]
            -> Int
            -> Double
            -> Double 
            -> Integer
            -> Double    
            -> StdGen
            -> [(Time, (Int, Int, Int))]
runEventSIR as cor inf ild maxEvents tLimit rng
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
initSIRPure as cor inf ild rng = do 
    -- NOTE: sendSync does not work in initialisation sirAgent function (and
    -- also not required and would also violate semantics of the simulation as
    -- the simulation is not running yet and a send event would not make sense)
    -- Thus we use an empty map and ignore the returned (empty) map in
    -- createAgent. If an agent performs sendSync in the initialisation phase,
    -- it will always result in a Nothing because it 
    let ss0 = SimState { simStateRng = rng, simStateAgents = Map.empty }
    let (ss0', ret) = foldr createAgent (ss0, []) (zip ais as)

    let (ags, ess) = unzip ret

    let es    = concat ess
        asMap = Prelude.foldr 
                  (\(aid, a, s) acc -> Map.insert aid (a, s) acc) 
                  Map.empty 
                  (Prelude.zip3 ais ags as)

        eq = foldr PQ.insert PQ.empty es

    let ss0'' = ss0' { simStateAgents = asMap }

    (ss0'', eq)
  where
    ais  = [0.. length as - 1]

    createAgent :: (AgentId, SIRState)
                -> (SimState, [(SIRAgent, [QueueItem SIREvent])])
                -> (SimState, [(SIRAgent, [QueueItem SIREvent])])
    createAgent (ai, s) (ss, acc) = (ss', (a, es) : acc)
      where
        (a, es, ss') = runSIRAgentPure 0 ai ais ss (sirAgent cor inf ild s)

processQueue :: Integer 
             -> Double
             -> SimState
             -> EventQueue SIREvent
             -> [AgentId]
             -> [(Time, (Int, Int, Int))]
             -> [(Time, (Int, Int, Int))]
processQueue 0 _ _ _ _ acc = acc -- terminated by externals of simulation: hit event limit
processQueue n tLimit ss q ais acc 
    | isNothing mayHead = acc -- terminated by internals of simulation model: no more events
    | evtTime > tLimit  = acc -- terminated by externals of simulation: hit time limit
    | otherwise = do
      let retMay = processEvent ss ais evt
      -- receiver not found, remove event and carray on
      case retMay of
        -- event-receiver not found, next event
        Nothing -> processQueue (n-1) tLimit ss q' ais acc
        -- event receiver found
        (Just (ss', es)) -> do
          -- insert new events into queue
          let q'' = foldr PQ.insert q' es
          
          -- sample domain-state for current event
          let (tPre, _) = head acc
          let sir = (evtTime, aggregateAgentMap (simStateAgents ss'))

          let acc' = if evtTime == tPre 
                      then sir : tail acc
                      else sir : acc
          
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
             -> Maybe (SimState, [QueueItem SIREvent])
processEvent ss ais (QueueItem receiver (Event e) evtTime) = do
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


{- newtype SIRAgentPure a = SIRAgentPure 
  { unSirAgentPure :: ReaderT (Time, AgentId, [AgentId]) 
                        (WriterT [QueueItem SIREvent] 
                          (StateT SIRAgentPureMap (Rand StdGen))) a}
  deriving (Functor, Applicative, Monad,
            MonadRandom, 
            MonadWriter [QueueItem SIREvent],
            MonadReader (Time, AgentId, [AgentId]),
            MonadState SIRAgentPureMap)
 -}

data SimState = SimState
  { simStateRng    :: StdGen
  , simStateAgents :: SIRAgentPureMap
  }

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
        let ((ao, aAct'), es, ss') = runSIRAgentPure tNow receiverId ais ss receiverAct

        -- update state
        put ss'

        -- update the agent MSF and update the State Monad with it
        modify' (\s -> s { simStateAgents = Map.insert receiverId (aAct', ao) (simStateAgents s) })

        -- filter the events to the initiator: agent id has to match AND 
        -- it has to be an instantaneous event with dt = 0/time to schedule is 
        -- the current time.
        let esToSender = map (\(QueueItem _ (Event e) _) -> e) $ filter 
              (\(QueueItem ai (Event _) t) -> ai == senderId && t == tNow) es

        -- NOTE: all other events not in this list have to be put into the
        -- queue... this is not directly possible but we can use a trick:
        -- we simply add it to the initiator event writer using tell ;)
        let esOthers = filter 
              (\(QueueItem ai (Event _) t) -> ai /= senderId || t == tNow) es
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