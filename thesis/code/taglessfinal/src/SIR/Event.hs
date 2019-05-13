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

data SendSync e = OK [e] | NoReply | NotFound

-- encapsulates the effectful API for agents
class Monad m => MonadAgent e m | m -> e where
  -- TODO: refactor random stuff out into separate MonadRandomABS which builds on MonadRandom
  randomExp   :: Double -> m Double
  randomBool  :: Double -> m Bool
  randomElem  :: [a] -> m a
  
  getAgentIds :: m [AgentId]
  getTime     :: m Time
  getMyId     :: m AgentId

  schedEvent  :: AgentId -> e -> Double -> m ()

  -- TODO: this synchronously sends an event to the respective agent
  sendSync :: AgentId -> e -> m (SendSync e)

--------------------------------------------------------------------------------
-- SIR TYPE DEFINITIONS 
--------------------------------------------------------------------------------
data SIREvent 
  = MakeContact
  | Contact AgentId SIRState
  | Recover 
  deriving (Show, Eq)

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

    handleEvent :: MonadAgent SIREvent m => SIREvent -> m (Maybe ())
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
type SIRAgentPureMap g = Map.IntMap (MSF (SIRAgentPure g) SIREvent SIRState, SIRState)

runEventSIR :: RandomGen g
            => [SIRState]
            -> Int
            -> Double
            -> Double 
            -> Integer
            -> Double    
            -> g
            -> [(Time, (Int, Int, Int))]
runEventSIR ss cor inf ild maxEvents tLimit
    = evalRand executeAgents
  where
    executeAgents ::  RandomGen g
                  => Rand g [(Time, (Int, Int, Int))]
    executeAgents = do
      (asMap, eq) <- initSIRPure ss cor inf ild
      
      let ais = Map.keys asMap
      let doms as t = (t, aggregateAgentMap as)

      processQueue maxEvents tLimit asMap eq ais doms

    aggregateAgentMap :: SIRAgentPureMap g -> (Int, Int, Int) 
    aggregateAgentMap = Prelude.foldr aggregateAgentMapAux (0,0,0)
      where
        aggregateAgentMapAux :: (MSF m SIREvent SIRState, SIRState)
                             -> (Int, Int, Int) 
                             -> (Int, Int, Int) 
        aggregateAgentMapAux (_, Susceptible) (s,i,r) = (s+1,i,r)
        aggregateAgentMapAux (_, Infected) (s,i,r)    = (s,i+1,r)
        aggregateAgentMapAux (_, Recovered) (s,i,r)   = (s,i,r+1)

initSIRPure :: RandomGen g
            => [SIRState]
            -> Int
            -> Double
            -> Double 
            -> Rand g (SIRAgentPureMap g, EventQueue SIREvent)
initSIRPure ss cor inf ild = do
    (as0', ess) <- unzip <$> mapM (\(ai, s) -> runSIRAgentPure 0 ai ais (sirAgent cor inf ild s)) (zip ais ss)

    let es    = concat ess
        asMap = Prelude.foldr 
                  (\(aid, a, s) acc -> Map.insert aid (a, s) acc) 
                  Map.empty 
                  (Prelude.zip3 ais as0' ss)

        eq = foldr PQ.insert PQ.empty es

    return (asMap, eq)
  where
    ais  = [0.. length ss - 1]

-- TODO: fix, extremely high memory demands!
processQueue :: RandomGen g
             => Integer 
             -> Double
             -> SIRAgentPureMap g
             -> EventQueue SIREvent
             -> [AgentId]
             -> (SIRAgentPureMap g -> Double -> s)
             -> Rand g [s]
processQueue 0 _ _ _ _ _ = return [] -- terminated by externals of simulation: hit event limit
processQueue n tLimit am q ais dsf 
    | isNothing mayHead = return [] -- terminated by internals of simulation model: no more events
    | evtTime > tLimit  = return [] -- terminated by externals of simulation: hit time limit
    | otherwise = do
      retMay <- processEvent am ais evt
      -- receiver not found, remove event and carray on
      case retMay of
        -- event-receiver not found, next event
        Nothing -> processQueue (n-1) tLimit am q' ais dsf  
        -- event receiver found
        (Just (am', es)) -> do
          -- insert new events into queue
          let q'' = foldr PQ.insert q' es
          -- sample domain-state for current event
          let s = dsf am' evtTime
          -- non tail-recursive call to support infinite [s]
          ss <- processQueue (n-1) tLimit am' q'' ais dsf
          return (s : ss)
  where
    mayHead = PQ.getMin q
    evt     = fromJust mayHead
    evtTime = eventTime evt
    q'      = PQ.drop 1 q

    eventTime :: QueueItem e -> Time
    eventTime (QueueItem _ _ et) = et

processEvent :: RandomGen g
             => SIRAgentPureMap g
             -> [AgentId]
             -> QueueItem SIREvent
             -> Rand g (Maybe (SIRAgentPureMap g, [QueueItem SIREvent]))
processEvent as ais (QueueItem receiver (Event e) evtTime)
  | isNothing aMay = return Nothing
  | otherwise = do
    let agentAct  = unMSF a e
        
    ((ao, a'), es) <- runSIRAgentPure evtTime receiver ais agentAct

    let as' = Map.insert receiver (a', ao) as

    return $ Just (as', es)
  where
    aMay  = Map.lookup receiver as
    (a,_) = fromJust aMay

--------------------------------------------------------------------------------
-- INTERPRETER
--------------------------------------------------------------------------------
runSIRAgentPure :: Time
                -> AgentId
                -> [AgentId]
                -> SIRAgentPure g a
                -> Rand g (a, [QueueItem SIREvent])
runSIRAgentPure t ai ais act = do
  let actEvtWriter  = runReaderT (unSirAgentPure act) (t, ai, ais)
      actRand       = runWriterT actEvtWriter
      --((a, es), g') = runRand actRand g

  --(a, es, g')
  actRand

newtype SIRAgentPure g a = SIRAgentPure 
  { unSirAgentPure :: ReaderT (Time, AgentId, [AgentId]) (WriterT [QueueItem SIREvent] (Rand g)) a }
  deriving (Functor, Applicative, Monad, MonadRandom,
            MonadWriter [QueueItem SIREvent], MonadReader (Time, AgentId, [AgentId]))

instance RandomGen g => MonadAgent SIREvent (SIRAgentPure g) where
  -- randomExp :: Double -> m Double
  randomExp = randomExpM
  
  -- randomBool :: Double -> m Bool
  randomBool = randomBoolM

  -- randomElem :: [a] -> m a
  randomElem = randomElemM

  -- schedEvent :: AgentId -> SIREvent -> Double -> m ()
  schedEvent receiver e dt = do
    t <- asks fst3
    let qe = QueueItem receiver (Event e) (t + dt)  
    tell [qe]

  -- getAgentIds :: m [AgentId]
  getAgentIds = asks trd

  -- getTime :: m Time
  getTime = asks fst3

  getMyId = asks snd3

  sendSync _receiver _e = return NoReply

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