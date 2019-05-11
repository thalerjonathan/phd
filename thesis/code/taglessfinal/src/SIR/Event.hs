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

-- encapsulates the effectful API for agents
class Monad m => MonadAgent e m | m -> e where
  randomExp   :: Double -> m Double
  randomBool  :: Double -> m Bool
  randomElem  :: [a] -> m a
  schedEvent  :: AgentId -> e -> Double -> m ()
  getAgentIds :: m [AgentId]
  getTime     :: m Time

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
         -> AgentId 
         -> m (MSF m SIREvent SIRState)
sirAgent cor inf ild Susceptible aid = do
  -- on start
  scheduleMakeContactM aid
  return $ susceptibleAgent aid cor inf ild 
sirAgent _ _ ild Infected aid = do
  -- on start
  scheduleRecoveryM aid ild
  return $ infectedAgent aid
sirAgent _ _ _ Recovered _ = 
  return recoveredAgent

--------------------------------------------------------------------------------
-- AGENTS
--------------------------------------------------------------------------------
susceptibleAgent :: MonadAgent SIREvent m
                 => AgentId
                 -> Int
                 -> Double
                 -> Double
                 -> MSF m SIREvent SIRState
susceptibleAgent aid cor inf ild = 
    switch
      susceptibleAgentInfected
      (const $ infectedAgent aid)
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
          scheduleRecoveryM aid ild
          return $ Just ()
        else return Nothing

    handleEvent MakeContact = do
      ais <- getAgentIds
      receivers <- forM [1..cor] (const $ randomElem ais)
      mapM_ makeContactWith receivers
      scheduleMakeContactM aid
      return Nothing

    handleEvent _ = return Nothing

    makeContactWith :: MonadAgent SIREvent m => AgentId -> m ()
    makeContactWith receiver = 
      schedEvent receiver (Contact aid Susceptible) 0.0

infectedAgent :: MonadAgent SIREvent m
              => AgentId 
              -> MSF m SIREvent SIRState
infectedAgent aid = 
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
    replyContact receiver = schedEvent receiver (Contact aid Infected) 0.0

recoveredAgent :: MonadAgent SIREvent m => MSF m SIREvent SIRState
recoveredAgent = arr (const Recovered)

--------------------------------------------------------------------------------
-- AGENT UTILS
--------------------------------------------------------------------------------
scheduleMakeContactM :: MonadAgent SIREvent m => AgentId -> m ()
scheduleMakeContactM aid = schedEvent aid MakeContact makeContactInterval

scheduleRecoveryM :: MonadAgent SIREvent m => AgentId -> Double -> m ()
scheduleRecoveryM aid ild = do
  dt <- randomExp (1 / ild)
  schedEvent aid Recover dt

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
            -> ([(Time, (Int, Int, Int))], Integer)
runEventSIR ss cor inf ild maxEvents tLimit g
    = (ds, 0)
  where
    ds = evalRand executeAgents g
    
    executeAgents = do
      (asMap, eq) <- initSIRPure ss cor inf ild
      
      let ais = Map.keys asMap
      let doms as t = (t, aggregateAgentMap as)

      processQueue maxEvents tLimit asMap eq ais doms

      -- let evtCnt = if maxEvents < 0
      --               then -(relEvtCnt + 1)
      --               else maxEvents - relEvtCnt

    aggregateAgentMap :: SIRAgentPureMap g -> (Int, Int, Int) 
    aggregateAgentMap = Prelude.foldr aggregateAgentMapAux (0,0,0)
      where
        aggregateAgentMapAux :: (MSF m SIREvent SIRState, SIRState)
                            -> (Int, Int, Int) 
                            -> (Int, Int, Int) 
        aggregateAgentMapAux (_, Susceptible) (s,i,r) = (s+1,i,r)
        aggregateAgentMapAux (_, Infected) (s,i,r) = (s,i+1,r)
        aggregateAgentMapAux (_, Recovered) (s,i,r) = (s,i,r+1)

initSIRPure :: RandomGen g
            => [SIRState]
            -> Int
            -> Double
            -> Double 
            -> Rand g (SIRAgentPureMap g, EventQueue SIREvent)
initSIRPure ss cor inf ild = do
    (as0', es) <- runSIRAgentPure 0 asIds (sequence asWIds)

    let asMap = Prelude.foldr 
                  (\(aid, a, s) acc -> Map.insert aid (a, s) acc) 
                  Map.empty 
                  (Prelude.zip3 asIds as0' ss)

        eq = foldr PQ.insert PQ.empty es

    return (asMap, eq)
  where
    as0    = map (sirAgent cor inf ild) ss
    asIds  = [0.. length ss - 1]
    asWIds = Prelude.zipWith (\a aid -> a aid) as0 asIds

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
        
    ((ao, a'), es) <- runSIRAgentPure evtTime ais agentAct

    let as' = Map.insert receiver (a', ao) as

    return $ Just (as', es)
  where
    aMay  = Map.lookup receiver as
    (a,_) = fromJust aMay

--------------------------------------------------------------------------------
-- INTERPRETER
--------------------------------------------------------------------------------
runSIRAgentPure :: Time
                -> [AgentId]
                -> SIRAgentPure g a
                -> Rand g (a, [QueueItem SIREvent])
runSIRAgentPure t ais act = do
  let actEvtWriter  = runReaderT (unSirAgentPure act) (t, ais)
      actRand       = runWriterT actEvtWriter
      --((a, es), g') = runRand actRand g

  --(a, es, g')
  actRand

newtype SIRAgentPure g a = SIRAgentPure 
  { unSirAgentPure :: ReaderT (Time, [AgentId]) (WriterT [QueueItem SIREvent] (Rand g)) a }
  deriving (Functor, Applicative, Monad, MonadRandom,
            MonadWriter [QueueItem SIREvent], MonadReader (Time, [AgentId]))

instance RandomGen g => MonadAgent SIREvent (SIRAgentPure g) where
  -- randomExp :: Double -> m Double
  randomExp = randomExpM
  
  -- randomBool :: Double -> m Bool
  randomBool = randomBoolM

  -- randomElem :: [a] -> m a
  randomElem = randomElemM

  -- schedEvent :: AgentId -> SIREvent -> Double -> m ()
  schedEvent aid e dt = do
    t <- asks fst
    let qe = QueueItem aid (Event e) (t + dt)  
    tell [qe]

  -- getAgentIds :: m [AgentId]
  getAgentIds = asks snd

  -- getTime :: m Time
  getTime = asks fst

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