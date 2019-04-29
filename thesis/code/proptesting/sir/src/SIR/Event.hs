{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
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

type ABSMonad m e    = ReaderT Time (WriterT [QueueItem e] (ReaderT [AgentId] m))
type AgentCont m e o = MSF (ABSMonad m e) e o
type Agent m e o     = AgentId -> (ABSMonad m e) (AgentCont m e o)
type AgentMap m e o  = Map.IntMap (AgentCont m e o, o)

data SIREvent 
  = MakeContact
  | Contact AgentId SIRState
  | Recover 
  deriving (Show, Eq)

type SIRMonad g     = Rand g
type SIRMonadT g    = ABSMonad (SIRMonad g) SIREvent
type SIRAgent g     = Agent (SIRMonad g) SIREvent SIRState
type SIRAgentCont g = AgentCont (SIRMonad g) SIREvent SIRState
type SIRAgentMap g  = AgentMap (SIRMonad g) SIREvent SIRState

makeContactInterval :: Double
makeContactInterval = 1.0

--------------------------------------------------------------------------------
-- AGENT CONSTRUCTOR
--------------------------------------------------------------------------------
-- | A sir agent which is in one of three states
sirAgent :: RandomGen g 
         => Int         -- ^ the contact rate
         -> Double      -- ^ the infectivity
         -> Double      -- ^ the illness duration
         -> SIRState    -- ^ the initial state of the agent
         -> SIRAgent g  -- ^ the continuation
sirAgent cor inf ild Susceptible aid = do
  -- on start
  scheduleMakeContact aid makeContactInterval
  return $ susceptibleAgent aid cor inf ild 
sirAgent _ _ ild Infected aid = do
  -- on start
  scheduleRecovery aid ild
  return $ infectedAgent aid
sirAgent _ _ _ Recovered _ = 
  return recoveredAgent

--------------------------------------------------------------------------------
-- AGENTS
--------------------------------------------------------------------------------
susceptibleAgent :: RandomGen g 
                 => AgentId
                 -> Int
                 -> Double
                 -> Double
                 -> SIRAgentCont g
susceptibleAgent aid cor inf ild = 
    switch
      susceptibleAgentInfected
      (const $ infectedAgent aid)
  where
    susceptibleAgentInfected :: RandomGen g 
                             => MSF
                                (SIRMonadT g) 
                                SIREvent
                                (SIRState, Maybe ()) 
    susceptibleAgentInfected = proc e -> do
      ret <- arrM handleEvent -< e
      case ret of
        Nothing -> returnA -< (Susceptible, ret)
        _       -> returnA -< (Infected, ret)

    handleEvent :: RandomGen g => SIREvent -> (SIRMonadT g) (Maybe ())
    handleEvent (Contact _ Infected) = do
      r <- lift $ lift $ lift $ randomBoolM inf
      if r 
        then do
          scheduleRecovery aid ild
          return $ Just ()
        else return Nothing

    handleEvent MakeContact = do
      ais       <- allAgentIds
      --corExp    <- lift $ lift $ lift $ randomExpM (1 / fromIntegral cor)
      receivers <- lift $ lift $ lift $ forM [1..cor] (const $ randomElem ais)
      mapM_ makeContactWith receivers
      scheduleMakeContact aid makeContactInterval
      return Nothing

    handleEvent _ = return Nothing

    makeContactWith :: AgentId -> (SIRMonadT g) ()
    makeContactWith receiver = 
      scheduleEvent receiver (Contact aid Susceptible) 0.0

infectedAgent :: AgentId -> SIRAgentCont g
infectedAgent aid = 
    switch 
      infectedAgentRecovered 
      (const recoveredAgent)
  where
    infectedAgentRecovered :: MSF 
                              (SIRMonadT g) 
                              SIREvent 
                              (SIRState, Maybe ()) 
    infectedAgentRecovered = proc e -> do
      ret <- arrM handleEvent -< e
      case ret of
        Nothing -> returnA -< (Infected, ret)
        _       -> returnA -< (Recovered, ret)

    handleEvent :: SIREvent -> (SIRMonadT g) (Maybe ())
    handleEvent (Contact sender Susceptible) = do
      replyContact sender
      return Nothing
    handleEvent Recover = return $ Just ()
    handleEvent _ = return Nothing

    replyContact :: AgentId -> (SIRMonadT g) ()
    replyContact receiver = scheduleEvent receiver (Contact aid Infected) 0.0

recoveredAgent :: SIRAgentCont g
recoveredAgent = arr (const Recovered)

--------------------------------------------------------------------------------
-- AGENT UTILS
--------------------------------------------------------------------------------
scheduleMakeContact :: RandomGen g => AgentId -> Double -> (SIRMonadT g) ()
scheduleMakeContact aid = scheduleEvent aid MakeContact

scheduleRecovery :: RandomGen g => AgentId -> Double -> (SIRMonadT g) ()
scheduleRecovery aid ild = do
  dt <- lift $ lift $ lift $ randomExpM (1 / ild)
  scheduleEvent aid Recover dt

allAgentIds :: Monad m => (ABSMonad m e) [AgentId]
allAgentIds = lift $ lift ask

scheduleEvent :: Monad m
              => AgentId 
              -> e
              -> Double
              -> (ABSMonad m e) ()
scheduleEvent aid e dt = do
  t <- ask
  let qe = QueueItem aid (Event e) (t + dt)  
  lift $ tell [qe]

--------------------------------------------------------------------------------
-- SIMULATION KERNEL
--------------------------------------------------------------------------------
-- NOTE: this is implemented in a way that it the output [s] can be treated as
-- an infinite list, which is definitely the case when the simulation does not
-- terminate by itself when running out of events (or no time-/event-limit)
-- This also requires that no function which needs to look at all elements
-- is used, like reverse. Also it means we cannot use an accumulator, and can
-- not use tail-recursion
processQueue :: Monad m 
             => Integer 
             -> Double
             -> AgentMap m e o
             -> EventQueue e
             -> (AgentMap m e o -> Double -> s)
             -> ReaderT [AgentId] m [s]
processQueue 0 _ _ _ _ = return [] -- terminated by externals of simulation: hit event limit
processQueue n tLimit am q dsf 
    | isNothing mayHead = return [] -- terminated by internals of simulation model: no more events
    | evtTime > tLimit  = return [] -- terminated by externals of simulation: hit time limit
    | otherwise = do
      retMay <- processEvent am evt 
      -- receiver not found, remove event and carray on
      case retMay of
        -- event-receiver not found, next event
        Nothing -> processQueue (n-1) tLimit am q' dsf  
        -- event receiver found
        (Just (am', es)) -> do
          -- insert new events into queue
          let q'' = foldr PQ.insert q' es
          -- sample domain-state for current event
          let s = dsf am' evtTime
          -- non tail-recursive call to support infinite [s]
          ss <- processQueue (n-1) tLimit am' q'' dsf
          return (s : ss)
  where
    mayHead = PQ.getMin q
    evt     = fromJust mayHead
    evtTime = eventTime evt
    q'      = PQ.drop 1 q

    eventTime :: QueueItem e -> Time
    eventTime (QueueItem _ _ et) = et

processEvent :: Monad m 
             => AgentMap m e o
             -> QueueItem e
             -> ReaderT [AgentId] m
                        -- no idea why have to use full expansion of AgentMap here...
                        (Maybe (Map.IntMap (AgentCont m e o, o), [QueueItem e]))
processEvent as (QueueItem receiver (Event e) evtTime)
  | isNothing aMay = return Nothing
  | otherwise = do
    let aReaderTime   = unMSF a e
        aWriterEvents = runReaderT aReaderTime evtTime
        amsf          = runWriterT aWriterEvents

    ((ao, a'), es) <- amsf

    let as' = Map.insert receiver (a', ao) as

    return $ Just (as', es)
  where
    aMay  = Map.lookup receiver as
    (a,_) = fromJust aMay

--------------------------------------------------------------------------------
-- SIR SPECIFIC SIMULATION KERNEL
--------------------------------------------------------------------------------
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
      (asMap, eq) <- initSIR ss cor inf ild
          
      let asIds = Map.keys asMap
      let doms as t = (t, aggregateAgentMap as)

      runReaderT (processQueue maxEvents tLimit asMap eq doms) asIds

      -- let evtCnt = if maxEvents < 0
      --               then -(relEvtCnt + 1)
      --               else maxEvents - relEvtCnt

aggregateAgentMap :: SIRAgentMap g -> (Int, Int, Int) 
aggregateAgentMap = Prelude.foldr aggregateAgentMapAux (0,0,0)
  where
    aggregateAgentMapAux :: (AgentCont (SIRMonad g) SIREvent SIRState, SIRState)
                         -> (Int, Int, Int) 
                         -> (Int, Int, Int) 
    aggregateAgentMapAux (_, Susceptible) (s,i,r) = (s+1,i,r)
    aggregateAgentMapAux (_, Infected) (s,i,r) = (s,i+1,r)
    aggregateAgentMapAux (_, Recovered) (s,i,r) = (s,i,r+1)

initSIR :: RandomGen g
        => [SIRState]
        -> Int
        -> Double
        -> Double
        -> SIRMonad g (SIRAgentMap g, EventQueue SIREvent)
initSIR ss cor inf ild = do
    let asEvtWriter   = runReaderT (sequence asWIds) 0
        asAsIdsReader = runWriterT asEvtWriter
  
    (as0', es) <- runReaderT asAsIdsReader asIds

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

--------------------------------------------------------------------------------
-- RANDOM UTILS
--------------------------------------------------------------------------------
randomElem :: RandomGen g => [e] -> Rand g e
randomElem es = do
  let len = length es
  idx <- getRandomR (0, len - 1)
  return $ es !! idx

randomBoolM :: RandomGen g => Double -> Rand g Bool
randomBoolM p = getRandomR (0, 1) >>= (\r -> return $ r <= p)

randomExpM :: RandomGen g => Double -> Rand g Double
randomExpM lambda = avoid 0 >>= (\r -> return ((-log r) / lambda))
  where
    avoid :: (Random a, Eq a, RandomGen g) => a -> Rand g a
    avoid x = do
      r <- getRandom
      if r == x
        then avoid x
        else return r