{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SIR.SIR where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.MSF.Except
import Data.MonadicStreamFunction.InternalCore
import Data.MonadicStreamFunction
import qualified Data.IntMap.Strict as Map 
import qualified Data.PQueue.Min as PQ

-- import Debug.Trace

type EventId      = Integer
type Time         = Double
type AgentId      = Int
newtype Event e   = Event e deriving Show
data QueueItem e  = QueueItem AgentId (Event e) Time deriving Show
type EventQueue e = PQ.MinQueue (QueueItem e)

instance Eq (QueueItem e) where
  (==) (QueueItem _ _ t1) (QueueItem _ _ t2) = t1 == t2

instance Ord (QueueItem e) where
  compare (QueueItem _ _ t1) (QueueItem _ _ t2) = compare t1 t2

type ABSMonad m e s    = ReaderT Time (WriterT [QueueItem e] (ReaderT [AgentId] (WriterT [s] m)))
type AgentCont m e s o = MSF (ABSMonad m e s) e o
type Agent m e s o     = AgentId -> (ABSMonad m e s) (AgentCont m e s o)
type AgentMap m e s o  = Map.IntMap (AgentCont m e s o)

data SIRDomainOp     = Inc | Dec deriving (Show, Eq)
data SIRDomainUpdate = SIRDomainUpdate SIRDomainOp SIRState Time deriving (Show, Eq)

data SIRState 
  = Susceptible
  | Infected
  | Recovered
  deriving (Show, Eq)

data SIREvent 
  = MakeContact
  | Contact AgentId SIRState
  | Recover 
  deriving (Show, Eq)

type SIRMonad g     = Rand g
type SIRMonadT g    = ABSMonad (SIRMonad g) SIREvent SIRDomainUpdate
type SIRAgent g     = Agent (SIRMonad g) SIREvent SIRDomainUpdate SIRState
type SIRAgentCont g = AgentCont (SIRMonad g) SIREvent SIRDomainUpdate SIRState

makeContactInterval :: Double
makeContactInterval = 1.0

-- | A sir agent which is in one of three states
sirAgent :: RandomGen g 
         => Int         -- ^ the contact rate
         -> Double      -- ^ the infectivity
         -> Double      -- ^ the illness duration
         -> SIRState    -- ^ the initial state of the agent
         -> SIRAgent g  -- ^ the continuation
sirAgent cr inf illDur Susceptible aid = do
  -- on start
  incSus
  scheduleMakeContact aid 
  return $ susceptibleAgent aid cr inf illDur 
sirAgent _ _ illDur Infected aid = do
  -- on start
  incInf
  scheduleRecovery aid illDur
  return $ infectedAgent aid
sirAgent _ _ _ Recovered _ = do
  incRec
  return recoveredAgent

susceptibleAgent :: RandomGen g 
                 => AgentId
                 -> Int
                 -> Double
                 -> Double
                 -> SIRAgentCont g
susceptibleAgent aid cr inf illDur = 
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
      r <- lift $ lift $ lift $ lift $ randomBoolM inf
      if r 
        then do
          decSus
          incInf
          scheduleRecovery aid illDur
          return $ Just ()
        else return Nothing

    handleEvent MakeContact = do
      ais       <- allAgentIds
      crExp     <- lift $ lift $ lift $ lift $ randomExpM (1 / fromIntegral cr)
      receivers <- lift $ lift $ lift $ lift $ forM [1..crExp] (const $ randomElem ais)
      mapM_ makeContactWith receivers
      scheduleMakeContact aid
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
    handleEvent Recover = do
      decInf
      incRec
      return $ Just ()
    handleEvent _ = return Nothing

    replyContact :: AgentId -> (SIRMonadT g) ()
    replyContact receiver = scheduleEvent receiver (Contact aid Infected) 0.0

recoveredAgent :: SIRAgentCont g
recoveredAgent = arr (const Recovered)

scheduleMakeContact :: AgentId -> (SIRMonadT g) ()
scheduleMakeContact aid = scheduleEvent aid MakeContact makeContactInterval

scheduleRecovery :: RandomGen g => AgentId -> Double -> (SIRMonadT g) ()
scheduleRecovery aid illnessDuration = do
  dt <- lift $ lift $ lift $ lift $ randomExpM (1 / illnessDuration)
  scheduleEvent aid Recover dt

incSus :: Monad m => (ABSMonad m e SIRDomainUpdate) ()
incSus = changeDomState Inc Susceptible

decSus :: Monad m => (ABSMonad m e SIRDomainUpdate) ()
decSus = changeDomState Dec Susceptible

incInf :: Monad m => (ABSMonad m e SIRDomainUpdate) ()
incInf = changeDomState Inc Infected

decInf :: Monad m => (ABSMonad m e SIRDomainUpdate) ()
decInf = changeDomState Dec Infected

incRec :: Monad m => (ABSMonad m e SIRDomainUpdate) ()
incRec = changeDomState Inc Recovered

changeDomState :: Monad m 
               => SIRDomainOp
               -> SIRState
               -> (ABSMonad m e SIRDomainUpdate) ()
changeDomState op ss = ask >>= \t -> lift $ lift $ lift $ tell [SIRDomainUpdate op ss t]

runSIR :: RandomGen g
       => [SIRState]
       -> Int
       -> Double
       -> Double 
       -> Integer
       -> Double    
       -> g
       -> ([(Time, (Int, Int, Int))], Integer)
runSIR ss cr inf illDur maxEvents tLimit g = (adus', ect)
  where
    as0    = map (sirAgent cr inf illDur) ss
    asIds  = [0.. length ss - 1]
    asWIds = Prelude.zipWith (\a aid -> a aid) as0 asIds

    (dus, ect) = evalRand act g
    adus = aggregateDomainUpdates dus

    adus' = if null ss
             then (0, (0,0,0)) : adus
             else adus

    act = do
      let asEvtWriter   = runReaderT (sequence asWIds) 0
          asAsIdsReader = runWriterT asEvtWriter
          asDomWriter   = runReaderT asAsIdsReader asIds
          
      ((as0', es), dus0) <- runWriterT asDomWriter

      let asMap = Prelude.foldr (\(aid, a) acc -> Map.insert aid a acc) Map.empty (Prelude.zip asIds as0')
          eq    = foldr PQ.insert PQ.empty es

          simDomWriter = runReaderT (stepClock maxEvents asMap eq) asIds

      (relEvtCnt, dusSim) <- runWriterT simDomWriter

      let evtCnt = if maxEvents < 0
                    then -(relEvtCnt + 1)
                    else maxEvents - relEvtCnt

      return (dus0 ++ dusSim, evtCnt)

    stepClock :: Monad m --(MonadReader [AgentId] m, MonadWriter [s] m, MonadRandom m)
              => Integer 
              -> AgentMap m e s o
              -> EventQueue e
              -> ReaderT [AgentId] (WriterT [s] m) Integer
    stepClock 0 _ _ = return 0 -- processed all events
    stepClock n as q
        | isNothing mayHead = return n -- finished, no more events
        | evtTime > tLimit  = return n -- finished, hit time-limit
        | isNothing am      = stepClock (n-1) as q' -- event-receiver not found, next event
        | otherwise         = do
          let aReaderTime   = unMSF a e
              aWriterEvents = runReaderT aReaderTime evtTime
              amsf          = runWriterT aWriterEvents

          ((_ao, a'), es) <- amsf

          let q'' = foldr PQ.insert q' es
              as' = Map.insert aid a' as

          stepClock (n-1) as' q''
      where
        mayHead = PQ.getMin q
        (QueueItem aid (Event e) evtTime) = fromJust mayHead

        q' = PQ.drop 1 q
        am = Map.lookup aid as
        a  = fromJust am

allAgentIds :: Monad m => (ABSMonad m e s) [AgentId]
allAgentIds = lift $ lift ask

scheduleEvent :: Monad m
              => AgentId 
              -> e
              -> Double
              -> (ABSMonad m e s) ()
scheduleEvent aid e dt = do
  t <- ask
  let qe = QueueItem aid (Event e) (t + dt)  
  lift $ tell [qe]

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

--------------------------------------------------------------------------------
-- UTILS
aggregateDomainUpdates :: [SIRDomainUpdate] -> [(Time, (Int, Int, Int))]
aggregateDomainUpdates dus = reverse $ snd $ foldr aggregateDomainUpdatesAux ((0,0,0), []) (reverse dus)
  where
    aggregateDomainUpdatesAux :: SIRDomainUpdate
                              -> ((Int, Int, Int), [(Time, (Int, Int, Int))])
                              -> ((Int, Int, Int), [(Time, (Int, Int, Int))])
    aggregateDomainUpdatesAux (SIRDomainUpdate Inc Susceptible t) ((s,i,r), acc) 
      = let sir = (s+1,i,r) in (sir, (t, sir) : acc)
    aggregateDomainUpdatesAux (SIRDomainUpdate Dec Susceptible t) ((s,i,r), acc) 
      = let sir = (s-1,i,r) in (sir, (t, sir) : acc)
    aggregateDomainUpdatesAux (SIRDomainUpdate Inc Infected t) ((s,i,r), acc) 
      = let sir = (s,i+1,r) in (sir, (t, sir) : acc)
    aggregateDomainUpdatesAux (SIRDomainUpdate Dec Infected t) ((s,i,r), acc) 
      = let sir = (s,i-1,r) in (sir, (t, sir) : acc)
    aggregateDomainUpdatesAux (SIRDomainUpdate Inc Recovered t) ((s,i,r), acc) 
      = let sir = (s,i,r+1) in (sir, (t, sir) : acc)
    aggregateDomainUpdatesAux (SIRDomainUpdate Dec Recovered t) ((s,i,r), acc) 
      = let sir = (s,i,r-1) in (sir, (t, sir) : acc)

compressOutput :: [(Time, (Int, Int, Int))] -> [(Time, (Int, Int, Int))]
compressOutput = foldr compressOutputAux []
  where
    compressOutputAux :: (Time, (Int, Int, Int))
                      -> [(Time, (Int, Int, Int))]
                      -> [(Time, (Int, Int, Int))]
    compressOutputAux ts [] = [ts]
    compressOutputAux (t, s) ((t', s') : acc) 
      | t == t'   = (t, s) : acc
      | otherwise = (t, s) : (t', s') : acc