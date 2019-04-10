{-# LANGUAGE Arrows #-}
{-# LANGUAGE Strict #-} -- NOTE: without strict will have dramatic space leak
module SIR.SIR where

import           Data.Maybe

import           Control.Monad.Random
import           Control.Monad.State.Strict
import           Data.MonadicStreamFunction
import qualified Data.IntMap.Strict as Map 
import qualified Data.PQueue.Min as PQ

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


-- TODO get rid of ABSState
-- Introduce a Reader to make the Time and AgentIds as read-only availble to agents
-- Introduce a Writer where agents schedule events into
-- evt count and agents can be kept within the main simulation loop
-- domain-state also a writer: increment, decrement command which are then
-- aggregated for final results
data ABSState m e s o = ABSState
  { absEvtQueue    :: !(EventQueue e)
  , absTime        :: !Time
  , absAgents      :: !(Map.IntMap (AgentCont m e s o))
  , absAgentIds    :: ![AgentId]
  , absEvtCount    :: !Integer
  , absDomainState :: !s
  }

type ABSMonad m e s o  = StateT (ABSState m e s o) m
type AgentCont m e s o = MSF (ABSMonad m e s o) e o
type Agent m e s o     = AgentId -> (ABSMonad m e s o) (AgentCont m e s o)

type SIRDomainState = (Int, Int, Int)

data SIRState 
  = Susceptible
  | Infected
  | Recovered
  deriving (Show, Eq)

data SIREvent 
  = MakeContact
  | Contact AgentId SIRState
  | Recover 
  deriving Show

type SIRMonad g     = Rand g
type SIRMonadT g    = ABSMonad (SIRMonad g) SIREvent SIRDomainState SIRState
type SIRAgent g     = Agent (SIRMonad g) SIREvent SIRDomainState SIRState
type SIRAgentCont g = AgentCont (SIRMonad g) SIREvent SIRDomainState SIRState
type SIRABSState g  = ABSState (SIRMonad g) SIREvent SIRDomainState SIRState

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
  changeSIRNumbers incSus
  scheduleMakeContact aid 
  return $ susceptibleAgent aid cr inf illDur 
sirAgent _ _ illDur Infected aid = do
  -- on start
  changeSIRNumbers incInf
  scheduleRecovery aid illDur
  return $ infectedAgent aid
sirAgent _ _ _ Recovered _ = do
  changeSIRNumbers incRec
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
      r <- lift $ randomBoolM inf
      if r 
        then do
          changeSIRNumbers decSus
          changeSIRNumbers incInf
          scheduleRecovery aid illDur
          return $ Just ()
        else return Nothing

    handleEvent MakeContact = do
      ais <- allAgentIds
      receivers <- forM [1..cr] (const $ lift $ randomElem ais)
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
      changeSIRNumbers decInf
      changeSIRNumbers incRec
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
  dt <- lift $ randomExpM (1 / illnessDuration)
  scheduleEvent aid Recover dt

incSus :: (Int, Int, Int) -> (Int, Int, Int)
incSus (s, i, r) = (s+1, i, r)

decSus :: (Int, Int, Int) -> (Int, Int, Int)
decSus (s, i, r) = (s-1, i, r)

incInf :: (Int, Int, Int) -> (Int, Int, Int)
incInf (s, i, r) = (s, i+1, r)

decInf :: (Int, Int, Int) -> (Int, Int, Int)
decInf (s, i, r) = (s, i-1, r)

incRec :: (Int, Int, Int) -> (Int, Int, Int)
incRec (s, i, r) = (s, i, r+1)

runSIR :: RandomGen g
       => [SIRState]
       -> Int
       -> Double
       -> Double 
       -> Integer
       -> Double    
       -> g
       -> [(Time, SIRDomainState)]
runSIR ss cr inf illDur steps tLimit = evalRand act
  where
    as0 = map (sirAgent cr inf illDur) ss
          
    abs0 = ABSState {
      absEvtQueue    = PQ.empty
    , absTime        = 0
    , absAgents      = Map.empty
    , absAgentIds    = []
    , absEvtCount    = 0
    , absDomainState = (0,0,0)
    }

    asWIds = Prelude.zipWith (\a aid -> a aid) as0 [0..]

    act = do
      (as0', abs') <- runStateT (sequence asWIds) abs0
      
      let asMap = Prelude.foldr (\(aid, a) acc -> Map.insert aid a acc) Map.empty (Prelude.zip [0..] as0')
      let abs'' = abs' {
        absAgents   = asMap
      , absAgentIds = Map.keys asMap
      }

      -- initial domain state at time = 0
      let domStateSamples0 = [(0, absDomainState abs'')]

      (domStateSamples, finalState) <- runStateT (stepClock steps domStateSamples0) abs''

      -- add final domain state 
      let finalTime     = absTime finalState
          finalDomState = absDomainState finalState
          finalDomStateSamples = (finalTime, finalDomState) : domStateSamples

      return (reverse finalDomStateSamples)

    stepClock :: Monad m
              => Integer 
              -> [(Time, s)]
              -> (ABSMonad m e s o) [(Time, s)]
    stepClock 0 acc = return acc
    stepClock n acc = do 
      q <- gets absEvtQueue
      
      let mayHead = PQ.getMin q
      if isNothing mayHead 
        then return acc
        else do
          let (QueueItem aid (Event e) evtTime) = fromJust mayHead

          if evtTime > tLimit
            then return acc
            else do
              let q' = PQ.drop 1 q
              ec <- gets absEvtCount

              -- modify time and changed queue before running the process
              -- because the process might change the queue 
              modify (\s -> s { 
                absEvtQueue = q' 
              , absTime     = evtTime
              , absEvtCount = ec + 1
              })

              as <- gets absAgents
              let a = fromJust $ Map.lookup aid as

              -- NOTE: this might update domain-state!
              (_, a') <- unMSF a e

              let as' = Map.insert aid a' as

              modify (\s -> s { absAgents = as' })

              -- domain-state might have been updated after running the event
              -- take a sample and add it
              s <- gets absDomainState
              let acc' = (evtTime, s) : acc

              stepClock (n-1) acc'

allAgentIds :: Monad m => (ABSMonad m e s o) [AgentId]
allAgentIds = gets absAgentIds

changeSIRNumbers :: ((Int, Int, Int) -> (Int, Int, Int)) -> (SIRMonadT g) ()
changeSIRNumbers = modifyDomainState

modifyDomainState :: Monad m 
                  => (s -> s) 
                  -> (ABSMonad m e s o) ()
modifyDomainState f = 
  modify (\s -> s { absDomainState = f $ absDomainState s })

scheduleEvent :: Monad m
              => AgentId 
              -> e
              -> Double
              -> (ABSMonad m e s o) ()
scheduleEvent aid e dt = do
  q <- gets absEvtQueue
  t <- gets absTime

  let qe = QueueItem aid (Event e) (t + dt)
      q' = PQ.insert qe q

  modify (\s -> s { absEvtQueue = q' })

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