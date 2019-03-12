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

data ABSState m e s = ABSState
  { absEvtQueue    :: !(EventQueue e)
  , absTime        :: !Time
  , absAgents      :: !(Map.IntMap (AgentCont m e s))
  , absAgentIds    :: ![AgentId]
  , absEvtCount    :: !Integer
  , absDomainState :: !s
  }

type ABSMonad m e s  = StateT (ABSState m e s) m
type AgentCont m e s = MSF (ABSMonad m e s) e ()
type Agent m e s     = AgentId -> (ABSMonad m e s) (AgentCont m e s)

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
type SIRMonadT g    = ABSMonad (SIRMonad g) SIREvent SIRDomainState
type SIRAgent g     = Agent (SIRMonad g) SIREvent SIRDomainState
type SIRAgentCont g = AgentCont (SIRMonad g) SIREvent SIRDomainState

makeContactInterval :: Double
makeContactInterval = 1.0

contactRate :: Int
contactRate = 5

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

initAgents :: RandomGen g 
           => Int 
           -> Int 
           -> [SIRAgent g]
initAgents n nInf = susAs ++ infAs
  where
    susAs = fmap (const $ sirAgent Susceptible) [1..n - nInf]
    infAs = fmap (const $ sirAgent Infected) [1..nInf] 

-- | A sir agent which is in one of three states
sirAgent :: RandomGen g 
         => SIRState    -- ^ the initial state of the agent
         -> SIRAgent g  -- ^ the continuation
sirAgent Susceptible aid = do
    -- on start
    changeSIRNumbers incSus
    scheduleMakeContact aid
    return $ susceptibleAgent aid
sirAgent Infected aid = do
    -- on start
    changeSIRNumbers incInf
    scheduleRecovery aid
    return $ infectedAgent aid
sirAgent Recovered _ = do
  changeSIRNumbers incRec
  return recoveredAgent

susceptibleAgent :: RandomGen g 
                 => AgentId
                 -> SIRAgentCont g
susceptibleAgent aid = 
    switch
      susceptibleAgentInfected
      (const $ infectedAgent aid)
  where
    susceptibleAgentInfected :: RandomGen g 
                             => MSF
                                (SIRMonadT g) 
                                SIREvent
                                ((), Maybe ()) 
    susceptibleAgentInfected = proc e -> do
      ret <- arrM handleEvent -< e
      returnA -< ((), ret)

    handleEvent :: RandomGen g => SIREvent -> (SIRMonadT g) (Maybe ())
    handleEvent (Contact _ Infected) = do
      r <- lift $ randomBoolM infectivity
      if r 
        then do
          changeSIRNumbers decSus
          changeSIRNumbers incInf
          scheduleRecovery aid
          return $ Just ()
        else return Nothing

    handleEvent MakeContact = do
      ais <- allAgentIds
      receivers <- forM [1..contactRate] (const $ lift $ randomElem ais)
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
                              ((), Maybe ()) 
    infectedAgentRecovered = proc e -> do
      ret <- arrM handleEvent -< e
      returnA -< ((), ret)

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
recoveredAgent = arr (const ()) 

scheduleMakeContact :: AgentId -> (SIRMonadT g) ()
scheduleMakeContact aid = scheduleEvent aid MakeContact makeContactInterval

scheduleRecovery :: RandomGen g => AgentId -> (SIRMonadT g) ()
scheduleRecovery aid = do
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

runABS :: [SIRAgent g] 
       -> g
       -> Integer 
       -> Double
       -> Double
       -> (Time, Integer, [SIRDomainState])
runABS as0 g0 steps tLimit tSampling = evalRand act g0
  where
    act = do
      (as0', abs') <- runStateT (sequence asWIds) abs0
      
      let asMap = Prelude.foldr (\(aid, a) acc -> Map.insert aid a acc) Map.empty (Prelude.zip [0..] as0')
      let abs'' = abs' { 
        absAgents   = asMap
      , absAgentIds = Map.keys asMap 
      }

      (domStateSamples, finalState) <- runStateT (stepClock steps 0 []) abs''
      
      let finalTime   = absTime finalState
          finalEvtCnt = absEvtCount finalState

      return (finalTime, finalEvtCnt, reverse domStateSamples)

    abs0 = ABSState {
      absEvtQueue    = PQ.empty
    , absTime        = 0
    , absAgents      = Map.empty
    , absAgentIds    = []
    , absEvtCount    = 0
    , absDomainState = (0,0,0)
    }

    asWIds = Prelude.zipWith (\a aid -> a aid) as0 [0..]

    stepClock :: Monad m
              => Integer 
              -> Double
              -> [s]
              -> (ABSMonad m e s) [s]
    stepClock 0 _ acc = return acc
    stepClock n ts acc = do 
      q <- gets absEvtQueue

      let mayHead = PQ.getMin q
      if isNothing mayHead 
        then return acc
        else do
          ec <- gets absEvtCount
          t  <- gets absTime

          let (QueueItem aid (Event e) t') = fromJust mayHead
              q' = PQ.drop 1 q

          -- modify time and changed queue before running the process
          -- because the process might change the queue 
          modify (\s -> s { 
            absEvtQueue = q' 
          , absTime     = t'
          , absEvtCount = ec + 1
          })

          as <- gets absAgents
          let a = fromJust $ Map.lookup aid as
          (_, a') <- unMSF a e
          let as' = Map.insert aid a' as

          modify (\s -> s { absAgents = as' })

          s <- gets absDomainState
          let (acc', ts') = if ts >= tSampling
              then (s : acc, 0)
              else (acc, ts + (t' - t))

          if t' < tLimit
            then stepClock (n-1) ts' acc'
            else return acc'

allAgentIds :: Monad m => (ABSMonad m e s) [AgentId]
allAgentIds = gets absAgentIds

changeSIRNumbers :: ((Int, Int, Int) -> (Int, Int, Int)) -> (SIRMonadT g) ()
changeSIRNumbers = modifyDomainState

modifyDomainState :: Monad m 
                  => (s -> s) 
                  -> (ABSMonad m e s) ()
modifyDomainState f = 
  modify (\s -> s { absDomainState = f $ absDomainState s })

scheduleEvent :: Monad m
              => AgentId 
              -> e
              -> Double
              -> (ABSMonad m e s) ()
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