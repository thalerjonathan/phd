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
    modifyDomainState incSus
    scheduleEvent aid MakeContact makeContactInterval
    return $ susceptibleAgent aid
sirAgent Infected aid = do
    -- on start
    modifyDomainState incInf
    dt <- lift $ randomExpM (1 / illnessDuration)
    scheduleEvent aid Recover dt
    return $ infectedAgent aid
sirAgent Recovered _ = modifyDomainState incRec >> return recoveredAgent

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
    susceptibleAgentInfected = proc e ->
      case e of
        (Contact _ s) ->
          if Infected == s
            then do
              r <- arrM_ (lift $ randomBoolM infectivity) -< ()
              if r 
                then do
                  arrM_ $ modifyDomainState decSus -< ()
                  arrM_ $ modifyDomainState incInf -< ()
                  dt <- arrM_ (lift $ randomExpM (1 / illnessDuration)) -< ()
                  arrM (scheduleEvent aid Recover) -< dt
                  returnA -< ((), Just ())
                else returnA -< ((), Nothing)
            else returnA -< ((), Nothing)

        MakeContact -> do
          ais <- arrM_ allAgentIds -< ()
          receivers <- arrM (\ais -> lift $ forM [1..contactRate] (const $ randomElem ais)) -< ais
          arrM (mapM_ makeContact) -< receivers
          arrM_ (scheduleEvent aid MakeContact makeContactInterval) -< ()
          returnA -< ((), Nothing)

        -- this will never occur for a susceptible
        Recover -> returnA -< ((), Nothing)

    makeContact :: AgentId -> (SIRMonadT g) ()
    makeContact receiver = 
      scheduleEvent 
        receiver 
        (Contact aid Susceptible)
        0.0 -- immediate schedule

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
    infectedAgentRecovered = proc e ->
      case e of
        (Contact sender s) ->
          -- can we somehow replace it with 'when'?
          if Susceptible == s
            then do
              arrM replyContact -< sender
              returnA -< ((), Nothing)
            else returnA -< ((), Nothing)
        -- this will never occur for an infected agent
        MakeContact  -> returnA -< ((), Nothing) 
        Recover      -> do
          arrM_ $ modifyDomainState decInf -< ()
          arrM_ $ modifyDomainState incRec -< ()
          returnA -< ((), Just ())

    replyContact :: AgentId -> (SIRMonadT g) ()
    replyContact receiver =
      scheduleEvent 
        receiver 
        (Contact aid Infected)
        0.0 -- immediate schedule

recoveredAgent :: SIRAgentCont g
recoveredAgent = arr (const ()) 

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

runABS :: Monad m 
       => [Agent m e s] 
       -> s
       -> Integer 
       -> Double
       -> Double
       -> m (Time, Integer, [s])
runABS as0 s0 steps tLimit tSampling = do
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
  where
    abs0 = ABSState {
      absEvtQueue    = PQ.empty
    , absTime        = 0
    , absAgents      = Map.empty
    , absAgentIds    = []
    , absEvtCount    = 0
    , absDomainState = s0
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