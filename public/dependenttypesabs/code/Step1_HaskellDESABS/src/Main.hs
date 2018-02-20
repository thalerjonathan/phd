{-# LANGUAGE Arrows     #-}
module Main where 

-- first import: base
-- none

-- second import: 3rd party libraries
import           Control.Monad.Random
import           Control.Monad.State.Strict
import           Data.Maybe
import           Data.MonadicStreamFunction
import qualified Data.Map as Map
import qualified Data.PQueue.Min as PQ
import           Debug.Trace

-- third import: project local
-- none

type Time        = Double
type AgentId     = Int
newtype Event e  = Event e deriving Show
data QueueItem e = QueueItem AgentId (Event e) Time deriving Show
type EventQueue e  = PQ.MinQueue (QueueItem e)

instance Eq (QueueItem e) where
  (==) (QueueItem _ _ t1) (QueueItem _ _ t2) = t1 == t2

instance Ord (QueueItem e) where
  compare (QueueItem _ _ t1) (QueueItem _ _ t2) = compare t1 t2

data ABSState e = ABSState
  { absEvtQueue :: EventQueue e
  , absTime     :: Time
  , absAgents   :: Map.Map AgentId (AgentCont e)
  , absAgentIds :: [AgentId]
  , absEvtCount :: Integer
  }

type ABSMonad e  = State (ABSState e)
type AgentCont e = MSF (ABSMonad e) (Event e) ()
type Agent e     = AgentId -> State (ABSState e) (AgentCont e)

type ABSOut = (Time, ())

data SIRState = Susceptible
              | Infected
              | Recovered
              deriving (Show, Eq)

data SIREvent = MakeContact
              | Contact AgentId SIRState
              | Recover 
              deriving Show

type SIRAgent     = Agent SIREvent
type SIRAgentCont = AgentCont SIREvent

rngSeed :: Int
rngSeed = 42

agentCount :: Int
agentCount = 1000

infectedCount :: Int
infectedCount = 10

-- use negative number for unlimited number of steps
maxSimulationSteps :: Integer
maxSimulationSteps = -1

-- use 1 / 0 for unrestricted time
maxSimulationTime :: Double
maxSimulationTime = 160.0

main :: IO ()
main = do
  let g = mkStdGen rngSeed
  let (as, _) = runRand (initAgents agentCount infectedCount) g

  let s = runABS as maxSimulationSteps maxSimulationTime

  let t = absTime s
  let ec = absEvtCount s

  print $ "Finished at t = " ++ show t ++ ", after " ++ show ec ++ " events"

makeContactInterval :: Double
makeContactInterval = 1.0

contactRate :: Int
contactRate = 5

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

-- maybe DEPENDENT TYPES a remedy for:
-- Recover event can never occur within a susceptible agent, still we need to check it to cover all cases

-- | A sir agent which is in one of three states
sirAgent :: RandomGen g 
         => SIRState    -- ^ the initial state of the agent
         -> g           -- ^ the initial random-number generator
         -> SIRAgent    -- ^ the continuation
sirAgent Susceptible g0 aid = do
    -- on start
    scheduleEvent aid (Event MakeContact) makeContactInterval
    return $ susceptibleAgent aid g0
sirAgent Infected g0 aid = do
    -- on start
    let (dt, _) = runRand (randomExpM illnessDuration) g0
    scheduleEvent aid (Event Recover) dt
    return $ infectedAgent aid
sirAgent Recovered _ _ = return recoveredAgent

susceptibleAgent :: RandomGen g => AgentId -> g -> SIRAgentCont
susceptibleAgent aid g0 = 
    switch
      susceptibleAgentInfected
      (const $ infectedAgent aid)
  where
    susceptibleAgentInfected :: MSF
                                (ABSMonad SIREvent) -- monadic context
                                (Event SIREvent)    -- input
                                ((), Maybe ())      -- output
    susceptibleAgentInfected = feedback g0 (proc (e, g) ->
      case e of
        Event (Contact _ s) -> 
          if Infected == s
            then do
              let (r, g') = runRand (randomBoolM infectivity) g
              if r 
                then returnA -< (((), Just ()), g')
                else returnA -< (((), Nothing), g')
            else returnA -< (((), Nothing), g)

        Event MakeContact -> do
          ais <- arrM_ allAgentIds -< ()
          let (receivers, g') = runRand (forM [1..contactRate] (const $ randomElem ais)) g
          arrM (mapM_ makeContact) -< receivers
          arrM_ (scheduleEvent aid (Event MakeContact) makeContactInterval) -< ()
          returnA -< (((), Nothing), g')

        -- this will never occur for a susceptible
        Event Recover -> returnA -< (((), Just ()), g)) 

    makeContact :: AgentId -> State (ABSState SIREvent) ()
    makeContact receiver = 
      scheduleEvent 
        receiver 
        (Event (Contact aid Susceptible)) 
        0.01

infectedAgent :: AgentId -> SIRAgentCont
infectedAgent aid = 
    switch 
      infectedAgentRecovered 
      (const recoveredAgent)
  where
    infectedAgentRecovered :: MSF 
                              (ABSMonad SIREvent) -- monadic context
                              (Event SIREvent)    -- input
                              ((), Maybe ())      -- output
    infectedAgentRecovered = proc e ->
      case e of
        Event (Contact sender s) ->
          -- can we somehow replace it with a when
          if Susceptible == s
            then do
              arrM replyContact -< sender
              returnA -< ((), Nothing)
            else returnA -< ((), Nothing)
        -- this will never occur for an infected agent
        Event MakeContact  -> returnA -< ((), Nothing) 
        Event Recover      -> returnA -< Debug.Trace.trace "recovered" ((), Just ())

    replyContact :: AgentId -> State (ABSState SIREvent) ()
    replyContact receiver =
      scheduleEvent 
        receiver 
        (Event (Contact aid Infected)) 
        0.01

recoveredAgent :: SIRAgentCont
recoveredAgent = proc _e -> returnA -< ()

initAgents :: RandomGen g => Int -> Int -> Rand g [SIRAgent]
initAgents n numInfected = do
    susAs <- forM [1..n - numInfected] (const $ createAgent Susceptible)
    infAs <- forM [1..numInfected] (const $ createAgent Susceptible)
    return $ susAs ++ infAs
  where
    createAgent :: RandomGen g => SIRState -> Rand g SIRAgent
    createAgent s = getSplit >>= \g -> return $ sirAgent s g

-- TODO: remove Show e after debugging
runABS :: Show e 
       => [Agent e] 
       -> Integer 
       -> Double
       -> ABSState e -- [ABSOut]
runABS ps steps tLimit = 
    execState (stepClock steps tLimit) s''
  where
    s = ABSState {
      absEvtQueue = PQ.empty
    , absTime     = 0
    , absAgents   = Map.empty
    , absAgentIds = []
    , absEvtCount = 0
    }

    psWIds = Prelude.zipWith (\p pid -> p pid) ps [0..]

    (ps', s') = runState (sequence psWIds) s
    psMap = Prelude.foldr (\(pid, p) acc -> Map.insert pid p acc) Map.empty (Prelude.zip [0..] ps')

    s'' = s' { 
      absAgents   = psMap
    , absAgentIds = Map.keys psMap 
    }
    
-- TODO: remove Show e after debugging
stepClock :: Show e 
          => Integer 
          -> Double
          -> State (ABSState e) ()
stepClock 0 _ = return ()
stepClock n tLimit = do 
  q <- gets absEvtQueue

  -- TODO: use MaybeT 

  let mayHead = PQ.getMin q
  if isNothing mayHead 
    then Debug.Trace.trace "no events, terminating simulation..." (return ())
    else do
      ec <- gets absEvtCount

      let _qi@(QueueItem aid e t') = fromJust mayHead
      --let q' = Debug.Trace.trace ("QueueItem: " ++ show qi) (PQ.drop 1 q)
      let q' = PQ.drop 1 q

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
    
      when
        (t' < tLimit)
        (stepClock (n-1) tLimit)
--(Debug.Trace.trace ("step " ++ show n ++ " t = " ++ show t') (stepClock (n-1) tLimit))
        
allAgentIds :: State (ABSState e) [AgentId]
allAgentIds = gets absAgentIds

scheduleEvent :: AgentId 
              -> Event e
              -> Double
              -> State (ABSState e) ()
scheduleEvent aid e dt = do
  q <- gets absEvtQueue
  t <- gets absTime

  let qe = QueueItem aid e (t + dt)
  let q' = PQ.insert qe q

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