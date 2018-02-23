{-# LANGUAGE Arrows                #-}
module Main where

import           Data.Maybe

import           Control.Monad.Random
import           Control.Monad.State.Strict
import           Data.MonadicStreamFunction
import qualified Data.PQueue.Min as PQ

-- TODO:
-- use dunai
-- propagate entities through the network
-- entities are emited through events and passed on to the next process through outputs of the MSF
-- events are scheduled in the DES monad
-- design a DES monad: can schedule events and read current time

-- all DES primitives (source, sink, queue, service, delay,...) must have some mechanism
-- to store some statistics about their internals e.g. throughput, time in the system
-- => store it in the DES monad?

type Time = Double
type EventId = Integer
data EventType e 
  = ForwardEntity e
  | Pull
  deriving Show
 
data Event e = Event EventId (EventType e) deriving Show

data QueueItem e = QueueItem (Event e) Time deriving Show
type DESQueue e = PQ.MinQueue (QueueItem e)

instance Eq (QueueItem e) where
  (==) (QueueItem _ t1) (QueueItem _ t2) = t1 == t2

instance Ord (QueueItem e) where
  compare (QueueItem _ t1) (QueueItem _ t2) = compare t1 t2

data DESState e s = DESState
  { desQueue  :: DESQueue e
  , desTime   :: Time
  , desEvtCnt :: Integer
  , desData   :: s
  }

type DESMonad e s = State (DESState e s)

rngSeed :: Int
rngSeed = 42

main :: IO ()
main = do
  let g = mkStdGen rngSeed

  let (ec, t, dts) = runDES (bank g) [] 100000.0

  let avg = sum dts / fromIntegral (length dts)

  print $ "Final t = " ++ show t ++ ", event count = " ++ show ec
  print $ "Average time in system: " ++ show avg

newtype Client = Client Double
type BankState = [Time]

bank :: RandomGen g 
     => g
     -> DESMonad Client BankState (MSF (DESMonad Client BankState) (Event Client) ())
bank g = do
    src <- source g 15.0 clientCreate
    let snk = sink clientSink
    let q = queue 10
    let del = Main.delay

    return (src >>> q >>> del >>> snk)
  where
    clientCreate :: DESMonad Client BankState Client
    clientCreate = do
      t <- gets desTime
      return $ Client t

    clientSink :: Client -> DESMonad Client BankState ()
    clientSink (Client ct) = do
      dts <- gets desData

      t <- gets desTime
      let dt = t - ct

      modify (\s -> s { desData = dt : dts })

runDES :: DESMonad e s (MSF (DESMonad e s) (Event e) ())
       -> s
       -> Time
       -> (Integer, Time, s)
runDES desM s0 tEnd = (desEvtCntFinal, desTimeFinal, desDataFinal)
  where
    des0 = DESState { 
      desQueue  = PQ.empty
    , desTime   = 0
    , desEvtCnt = 0
    , desData   = s0
    }

    (desMsf0, des') = runState desM des0
    desFinal = execState (stepClock desMsf0) des'

    desDataFinal  = desData desFinal
    desTimeFinal   = desTime desFinal
    desEvtCntFinal = desEvtCnt desFinal

    stepClock :: MSF (DESMonad e s) (Event e) () 
              -> DESMonad e s ()
    stepClock desMsf = do
      q <- gets desQueue

      let mayHead = PQ.getMin q
      if isNothing mayHead 
        then return ()
        else do
          let (QueueItem e t') = fromJust mayHead
          let q' = PQ.drop 1 q

          -- modify time and changed queue before running the process
          -- because the process might change the queue 
          modify (\s -> s { 
            desQueue  = q' 
          , desTime   = t'
          , desEvtCnt = desEvtCnt s + 1
          })

          (_, desMsf') <- unMSF desMsf e
          
          when 
            (t' < tEnd)
            (stepClock desMsf')

-- | A source emits entities of type e with a given rate
source :: RandomGen g 
       => g                   -- ^ the random-number generator to use
       -> Double              -- ^ the arrival rate
       -> DESMonad e s e
       -> DESMonad e s (MSF (DESMonad e s) (Event e) (Event e))   -- ^ the source is a MSF with no input and outputs e
source gInit arrivalRate es0 = do
    g <- scheduleNextArrival es0 gInit
    return $ sourceAux g es0
  where
    sourceAux :: RandomGen g 
              => g
              -> DESMonad e s e
              -> MSF (DESMonad e s) (Event e) (Event e)
    sourceAux g0 es = proc evt -> do
      rec
        g1 <- iPre g0 -< g2
        g2 <- arrM (scheduleNextArrival es) -< g1
      returnA -< evt

    scheduleNextArrival :: RandomGen g 
                        => DESMonad e s e
                        -> g
                        -> DESMonad e s g
    scheduleNextArrival entitySource g = do
      let (dt, g') = runRand (randomExpM (1 / arrivalRate)) g
      e <- entitySource
      scheduleEvent (ForwardEntity e) dt
      return g'

-- | Stores entities in the specified order.
queue :: Integer -> MSF (DESMonad e s) (Event e) (Event e)
queue _size = undefined

delay :: MSF (DESMonad e s) (Event e) (Event e)
delay = undefined

-- | A sink just absorbs entities send to it
-- It receives only the entities and has no output
sink :: (e -> DESMonad e s ()) 
     -> MSF (DESMonad e s) (Event e) ()
sink sinkAction = proc (Event _eid et) -> 
  case et of
    ForwardEntity e -> arrM sinkAction -< e
    _               -> returnA -< ()

scheduleEvent :: EventType e
              -> Double
              -> DESMonad e s ()
scheduleEvent et dt = do
  q <- gets desQueue
  t <- gets desTime

  let evt = Event 0 et 

  let qe = QueueItem evt (t + dt)
  let q' = PQ.insert qe q

  modify (\s -> s { desQueue = q' })



{-
-- | A service seizes resource units for the entity, delays it, and releases the seized units.
-- For now it is assumed that each entity 1 ressources is required
service :: RandomGen g
        => g            -- ^ the random-number generator to use
        -> Int          -- ^ the number of ressources available to the service
        -> MSF DESMonad Event Event
service _g _n = undefined

-- | Forwards the entity to one of the output ports depending on the condition.
selectOutputProb :: RandomGen g
                 => g 
                 -> MSF DESMonad Event (Maybe Event, Maybe Event)
selectOutputProb _g = undefined

-- | Stores entities in the specified order.
queue :: MSF DESMonad Event Event
queue = undefined
-}

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

-- taken from https://stackoverflow.com/questions/33220176/triangular-distribution-in-java
randomTriM :: RandomGen g 
           => Double
           -> Double
           -> Double
           -> Rand g Double
randomTriM a b c = do
  let f = (c - a) / (b - a)
  r <- getRandomR (0, 1)

  if r < f
    then return $ a + sqrt r * (b - a) * (c - a)
    else return $ b - sqrt (1 - r) * (b - a) * (b - c)