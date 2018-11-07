{-# LANGUAGE Arrows                #-}
module Main where

import           Data.Maybe

import           Control.Monad.Random
import           Control.Monad.State.Strict
import           Data.MonadicStreamFunction
import qualified Data.PQueue.Min as PQ

import Debug.Trace as DBG

-- TODO:
-- use dunai
-- propagate entities through the network
-- entities are emited through events and passed on to the next process through outputs of the MSF
-- events are scheduled in the DES monad
-- design a DES monad: can schedule events and read current time

-- all DES primitives (source, sink, queue, service, delay,...) must have some mechanism
-- to store some statistics about their internals e.g. throughput, time in the system
-- => store it in the DES monad?

-- TODO: central problem is how to to direct an event to an MSF: events must start
-- at the beginning of the DES network and are propagated through it. Each component
-- MUST be isolated from each other and not carying about where it is in the network,
-- it just cares about its interface => cant assume anything about global ids or anything
-- (would work but is a dirty hack). Solution: count hops an event takes allows an
-- component to identify where it is

type Time    = Double
type DTime   = Double
type CompId  = Integer

data EventType e 
  = NewEntity CompId
  | ForwardEntity e
  | Consumed
  | Occupied
  | Undelay CompId
  deriving Show

newtype Event e = Event (EventType e) deriving Show

data QueueItem e = QueueItem (Event e) Time deriving Show
type DESQueue e  = PQ.MinQueue (QueueItem e)

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

type DESMonad e s     = State (DESState e s)
type DESComponent e s = MSF (DESMonad e s) (Event e) (Event e)

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
    -- TODO: find a better way of assigning component ids than hard-coding them from the beginning
    src <- source 1 g 15.0 clientCreate
    let snk = sink clientSink
        del = Main.delay
        q   = queue 10 (del >>> snk)
        
        net = src >>> q  

    return net 
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

-- | A source emits entities of type e with a given rate
source :: RandomGen g 
       => CompId
       -> g                  -- ^ the random-number generator to use
       -> DTime              -- ^ the arrival rate
       -> DESMonad e s e
       -> DESMonad e s (DESComponent e s)   -- ^ the source is a MSF with no input and outputs e
source myCompId gInit arrivalRate es0 = do
    g <- scheduleNextArrival gInit
    return $ sourceAux g es0
  where
    sourceAux :: RandomGen g 
              => g
              -> DESMonad e s e
              -> DESComponent e s
    sourceAux g0 entitySource = proc evt@(Event et) -> 
      case et of
        NewEntity cid -> 
          if myCompId /= cid 
          then returnA -< evt
          else do
            rec
              g1 <- iPre g0 -< g2
              g2 <- arrM scheduleNextArrival -< g1
            e <- arrM_ entitySource -< ()
            returnA -< DBG.trace "emitting new entity" (Event (ForwardEntity e))
        _               -> returnA -< evt

    scheduleNextArrival :: RandomGen g 
                        => g
                        -> DESMonad e s g
    scheduleNextArrival g = do
      let (dt, g') = runRand (randomExpM (1 / arrivalRate)) g
      scheduleEvent (NewEntity myCompId) dt
      return g'

-- | Stores entities in the specified order.
queue :: Int          -- ^ the size of the queue, when overflow system will exit with error
      -> DESComponent e s -- ^ the connection to the queue, this is necessary because a queue needs to forward entities when the connector is 
      -> MSF (DESMonad e s) (Event e) ()
queue queueSize nextComp = DBG.trace "Queue: init" $ feedback [] (proc (evt@(Event et), q) -> 
  case et of
    ForwardEntity e -> do
      (Event ret) <- nextComp -< DBG.trace "Queue: forwarding ForwardEntity event..." evt

      case ret of
        Consumed -> returnA -< DBG.trace "Queue: received ForwardEntity and forwarded to next comp, received Consumed" ((), q)
        _        -> 
          if length q == queueSize
            then returnA -< error "Queue full" --(Event (Error "Queue already full"), q)
            else returnA -< DBG.trace "Queue: forward couldnt consume it => queue has space => enqueuing" ((), q ++ [e])

    -- other cases
    _               -> do
      (Event ret) <- nextComp -< DBG.trace "Queue: forwarding unkown event..." evt

      case ret of
        Consumed -> 
          if null q
            then returnA -< DBG.trace "Queue: received non forwardentity and forwarded to next comp, queue empty"  ((), q)
            else do
              _ <- nextComp -< (Event $ ForwardEntity (head q))
              returnA -< DBG.trace "Queue: dequeued element and forwarded" ((), tail q)
        _        -> returnA -< DBG.trace "Queue: just forwarded" ((), q))

delay :: DESComponent e s 
delay = DBG.trace "Delay: init" $ feedback Nothing (proc (evt@(Event et), me) -> 
  case et of
    ForwardEntity e -> 
      case me of
        Nothing -> do
          arrM (uncurry scheduleEvent) -< (Undelay 0, 5)
          returnA -< DBG.trace "Delay: received ForwardEntity, and delay is unoccupied, will delay it..." (Event Consumed, Just e)
        Just _  -> returnA -< DBG.trace "Delay: received ForwardEntity but delay is occupied, will not delay it..." (Event Occupied, me)
    Undelay _ ->
      case me of
        -- still have a bug, dunno why this occurs
        Nothing -> returnA -< error "Delay: received Undelay but cant undelay with empty delay!"
        Just e  -> returnA -< DBG.trace "Delay: received Undelay, and delay is occupied, will forward the entity and become unoccupied..." (Event $ ForwardEntity e, Nothing)
    _ -> returnA -< DBG.trace "Delay: received unknown event, just forward and stay as delay is..."  (evt, me))

-- | A sink just absorbs entities send to it
-- It receives only the entities and has no output
sink :: (e -> DESMonad e s ()) 
     -> DESComponent e s
sink sinkAction = proc evt@(Event et) -> 
  case et of
    ForwardEntity e -> do
      arrM sinkAction -< e
      returnA -< DBG.trace "Sink: received ForwardEntity, is sinked." (Event Consumed)
    _               -> returnA -< DBG.trace "Sink: received unknown entity, ignore it" evt

scheduleEvent :: EventType e
              -> DTime
              -> DESMonad e s ()
scheduleEvent et dt = do
  q <- gets desQueue
  t <- gets desTime

  let evt = Event et 

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


          (_, desMsf') <- DBG.trace "Kernel: unMSF" (unMSF desMsf e)
          
          when 
            (t' < tEnd)
            (stepClock desMsf')

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