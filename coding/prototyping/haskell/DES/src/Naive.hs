{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE Arrows     #-}

module Naive 
  (
    runNaive
  ) where

{-
MultiWayIf
NamedFieldPuns
TupleSections
-}

-- first import: base
-- none

-- second import: 3rd party libraries

import           Control.Monad.Random
import           Control.Monad.State.Strict
import           Data.Maybe
import           Data.MonadicStreamFunction
import qualified Data.Map as Map
import qualified Data.PQueue.Min as PQ
import           Data.Text
--import           Data.Vector
import           Debug.Trace


-- third import: project local
-- none

-- all DES primitives (source, sink, queue, service, delay,...) must have some mechanism
-- to store some statistics about their internals e.g. throughput, time in the system
-- => store it in the DES monad?

type ProcId = Int
type Time = Double
data Event = Evt Text 
           | Input deriving Show

data QueueItem = QueueItem ProcId Event Time deriving Show
type DESQueue = PQ.MinQueue QueueItem

instance Eq QueueItem where
  (==) (QueueItem _ _ t1) (QueueItem _ _ t2) = t1 == t2

instance Ord QueueItem where
  compare (QueueItem _ _ t1) (QueueItem _ _ t2) = compare t1 t2

data DESState = DESState
  { desQueue :: DESQueue
  , desTime  :: Time
  , desProcs :: Map.Map ProcId DESProcessCont -- replace by mutable vector
  }

--newtype DESProcessCont = Cont (Event -> State DESState DESProcessCont)
type DESMonad = State DESState
type DESProcessCont = MSF DESMonad Event ()
type DESProcess = ProcId -> State DESState DESProcessCont

rngSeed :: Int
rngSeed = 42

runNaive :: IO ()
runNaive = do
  let g = mkStdGen rngSeed
  let processSource = source g 1.0

  let s = runDES [processSource] 100

  print $ desTime s

runDES :: [DESProcess] -> Integer -> DESState
runDES ps steps = execState (runClock steps) s' { desProcs = psMap }
  where
    s = DESState {
      desQueue = PQ.empty
    , desTime  = 0
    , desProcs = Map.empty
    }

    psWIds = Prelude.zipWith (\p pid -> p pid) ps [0..]

    (ps', s') = runState (sequence psWIds) s
    psMap = Prelude.foldr (\(pid, p) acc -> Map.insert pid p acc) Map.empty (Prelude.zip [0..] ps')

runClock :: Integer -> State DESState ()
runClock 0 = return ()
runClock n = do 
  q <- gets desQueue

  -- TODO: use MaybeT 

  let mayHead = PQ.getMin q
  if isNothing mayHead 
    then Debug.Trace.trace "no events, terminating simulation..." (return ())
    else do
      let qi@(QueueItem pid e t') = fromJust mayHead
      let q' = Debug.Trace.trace ("QueueItem: " ++ show qi) (PQ.drop 1 q)
  
      -- modify time and changed queue before running the process
      -- because the process might change the queue 
      modify (\s -> s { 
        desQueue = q' 
      , desTime  = t'
      })

      ps <- gets desProcs
      let p = fromJust $ Map.lookup pid ps
      (_, p') <- unMSF p e
      let ps' = Map.insert pid p' ps

      modify (\s -> s { desProcs = ps' })

      Debug.Trace.trace ("step " ++ show n ++ " t = " ++ show t') (runClock (n-1))

-- | A source emits entities of type e with a given rate
source :: RandomGen g 
       => g             -- ^ the random-number generator to use
       -> Double        -- ^ the arrival rate
       -> DESProcess    -- ^ the source is a MSF with no input and outputs e
source g0 arrivalRate pid = do
    -- on start
    g' <- scheduleNextItem g0
    return $ Debug.Trace.trace "on start..." (sourceAux g')
  where
    sourceAux :: RandomGen g => g -> DESProcessCont
    sourceAux g = feedback g sourceFeed

    sourceFeed :: RandomGen g => MSF DESMonad (Event, g) ((), g)
    sourceFeed = proc (e, g) -> case e of
        Evt _txt -> do
          g' <- arrM scheduleNextItem -< g
          returnA -< Debug.Trace.trace "on Event Evt..." ((), g')
        Input -> returnA -< Debug.Trace.trace "on Event Input..." ((), g)

    scheduleNextItem :: RandomGen g => g -> State DESState g
    scheduleNextItem g = do
      let (dt, g') = runRand (randomExpM arrivalRate) g
      Debug.Trace.trace ("scheduleEvent dt = " ++ show dt) (scheduleEvent pid (Evt "NextItem") dt)
      return g'

-- | A sink just absorbs entities send to it
-- It receives only the entities and has no output
sink :: DESProcess
sink = undefined

scheduleEvent :: ProcId -> Event -> Double -> State DESState ()
scheduleEvent pid e dt = do
  q <- gets desQueue
  t <- gets desTime

  let qe = QueueItem pid e (t + dt)
  let q' = PQ.insert qe q

  modify (\s -> s { desQueue = q' })

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