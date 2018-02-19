{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

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
import Data.Maybe
import Data.Text
import qualified Data.Map as Map
import qualified Data.PQueue.Min as PQ
import Control.Monad.Random
import Control.Monad.State.Strict

import Debug.Trace

-- third import: project local
-- none

type ProcId = Int
type Time = Double
data Event = Evt Text 
           | Input deriving Show

-- can we replace this by a MSF as in dunai?
newtype DESProcessCont = Cont (Event -> State DESState DESProcessCont)
type DESProcess = ProcId -> State DESState DESProcessCont

data QueueItem = QueueItem ProcId Event Time deriving Show
type DESQueue = PQ.MinQueue QueueItem

instance Eq QueueItem where
  (==) (QueueItem _ _ t1) (QueueItem _ _ t2) = t1 == t2

instance Ord QueueItem where
  compare (QueueItem _ _ t1) (QueueItem _ _ t2) = compare t1 t2

data DESState = DESState
  { desQueue :: DESQueue
  , desTime  :: Time
  , desProcs :: Map.Map ProcId DESProcessCont -- TODO: replace by array and index by pid: processes are never added or removed
  }

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
  t <- gets desTime
  q <- gets desQueue

  -- TODO: use MaybeT 

  let mayHead = PQ.getMin q
  if isNothing mayHead 
    then trace ("no events, terminating simulation...") (return ())
    else do
      let qi@(QueueItem pid e dt) = fromJust mayHead
      let t' = t + dt
      let q' = trace ("QueueItem: " ++ show qi) (PQ.drop 1 q)
  
      -- modify time and changed queue before running the process
      -- because the process might change the queue 
      modify (\s -> s { 
        desQueue = q' 
      , desTime  = t'
      })

      ps <- gets desProcs
      let (Cont p) = fromJust $ Map.lookup pid ps
      p' <- p e
      let ps' = Map.insert pid p' ps

      modify (\s -> s { desProcs = ps' })

      trace ("step " ++ show n ++ " t = " ++ show t') (runClock (n-1))

source :: RandomGen g => g -> Double -> DESProcess
source g0 arrivalRate pid = do
    -- on start
    g' <- scheduleNextItem g0
    return $ trace ("on start...") (sourceAux g')
  where
    sourceAux :: RandomGen g => g -> DESProcessCont
    sourceAux g = Cont (\case
        Evt _txt -> do
          g' <- scheduleNextItem g
          return $ trace ("on Event Evt...") (sourceAux g')
        Input -> return $ trace ("on Event Input...") (sourceAux g))

    scheduleNextItem :: RandomGen g => g -> State DESState g
    scheduleNextItem g = do
      let (dt, g') = runRand (randomExpM arrivalRate) g
      trace ("scheduleEvent dt = " ++ show dt) (scheduleEvent pid (Evt "NextItem") dt)
      return g'

scheduleEvent :: ProcId -> Event -> Double -> State DESState ()
scheduleEvent pid e dt = do
  q <- gets desQueue

  let qe = QueueItem pid e dt
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