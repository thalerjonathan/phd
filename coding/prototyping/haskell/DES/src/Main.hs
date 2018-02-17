{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

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

-- third import: project local
-- none

type ProcId = Int
type Time = Double
data Event = Evt Text | Input 

-- can we replace this by a MSF as in dunai?
newtype DESProcessCont = Cont (Event -> State DESState DESProcessCont)
type DESProcess = ProcId -> State DESState DESProcessCont

data QueueItem = QueueItem ProcId Event Time 
type DESQueue = PQ.MinQueue QueueItem

instance Eq QueueItem where
  (==) (QueueItem _ _ t1) (QueueItem _ _ t2) = t1 == t2

instance Ord QueueItem where
  compare (QueueItem _ _ t1) (QueueItem _ _ t2) = compare t1 t2

data DESState = DESState
  { desQueue :: DESQueue
  , desTime  :: Time
  , desProcs :: Map.Map ProcId DESProcessCont
  }

main :: IO ()
main = do
  let _q = initQueue
  putStrLn ""

initQueue :: PQ.MinQueue QueueItem
initQueue = PQ.empty

source :: RandomGen g => g -> Double -> DESProcess
source g0 arrivalRate pid = do
    -- on start
    g' <- scheduleNextItem g0
    return $ sourceAux g'
  where
    sourceAux :: RandomGen g => g -> DESProcessCont
    sourceAux g = Cont (\case
        Evt _txt -> do
          g' <- scheduleNextItem g
          return $ sourceAux g'
        Input -> return $ sourceAux g)

    scheduleNextItem :: RandomGen g => g -> State DESState g
    scheduleNextItem g = do
      let (dt, g') = runRand (randomExpM arrivalRate) g
      scheduleEvent pid (Evt "NextItem") dt 
      return g'

scheduleEvent :: ProcId -> Event -> Double -> State DESState ()
scheduleEvent pid e dt = do
  t <- gets desTime
  q <- gets desQueue
  s <- get 

  let qe = QueueItem pid e (t + dt)
  let q' = PQ.insert qe q

  put s { desQueue = q' }

runDES :: [DESProcess] -> DESState
runDES ps = execState runClock s' { desProcs = psMap }
  where
    s = DESState {
      desQueue = PQ.empty
    , desTime  = 0
    , desProcs = Map.empty
    }

    (ps', s') = runState (sequence ps) s
    psMap = Prelude.foldr (\(pid, p) acc -> Map.insert pid p acc) Map.empty (Prelude.zip ([0..], ps'))

runClock :: State DESState ()
runClock = do 
  t <- gets desTime
  q <- gets desQueue

  -- TODO: use MaybeT 

  let mayHead = PQ.getMin q
  if isNothing mayHead 
    then return ()
    else do
      let (QueueItem pid e dt) = fromJust mayHead
      let t' = t + dt
      let q' = PQ.drop 1 q
      
      ps <- gets desProcs

      let (Cont p) = fromJust $ Map.lookup pid ps
      p' <- p e

      s <- get

      let ps' = Map.insert pid p' ps

      put s { 
        desQueue = q' 
      , desTime  = t'
      , desProcs = ps'
      }

      runClock

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