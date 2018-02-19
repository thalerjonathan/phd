module Streams where

import           Control.Monad.Random
import           Control.Monad.State.Strict
import           Data.MonadicStreamFunction
import qualified Data.PQueue.Min as PQ
import           Data.Text

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
newtype Event = Evt Text deriving Show

data QueueItem = QueueItem Event Time deriving Show
type DESQueue = PQ.MinQueue QueueItem

instance Eq QueueItem where
  (==) (QueueItem _ t1) (QueueItem _ t2) = t1 == t2

instance Ord QueueItem where
  compare (QueueItem _ t1) (QueueItem _ t2) = compare t1 t2

data DESState = DESState
  { desQueue :: DESQueue
  , desTime  :: Time
  }

type DESMonad = State DESState

rngSeed :: Int
rngSeed = 42

runStreams :: IO ()
runStreams = do
  let g = mkStdGen rngSeed

  let _src = source g 1.0
  let _snk = sink

  let _sim = _src >>> _snk

  print ""

-- | A source emits entities of type e with a given rate
source :: RandomGen g 
       => g                   -- ^ the random-number generator to use
       -> Double              -- ^ the arrival rate
       -> MSF DESMonad () e   -- ^ the source is a MSF with no input and outputs e
source _arrivalRate = undefined

-- | A sink just absorbs entities send to it
-- It receives only the entities and has no output
sink :: MSF DESMonad e ()
sink = undefined