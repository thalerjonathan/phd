{-# LANGUAGE Arrows                #-}
module Main where

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
type EventId = Integer
data EventType e 
  = NewEntity e
  | Pull
 
data Event e = Event EventId (EventType e) deriving Show

data QueueItem e = QueueItem (Event e) Time deriving Show
type DESQueue e = PQ.MinQueue (QueueItem e)

instance Eq (QueueItem e) where
  (==) (QueueItem _ t1) (QueueItem _ t2) = t1 == t2

instance Ord (QueueItem e) where
  compare (QueueItem _ t1) (QueueItem _ t2) = compare t1 t2

data DESState e = DESState
  { desQueue :: DESQueue e
  , desTime  :: Time
  }

type DESMonad e = State (DESState e)

rngSeed :: Int
rngSeed = 42

main :: IO ()
main = do
  let g = mkStdGen rngSeed

  print ""

newtype Client = Client Double

bank :: RandomGen g 
     => g
     -> DESMonad (MSF DESMonad Event Event)
bank g = do
  src <- source g 1.0
  
  return (proc _ -> returnA -< Client 0)

-- | A source emits entities of type e with a given rate
source :: RandomGen g 
       => g                   -- ^ the random-number generator to use
       -> Double              -- ^ the arrival rate
       -> DESMonad (MSF DESMonad () Event)   -- ^ the source is a MSF with no input and outputs e
source _g _arrivalRate = undefined

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

-- | A sink just absorbs entities send to it
-- It receives only the entities and has no output
sink :: MSF DESMonad Event ()
sink = undefined
