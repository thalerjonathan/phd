{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad.Random
import Control.Monad.Reader
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Data.IntMap.Strict as Map 
import Data.Maybe

import SIR.Model
import SIR.Event
import SIR.SD
import Utils.GenEventSIR
import Utils.GenTimeSIR
import Utils.GenSIR
import Utils.Numeric
import Utils.Stats

import Debug.Trace

-- --quickcheck-replay=557780
-- --quickcheck-tests=1000
-- --quickcheck-verbose
-- --test-arguments=""
-- clear & stack test sir:sir-invariants-tests

main :: IO ()
main = do
  let t = testGroup "SIR Invariant Tests" 
          [ 
            QC.testProperty "SIR SD invariant" prop_sir_sd_invariants
          , QC.testProperty "SIR event-driven invariant" prop_sir_event_invariants
          , QC.testProperty "SIR event-driven random event sampling invariant" prop_sir_random_invariants
          , QC.testProperty "SIR time-driven invariant" prop_sir_time_invariants
          , QC.testProperty "SIR time- and event-driven distribution" prop_sir_event_time_equal
          ]

  defaultMain t

--------------------------------------------------------------------------------
-- SIMULATION INVARIANTS
--------------------------------------------------------------------------------
prop_sir_event_invariants :: Positive Int    -- ^ beta, contact rate
                          -> Probability     -- ^ gamma, infectivity, within (0,1) range
                          -> Positive Double -- ^ delta, illness duration
                          -> [SIRState]      -- ^ population
                          -> Property
prop_sir_event_invariants (Positive cor) (P inf) (Positive ild) as = checkCoverage $ do
  -- total agent count
  let n = length as

  -- run simulation UNRESTRICTED in both time and event count
  ret <- genEventSIR as cor inf ild (-1) (1/0)
  
  -- after a finite number of steps SIR will reach equilibrium, when there
  -- are no more infected agents. WARNING: this could be a potentially non-
  -- terminating computation but a correct SIR implementation will always
  -- lead to a termination of this
  let equilibriumData = takeWhile ((>0).snd3.snd) ret

  return (sirInvariants n equilibriumData)

prop_sir_time_invariants :: Positive Double -- ^ beta, contact rate
                         -> Probability     -- ^ gamma, infectivity, within (0,1) range
                         -> Positive Double -- ^ delta, illness duration
                         -> [SIRState]      -- ^ population
                         -> Property
prop_sir_time_invariants (Positive cor) (P inf) (Positive ild) as = property $ do
  -- total agent count
  let n = length as

  let dt = 0.1
  -- run simulation UNRESTRICTED TIME
  ret <- genTimeSIR as cor inf ild dt 0

  -- after a finite number of steps SIR will reach equilibrium, when there
  -- are no more infected agents. WARNING: this could be a potentially non-
  -- terminating computation but a correct SIR implementation will always
  -- lead to a termination of this 
  let equilibriumData = takeWhile ((>0).snd3.snd) ret

  return (sirInvariants n equilibriumData)

prop_sir_sd_invariants :: Positive Double -- ^ Susceptible agents
                       -> Positive Double -- ^ Infected agents
                       -> Positive Double -- ^ Recovered agents
                       -> Positive Double -- ^ Random beta, contact rate
                       -> Probability     -- ^ Random gamma, infectivity, within (0,1) range
                       -> Positive Double -- ^ Random delta, illness duration
                       -> Positive Double -- ^ Random time
                       -> Bool
prop_sir_sd_invariants (Positive s) (Positive i) (Positive r) 
                       (Positive cor) (P inf) (Positive ild) 
                       (Positive t)
      = sirInvariantsFloating (s + i + r) ret
  where
    -- NOTE: due to SD continuous nature it will take basically FOREVER to reach
    -- an infected of 0 => we always limit the duration but we do it randomly
    ret = runSIRSD s i r cor inf ild t

prop_sir_random_invariants :: Positive Int    -- ^ beta, contact rate
                           -> Probability     -- ^ gamma, infectivity, within (0,1) range
                           -> Positive Double -- ^ delta, illness duration
                           -> NonEmptyList SIRState -- ^ population
                           -> Property
prop_sir_random_invariants (Positive cor) (P inf) (Positive ild) (NonEmpty as) = property $ do
  -- total agent count
  let n = length as
  -- number of random events to generate
  let eventCount = 10000
  -- run simulation with random population and random events
  ret <- genRandomEventSIR as cor inf ild eventCount

  return (sirInvariants n ret)

sirInvariants :: Int -> [(Time, (Int, Int, Int))] -> Bool
sirInvariants n aos = timeInc && aConst && susDec && recInc && infInv
  where
    (ts, sirs)  = unzip aos
    (ss, _, rs) = unzip3 sirs

    -- 1. time is monotonic increasing
    timeInc = mono (<=)  ts
    -- 2. number of agents N stays constant in each step
    aConst = all agentCountInv sirs
    -- 3. number of susceptible S is monotonic decreasing
    susDec = mono (>=) ss
    -- 4. number of recovered R is monotonic increasing
    recInc = mono (<=)  rs
    -- 5. number of infected I = N - (S + R)
    infInv = all infectedInv sirs

    agentCountInv :: (Int, Int, Int) -> Bool
    agentCountInv (s,i,r) = s + i + r == n

    infectedInv :: (Int, Int, Int) -> Bool
    infectedInv (s,i,r) = i == n - (s + r)

    mono :: (Ord a, Num a) => (a -> a -> Bool) -> [a] -> Bool
    mono f xs = all (uncurry f) (pairs xs)

    pairs :: [a] -> [(a,a)]
    pairs xs = zip xs (tail xs)

-- NOTE: invariants under floating-point are much more difficult to get right
-- because we are comparing floating point values which are evil anyway.
-- We removed infected invariant due to subtraction operation which f*** up
-- things with floating point, so we remove this propery as it follows from
-- the other ones anyway (even if it gets violated in this case)
sirInvariantsFloating :: Double -> [(Time, (Double, Double, Double))] -> Bool
sirInvariantsFloating n aos = timeInc && aConst && susDec && recInc
  where
    epsilon     = 0.0001
    (ts, sirs)  = unzip aos
    (ss, _, rs) = unzip3 sirs

    -- 1. time is monotonic increasing
    timeInc = mono (<=) ts
    -- 2. number of agents N stays constant in each step
    aConst = all agentCountInv sirs
    -- 3. number of susceptible S is monotonic decreasing
    susDec = mono (>=) ss
    -- 4. number of recovered R is monotonic increasing
    recInc = mono (<=) rs

    agentCountInv :: (Double, Double, Double) -> Bool
    agentCountInv (s,i,r) = compareDouble n (s + i + r) epsilon
    
    mono :: (Ord a, Num a) => (a -> a -> Bool) -> [a] -> Bool
    mono f xs = all (uncurry f) (pairs xs)

    pairs :: [a] -> [(a,a)]
    pairs xs = zip xs (tail xs)

-- NOTE: need to use mann whitney because both produce bi-modal distributions
-- thus t-test does not work because it assumes normally distributed samples
prop_sir_event_time_equal :: Positive Int    -- ^ Random beta, contact rate
                          -> Probability     -- ^ Random gamma, infectivity, within (0,1) range
                          -> Positive Double -- ^ Random delta, illness duration
                          -> TimeRange       -- ^ Random time to run, within (0, 50) range)
                          -> [SIRState]      -- ^ Random population
                          -> Property
prop_sir_event_time_equal
    (Positive cor) (P inf) (Positive ild) (T t) as = checkCoverage $ do
  -- run 100 replications
  let repls = 100
 
  -- run 100 replications for time- and event-driven simulation
  (ssTime, isTime, rsTime) <- 
    unzip3 . map int3ToDbl3 <$> genTimeSIRRepls repls as (fromIntegral cor) inf ild 0.01 t
  (ssEvent, isEvent, rsEvent) <- 
    unzip3 . map int3ToDbl3 <$> genEventSIRRepls repls as cor inf ild (-1) t
  
  let p = 0.05

  let ssTest = mannWhitneyTwoSample ssTime ssEvent p
      isTest = mannWhitneyTwoSample isTime isEvent p
      rsTest = mannWhitneyTwoSample rsTime rsEvent p

  let allPass = fromMaybe True ssTest &&
                fromMaybe True isTest &&
                fromMaybe True rsTest 

  return $ trace (show allPass) 
    cover 90 allPass "SIR event- and time-driven produce equal distributions" True

-- NOTE: all these properties are already implicitly checked in the agent 
-- specifications and sir invariants
-- > Recovered Agent generates no events and stays recovered FOREVER. This means:
--  pre-condition:   in Recovered state and ANY event
--  post-condition:  in Recovered state and 0 scheduled events
-- > Susceptible Agent MIGHT become Infected and Recovered
-- > Infected Agent will NEVER become Susceptible and WILL become Recovered

--------------------------------------------------------------------------------
-- CUSTOM GENERATOR, ONLY RELEVANT TO STATEFUL TESTING 
--------------------------------------------------------------------------------
genRandomEventSIR :: [SIRState]
                  -> Int
                  -> Double
                  -> Double 
                  -> Integer
                  -> Gen [(Time, (Int, Int, Int))]
genRandomEventSIR as cor inf ild maxEvents = do
    g <- genStdGen 

    -- ignore initial events
    let (am0, _) = evalRand (initSIR as cor inf ild) g
        ais = Map.keys am0

    -- infinite stream of events, prevents us from calling genQueueItem in
    -- the execEvents function - lazy evaluation is really awesome!
    evtStream <- genQueueItemStream 0 ais
    
    return $ evalRand (runReaderT (executeEvents maxEvents evtStream am0) ais) g
  where
    executeEvents :: RandomGen g
                  => Integer
                  -> [QueueItem SIREvent]
                  -> AgentMap (SIRMonad g) SIREvent SIRState
                  -> ReaderT [AgentId] (Rand g) [(Time, (Int, Int, Int))]
    executeEvents 0 _ _  = return []
    executeEvents _ [] _ = return []
    executeEvents n (evt:es) am = do
      retMay <- processEvent am evt 
      case retMay of 
        Nothing -> executeEvents (n-1) es am
        -- ignore events produced by agents
        (Just (am', _)) -> do
          let s = (eventTime evt, aggregateAgentMap am)
          ss <- executeEvents (n-1) es am'
          return (s : ss)

--------------------------------------------------------------------------------
-- UTILS
--------------------------------------------------------------------------------
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b