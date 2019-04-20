{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad.Random
import Control.Monad.Reader
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Data.IntMap.Strict as Map 

import SIR.SIR
import SIRGenerators

-- clear & stack test sir-event:sir-invariants-test --test-arguments="--quickcheck-replay=557780 --quickcheck-verbose"

main :: IO ()
main = do
  let t = testGroup "SIR Invariants Tests" 
          [ 
            QC.testProperty "SIR simulation invariants" prop_sir_simulation_invariants
          , QC.testProperty "SIR random event sampling invariants" prop_sir_random_invariants
          ]

  defaultMain t

--------------------------------------------------------------------------------
-- SIMULATION INVARIANTS
--------------------------------------------------------------------------------
prop_sir_simulation_invariants :: Property
prop_sir_simulation_invariants = property $ do
  let cor = 5     -- beta, contact rate
      inf = 0.05  -- gamma, infectivitry
      ild = 15    -- delta, illness duration

  -- generate population with size of up to 1000
  as <- resize 1000 (listOf genSIRState)
  -- total agent count
  let n = length as

  -- run simulation UNRESTRICTED in both time and event count
  ret <- genSimulationSIR as cor inf ild (-1) (1/0)
  
  -- after a finite number of steps SIR will reach equilibrium, when there
  -- are no more infected agents. WARNING: this could be a potentially non-
  -- terminating computation but a correct SIR implementation will always
  -- lead to a termination of this 
  let equilibriumData = takeWhile ((>0).snd3.snd) ret

  return (sirInvariants n equilibriumData)

prop_sir_random_invariants :: Property
prop_sir_random_invariants = property $ do
  let cor = 5     -- beta, contact rate
      inf = 0.05  -- gamma, infectivitry
      ild = 15    -- delta, illness duration

  -- generate random population with size of up to 1000
  as <- resize 1000 (listOf genSIRState)
  -- total agent count
  let n = length as
  -- number of infected at t=0
  let i0 = length (filter (==Infected) as)
  -- number of random events to generate
  let eventCount = 100000
  -- run simulation with random population and random events
  ret <- genRandomEventSIR as cor inf ild eventCount

  let equilibrium = any ((>0).snd3.snd) ret

  return $ cover 90 (i0 > 0 && equilibrium) 
            "Random event sampling reached equilibrium" 
              (sirInvariants n ret)

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
genRandomEventSIR as cr inf illDur maxEvents = do
    g <- genStdGen 

    -- ignore initial events
    let (am0, _) = evalRand (initSIR as cr inf illDur) g
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