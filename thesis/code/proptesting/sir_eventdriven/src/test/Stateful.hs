{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad.Random
import Control.Monad.Reader
-- import Control.Monad.Writer
-- import Data.MonadicStreamFunction.InternalCore
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Data.IntMap.Strict as Map 

import SIR.SIR
import SIRGenerators

-- represent the testing state 
data AgentTestState g = AgentTestState
  { rng      :: g
  , agent    :: SIRAgentCont g
  , time     :: Time
  , agentIds :: [AgentId]
  }

-- the api of the agent is represented by the QueueItem SIREvent
type Command = QueueItem SIREvent
-- the output of an agent is its current SIRState and the events it has scheduled
data Response = Resp SIRState [QueueItem SIREvent]

-- clear & stack test sir-event:sir-stateful-test --test-arguments="--quickcheck-replay=557780 --quickcheck-verbose"

main :: IO ()
main = do
  let t = testGroup "SIR Stateful Tests" 
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
  ss <- resize 1000 (listOf genSIRState)
  -- total agent count
  let n = length ss

  -- run simulation UNRESTRICTED in both time and event count
  ret <- genSimulationSIR ss cor inf ild (-1) (1/0)
  
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
  ss <- resize 1000 (listOf genSIRState)
  -- total agent count
  let n = length ss
  -- number of infected at t=0
  let i0 = length (filter (==Infected) ss)
  -- number of random events to generate
  let eventCount = 10000
  -- run simulation with random population and random events
  ret <- genRandomEventSIR ss cor inf ild eventCount

  let equilibrium = any ((>0).snd3.snd) ret

  return $ cover 90 (i0 > 0 && equilibrium) 
            "Random event sampling reached equilibrium" 
              (sirInvariants n ret)

sirInvariants :: Int -> [(Time, (Int, Int, Int))] -> Bool
sirInvariants n aos = aci && susInc && infConst && recDec && timeInc
  where
    (ts, sirs) = unzip aos

    -- number of agents stays constant in each step
    aci = all agentCountInvariant sirs
    -- number of susceptible can only decrease
    susInc = monotonousDecreasing (fst3 $ unzip3 sirs)
    -- number of infected i = N - (S + R)
    infConst = all infectedInvariant sirs
    -- number of recovered R can only increase
    recDec = monotonousIncrasing (trd3 $ unzip3 sirs)
    -- time is monotonously increasing
    timeInc = monotonousIncrasing ts

    agentCountInvariant :: (Int, Int, Int) -> Bool
    agentCountInvariant (s,i,r) = s + i + r == n

    infectedInvariant :: (Int, Int, Int) -> Bool
    infectedInvariant (s,i,r) = i == n - (s + r)

    monotonousDecreasing :: (Ord a, Num a) => [a] -> Bool
    monotonousDecreasing xs = and [ x' <= x | (x, x') <- zip xs (tail xs) ]

    monotonousIncrasing :: (Ord a, Num a) => [a] -> Bool
    monotonousIncrasing xs = and [ x' >= x | (x, x') <- zip xs (tail xs) ]

-- Recovered Agent generates no events and stays recovered FOREVER. This means:
--  pre-condition:   in Recovered state and ANY event
--  post-condition:  in Recovered state and 0 scheduled events

-- Susceptible Agent MIGHT become Infected and Recovered
-- TODO:

-- Infected Agent will NEVER become Susceptible and WILL become Recovered
-- TODO: right its not in the control of the infected to become recovered,
-- that is part of the susceptible agent, which makes it difficult to test
-- what can we do?

genRandomEventSIR :: [SIRState]
                  -> Int
                  -> Double
                  -> Double 
                  -> Integer
                  -> Gen [(Time, (Int, Int, Int))]
genRandomEventSIR ss cr inf illDur maxEvents = do
    g <- genStdGen 

    -- ignore initial events
    let (am0, _) = evalRand (initSIR ss cr inf illDur) g
    let ais = Map.keys am0

    evtStream <- genQueueItemStream 0 ais
    let (_amFinal, ds) = evalRand (runReaderT (executeAgents maxEvents evtStream am0 []) ais) g
    return ds
  where
    executeAgents :: RandomGen g
                  => Integer
                  -> [QueueItem SIREvent]
                  -> AgentMap (SIRMonad g) SIREvent SIRState
                  -> [(Time, (Int, Int, Int))]
                  -> ReaderT [AgentId] (Rand g)
                            (SIRAgentMap g, [(Time, (Int, Int, Int))])
    executeAgents 0 _ am acc  = return (am, reverse acc)
    executeAgents _ [] am acc = return (am, reverse acc)
    executeAgents n (evt:es) am acc = do
      retMay <- processEvent am evt 
      case retMay of 
        Nothing -> executeAgents (n-1) es am acc
        -- ignore events produced by agents
        (Just (am', _)) -> do
          let acc' = (eventTime evt, aggregateAgentMap am) : acc
          executeAgents (n-1) es am' acc'
          
--------------------------------------------------------------------------------
-- AGENT API INTERPRETER
--------------------------------------------------------------------------------
-- runAgent :: RandomGen g
--          => AgentTestState g             -- ^ The current testing state
--          -> Command                      -- ^ An instance of an agent API 'call'
--          -> (AgentTestState g, Response) -- ^ Results in a new testing state with some agent output
-- runAgent as (QueueItem _ai (Event e) t) = (as', ao)
--   where
--     g   = rng as 
--     a   = agent as
--     ais = agentIds as

--     aMsf       = unMSF a e
--     aEvtWriter = runReaderT aMsf t
--     aAisReader = runWriterT aEvtWriter
--     aDomWriter = runReaderT aAisReader ais
--     aRand      = runWriterT aDomWriter
--     ((((o, a'), es), _dus), g') = runRand aRand g

--     as' = as { rng = g', agent = a', time = t }
--     ao  = Resp o es

-- mkInitState :: RandomGen g
--             => g
--             -> SIRAgentCont g
--             -> AgentTestState g
-- mkInitState g a = AgentTestState
--   { rng      = g
--   , agent    = a
--   , time     = 0
--   , agentIds = []
--   }

--------------------------------------------------------------------------------
-- UTILS
--------------------------------------------------------------------------------
fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c