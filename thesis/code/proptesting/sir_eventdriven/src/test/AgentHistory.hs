{-# LANGUAGE InstanceSigs #-}
module Main where

import Text.Printf

import Control.Monad.Random
import Control.Monad.Reader
-- import Control.Monad.Writer
-- import Data.MonadicStreamFunction.InternalCore
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Data.IntMap.Strict as Map 

import SIR.SIR

--import Debug.Trace

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

-- clear & stack test sir-event:sir-agent-history-test --test-arguments="--quickcheck-replay=557780 --quickcheck-verbose"

main :: IO ()
main = do
  let t = testGroup "Agent History Tests" 
          [ 
          --  QC.testProperty "SIR simulation invariants" prop_sir_simulation_invariants
          --, QC.testProperty "SIR random event sampling invariants" prop_sir_random_invariants
           QC.testProperty "SIR random sampling equilibrium" prop_sir_random_equilibrium
          ]

  defaultMain t

--------------------------------------------------------------------------------
-- TEST-CASES
--------------------------------------------------------------------------------
prop_sir_simulation_invariants :: Property
prop_sir_simulation_invariants = property $ do
  let cor = 5
      inf = 0.05
      ild = 15

  -- random agents
  ss <- listOf genSIRState
  -- total agent count
  let n = length ss

  -- don't restrict
  ret <- genSimulationSIR ss cor inf ild (-1) (1/0)
  
  -- after a finite number of steps SIR will reach equilibrium, when there
  -- are no more infected agents
  let retFin = takeWhile ((>0).snd3.snd) ret

  return (sirInvariants n retFin)

prop_sir_random_equilibrium :: Property
prop_sir_random_equilibrium = property $ do
  let cor = 5     -- beta, contact rate
      inf = 0.05  -- gamma, infectivitry
      ild = 15    -- delta, illness duration

  -- generate non-empty random population
  ss <- listOf1 genSIRState
  -- number of random events to generate
  let eventCount = 100000
  -- run simulation with random population and random events
  ret <- genRandomEventSIR ss cor inf ild eventCount
  
  -- 
  let _i0 = length (filter (==Infected) ss)
  let equilibrium = any ((>0).snd3.snd) ret

  return $ cover 10 (_i0 > 0 && equilibrium) "Reached equilibrium" True 

prop_sir_random_invariants :: Property
prop_sir_random_invariants = property $ do
  let cor = 5     -- beta, contact rate
      inf = 0.05  -- gamma, infectivitry
      ild = 15    -- delta, illness duration

  -- generate non-empty random population
  ss <- listOf1 genSIRState
  -- total agent count
  let n = length ss
  -- number of random events to generate
  let eventCount = 500000
  -- run simulation with random population and random events
  ret <- genRandomEventSIR ss cor inf ild eventCount
  
  return (sirInvariants n ret)

sirInvariants :: Int -> [(Time, (Int, Int, Int))] -> Bool
sirInvariants n aos = susInc && infConst && recDec && aci && timeInc
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
    monotonousDecreasing xs = and [x' <= x | (x,x') <- zip xs (tail xs)]

    monotonousIncrasing :: (Ord a, Num a) => [a] -> Bool
    monotonousIncrasing xs = and [x' >= x | (x,x') <- zip xs (tail xs)]

labelPopulation :: [SIRState] -> String
labelPopulation as = ss ++ ", " ++ is ++ ", " ++ rs
  where
    s = fromIntegral $ length $ filter (==Susceptible) as
    i = fromIntegral $ length $ filter (==Infected) as
    r = fromIntegral $ length $ filter (==Recovered) as
    n = fromIntegral $ length as

    ss = printf "%.2f" ((s / n) :: Double)
    is = printf "%.2f" (i / n)
    rs = printf "%.2f" (r / n)

-- Recovered Agent generates no events and stays recovered FOREVER. This means:
--  pre-condition:   in Recovered state and ANY event
--  post-condition:  in Recovered state and 0 scheduled events

-- Susceptible Agent MIGHT become Infected and Recovered
-- TODO:

-- Infected Agent will NEVER become Susceptible and WILL become Recovered
-- TODO: right its not in the control of the infected to become recovered,
-- that is part of the susceptible agent, which makes it difficult to test
-- what can we do?

--------------------------------------------------------------------------------
-- CUSTOM GENERATORS
--------------------------------------------------------------------------------
genNonEmptyAgentIds :: Gen [AgentId]
genNonEmptyAgentIds = listOf1 (do 
  (Positive t) <- arbitrary :: Gen (Positive Int)
  return t)

genAgentIds :: Gen [AgentId]
genAgentIds = map (\(Positive i) -> i) <$> (arbitrary :: Gen [Positive Int])

genEventFreq :: Int
             -> Int
             -> Int
             -> (Int, Int, Int)
             -> [AgentId]
             -> Gen SIREvent
genEventFreq mcf _ rcf _ []  
  = frequency [ (mcf, return MakeContact), (rcf, return Recover)]
genEventFreq mcf cof rcf (s,i,r) ais
  = frequency [ (mcf, return MakeContact)
              , (cof, do
                  ss <- frequency [ (s, return Susceptible)
                                  , (i, return Infected)
                                  , (r, return Recovered)]
                  ai <- elements ais
                  return $ Contact ai ss)
              , (rcf, return Recover)]

genEvent :: [AgentId] -> Gen SIREvent
genEvent = genEventFreq 1 1 1 (1,1,1)

genStdGen :: Gen StdGen
genStdGen = do
  seed <- choose (minBound, maxBound)
  return $ mkStdGen seed

genSimulationSIR :: [SIRState]
                 -> Int
                 -> Double
                 -> Double 
                 -> Integer
                 -> Double
                 -> Gen [(Time, (Int, Int, Int))]
genSimulationSIR ss cr inf illDur maxEvents maxTime = do
  g <- genStdGen 
  return $ fst $ runSIR ss cr inf illDur maxEvents maxTime g

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

eventTime :: QueueItem e -> Time
eventTime (QueueItem _ _ et) = et

genSIRState :: Gen SIRState
genSIRState = elements [Susceptible, Infected, Recovered]

genQueueItemStream :: Double 
                   -> [AgentId]
                   -> Gen [QueueItem SIREvent]
genQueueItemStream t ais = do
  evt  <- genQueueItem t ais
  evts <- genQueueItemStream (eventTime evt) ais
  return (evt : evts)

genQueueItem :: Double 
             -> [AgentId]
             -> Gen (QueueItem SIREvent)
genQueueItem t ais = do
  (Positive dt) <- arbitrary
  e <- genEvent ais
  receiver <- elements ais

  let evtTime = t + dt

  return $ QueueItem receiver (Event e) evtTime
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
