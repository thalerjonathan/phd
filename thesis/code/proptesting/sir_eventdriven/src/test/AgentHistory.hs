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

import Debug.Trace

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

-- clear & stack test sir-event:sir-agent-history-test --test-arguments="--quickcheck-replay=557780"

main :: IO ()
main = do
  let t = testGroup "Agent History Tests" 
          [ 
            QC.testProperty "SIR random sampling invariants" prop_sir_random_invariants
          -- QC.testProperty "SIR simulation invariants" prop_sir_simulation_invariants
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

  ss <- listOf genSIRState
  g  <- genStdGen

  -- total agent count
  let n = length ss
  
  -- run simulation with no restrictions on events AND time!
  let ret = trace ("test-case with: " ++ show ss) map snd $ fst $ runSIR ss cor inf ild (-1) (1/0) g
  
  -- after a finite number of steps SIR will reach equilibrium, when there
  -- are no more infected agents
  -- TODO
  let retFin = ret --takeWhile ((>0).snd3) ret

  -- number of agents stays constant in each step
  let aci = all (agentCountInvariant n) retFin
  -- number of susceptible can only decrease
  let susInc = monotonousDecreasing (fst3 $ unzip3 retFin)
  -- number of infected i = N - (S + R)
  let infConst = all (infectedInvariant n) retFin
  -- number of recovered R can only increase
  let recDec = monotonousIncrasing (trd3 $ unzip3 retFin)

  let prop = susInc && infConst && recDec && aci
  
  return $ label (show (length ss)) prop

prop_sir_random_invariants :: Property
prop_sir_random_invariants = property $ do
  let cor = 5
      inf = 0.05
      ild = 15

  ss <- listOf1 genSIRState
  
  -- total agent count
  let n = length ss
  
  let maxEvents = 100000
  ret <- trace (show ss) map snd <$> genRandomEventSIR ss cor inf ild maxEvents
  
  -- after a finite number of steps SIR will reach equilibrium, when there
  -- are no more infected agents
  -- TODO
  let retFin = ret --takeWhile ((>0).snd3) ret
  
  -- number of agents stays constant in each step
  let aci = all (agentCountInvariant n) retFin
  -- number of susceptible can only decrease
  let susInc = monotonousDecreasing (fst3 $ unzip3 retFin)
  -- number of infected i = N - (S + R)
  let infConst = all (infectedInvariant n) retFin
  -- number of recovered R can only increase
  let recDec = monotonousIncrasing (trd3 $ unzip3 retFin)

  let prop = susInc && infConst && recDec && aci
  
  return $ label (show (length ss)) prop

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

agentCountInvariant :: Int -> (Int, Int, Int) -> Bool
agentCountInvariant n (s,i,r) = s+i+r == n

infectedInvariant :: Int -> (Int, Int, Int) -> Bool
infectedInvariant n (s,i,r) = i == n - (s + r)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

monotonousDecreasing :: [Int] -> Bool
monotonousDecreasing xs = and [x' <= x | (x,x') <- zip xs (tail xs)]

monotonousIncrasing :: [Int] -> Bool
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