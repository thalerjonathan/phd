{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Writer
import Data.MonadicStreamFunction.InternalCore
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIR.SIR

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

-- clear & stack test sir-event:sir-agent-history-test

main :: IO ()
main = do
  let t = testGroup "Agent History Tests" 
          [ 
            QC.testProperty "Recovered forever" prop_recovered_forever
          ]

  defaultMain t

--------------------------------------------------------------------------------
-- TEST-CASES
--------------------------------------------------------------------------------

-- RANDOM EVENT SAMPLING OF A SYSTEM WITH A RANDOM AGENT POPULATION
-- TODO: after a finite number of steps SIR will reach equilibrium, when there
-- are no more infected agents
-- => but how can we express this in a property?
-- TODO: all S,I,R counts have to respect S+I+R = N - the number of agents
--       stays constant
-- TODO: number of susceptible S can only decrease
-- TODO: number of recovered R can only increase
-- TODO: number of infected I = N - (S + R)
prop_random_sir :: Property
prop_random_sir = do
  let cor = 5
      inf = 0.05
      ild = 15

  mkInitState

runSir :: [(AgentId, SIRAgentCont g)]
       -> Time
       -> Gen ([(AgentId, SIRAgentCont g)], Time)
runSir as t = do
  let (ais, acs) = unzip as

  evt <- genEvent ais

  a  

genEvent :: [AgentId] -> Gen SIREvent
genEvent = genEventFreq 1 1 1 (1,1,1)

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

-- Recovered Agent generates no events and stays recovered FOREVER. This means:
--  pre-condition:   in Recovered state and ANY event
--  post-condition:  in Recovered state and 0 scheduled events

prop_recovered_forever :: Property
prop_recovered_forever = property $ do
    let cor = 5
        inf = 0.05
        ild = 15

    ais <- genNonEmptyAgentIds
    ai  <- elements ais 
    g   <- genStdGen

    let a = sirAgent cor inf ild Susceptible ai

    let asEvtWriter    = runReaderT a 0
        asAsIdsReader  = runWriterT asEvtWriter
        asDomWriter    = runReaderT asAsIdsReader ais
        aRand          = runWriterT asDomWriter
        (((_a', _es), _), _g') = runRand aRand g

    return True

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
runAgent :: RandomGen g
         => AgentTestState g             -- ^ The current testing state
         -> Command                      -- ^ An instance of an agent API 'call'
         -> (AgentTestState g, Response) -- ^ Results in a new testing state with some agent output
runAgent as (QueueItem _ai (Event e) t) = (as', ao)
  where
    g   = rng as 
    a   = agent as
    ais = agentIds as

    aMsf       = unMSF a e
    aEvtWriter = runReaderT aMsf t
    aAisReader = runWriterT aEvtWriter
    aDomWriter = runReaderT aAisReader ais
    aRand      = runWriterT aDomWriter
    ((((o, a'), es), _dus), g') = runRand aRand g

    as' = as { rng = g', agent = a', time = t }
    ao  = Resp o es

mkInitState :: RandomGen g
            => g
            -> SIRAgentCont g
            -> AgentTestState g
mkInitState g a = AgentTestState
  { rng      = g
  , agent    = a
  , time     = 0
  , agentIds = []
  }