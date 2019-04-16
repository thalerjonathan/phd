{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Writer
import Data.MonadicStreamFunction.InternalCore
import Test.QuickCheck
import Test.StateMachine
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
type AgentCmd = QueueItem SIREvent
-- the output of an agent is its current SIRState and the events it has scheduled
data AgentResp = AgentResp SIRState [QueueItem SIREvent]
-- the model
type Model = AgentTestState StdGen

-- clear & stack test sir-event:sir-agent-state-test

main :: IO ()
main = do
  let t = testGroup "Agent Stateful Tests" 
          [ 
            QC.testProperty "Recovered forever" prop_recovered_forever
          ]

  defaultMain t

--------------------------------------------------------------------------------
-- TEST-CASES
--------------------------------------------------------------------------------

-- Recovered Agent generates no events and stays recovered FOREVER. This means:
--  pre-condition:   in Recovered state and ANY event
--  post-condition:  in Recovered state and 0 scheduled events



prop_recovered_forever :: Property
prop_recovered_forever = do
    g <- genStdGen

    let a  = recoveredAgent
        sm = createSM g a

    forAllCommands sm Nothing (\cmds -> monadicIO $ do
      (hist, _model, res) <- runCommands sm' cmds
      prettyCommands sm' hist (checkCommandNames cmds (res === Ok)))

  where
    createSM :: RandomGen g
            => g
            -> SIRAgentCont g
            -> StateMachine Model AgentCmd IO AgentResp
    createSM g a = 
      StateMachine 
        (mkInitState g a) 
        transition 
        precondition 
        postcondition
        Nothing        -- Invariant
        generator
        Nothing        -- Distribution
        shrinker
        semantics
        mock

    transition ::  Model -> AgentCmd -> AgentResp -> Model 
    transition = undefined

    precondition :: Model -> AgentCmd -> Logic
    precondition = undefined

    postcondition :: Model -> AgentCmd -> AgentResp -> Logic
    postcondition = undefined

    generator :: Model -> Maybe (Gen AgentCmd)
    generator = undefined

    shrinker :: Model -> AgentCmd -> [AgentCmd]
    shrinker = undefined

    semantics :: AgentCmd -> IO AgentResp
    semantics = undefined

    mock :: Model -> AgentCmd -> GenSym AgentResp
    mock = undefined

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
         => AgentTestState g         -- ^ The current testing state
         -> AgentCmd                 -- ^ An instance of an agent API 'call'
         -> (AgentTestState g, AgentResp) -- ^ Results in a new testing state with some agent output
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
    ao  = AgentResp o es

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