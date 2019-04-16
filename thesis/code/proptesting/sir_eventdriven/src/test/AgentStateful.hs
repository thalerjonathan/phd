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
-- a model for the implementation to be tested against.
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

    -- The sequential property checks if the model is consistent with respect
    -- to the semantics. 
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

    -- The transition function explains how actions change the model. Note that 
    -- the transition function is polymorphic in r. The reason for this is that 
    -- we use the transition function both while generating and executing.
    transition :: Model -> AgentCmd -> AgentResp -> Model 
    transition = undefined

    -- The pre-condition of an action specifies in what context the action is
    -- well-defined. The pre-conditions are used while generating programs 
    -- (lists of actions).
    precondition :: Model -> AgentCmd -> Logic
    precondition = undefined

    -- Post-conditions are checked after we executed an action and got access to 
    -- the result.
    postcondition :: Model -> AgentCmd -> AgentResp -> Logic
    postcondition = undefined

    -- To stop the generation of new commands, e.g., when the model has reached
    -- a terminal or error state, let generator return Nothing.
    generator :: Model -> Maybe (Gen AgentCmd)
    generator = undefined

    -- shrinks commands
    shrinker :: Model -> AgentCmd -> [AgentCmd]
    shrinker = undefined

    -- the semantics actually run the command within the underlying semantic
    -- Note that if an example has no meaningful semantics, we merely 
    -- model-check. This is the case for us, so here we can generate expected
    -- responses to given commands. TODO: this is not sufficient! because it
    -- changes when the agent changes
    semantics :: AgentCmd -> IO AgentResp
    semantics = undefined

    -- mocks responses given a model.
    -- Note: in this case we can run the given command using the model because
    -- all data is there to be able to do it.
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