{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad.Random
import Control.Monad.State.Strict

import Data.MonadicStreamFunction
import Data.IntMap.Strict as Map 
import Data.PQueue.Min as PQ
import Test.QuickCheck

import SIR.SIR

contactRate :: Int
contactRate = 5

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

newtype AllSIREvent = AllSIREvent SIREvent deriving Show
newtype NoRecoverSIREvent = NoRecoverSIREvent SIREvent deriving Show

-- instance Arbitrary SIRState where
--   arbitrary :: Gen SIRState
--   arbitrary = elements [Susceptible, Infected, Recovered]

instance Arbitrary AllSIREvent where
  arbitrary :: Gen AllSIREvent
  arbitrary = do
    s <- elements [Susceptible, Infected, Recovered] 
    e <- elements [MakeContact, Contact 0 s, Recover]
    return $ AllSIREvent e

-- clear & stack test sir-event:sir-agent-test

main :: IO ()
main = do
  let tests = [ ("Recovered agent stays recovered forever", prop_recovered_forever)
              , ("Recovered agent generates no events", prop_recovered_no_events)
              , ("Susceptible agent never recovered", prop_susceptible_noreceovered)
              ]

  putStrLn ""
  putStrLn "--------------------------------------------------------------------------------"
  putStrLn "Running Agent Tests..."

  mapM_ (\(tName, t) -> do
    putStrLn ""
    putStrLn $ "Testing " ++ tName ++ " ..."
    quickCheckWith agentTestingArgs t) tests

  putStrLn ""
  putStrLn "Running Agent Tests finished."
  putStrLn "--------------------------------------------------------------------------------"

agentTestingArgs :: Args
agentTestingArgs = stdArgs { maxSuccess = 1000      -- number successful tests
                           --, maxFailPercent = 100 -- number of maximum failed tests
                           --, maxShrinks = 0       
                            --, replay = Just (mkQCGen 42, 0) -- use to replay reproducible
                           }

--------------------------------------------------------------------------------
-- INVARIANT PROPERTIES

-- recovered stays recovered never susceptible or infected
prop_recovered_forever :: AllSIREvent -> Property
prop_recovered_forever ae@(AllSIREvent evt) 
    = label (show ae) (ao == Recovered)
  where
    ag = recoveredAgent
    ao = runDefaultAgent ag evt

-- recovered schedules no events
prop_recovered_no_events :: AllSIREvent -> Property
prop_recovered_no_events ae@(AllSIREvent evt) 
    = label (show ae) (PQ.null aq)
  where
    ag = recoveredAgent
    as = runDefaultAgentState ag evt
    aq = absEvtQueue as

-- susceptible can not become recovered (has to become infected first) 
prop_susceptible_noreceovered :: AllSIREvent -> Property
prop_susceptible_noreceovered ae@(AllSIREvent evt) 
    = label (show ae) (ao /= Recovered)
  where
    -- TODO: generate random population as well!
    ag = susceptibleAgent 0 contactRate infectivity illnessDuration
    ao = runDefaultAgent ag evt

--  infected never back to susceptible 
prop_infected_neversusceptible :: Bool
prop_infected_neversusceptible = undefined

-- an infected replies to each contact
prop_infected_contact_reply :: Bool
prop_infected_contact_reply = undefined

--------------------------------------------------------------------------------
-- PROBABILITIES / DURATIONS PROPERTIES

-- infected agent recovering schedules event with average illnessduration
-- susceptible schedules on average contactrate events

--------------------------------------------------------------------------------
-- UTILS
runAgent :: RandomGen g
         => g
         -> SIRABSState g
         -> SIRAgentCont g
         -> SIREvent
         -> (g, SIRABSState g, SIRAgentCont g, SIRState)
runAgent g as a e = (g', abs', a', ao)
  where
    amsfState = unMSF a e
    amsfRand  = runStateT amsfState as
    (((ao, a'), abs'), g') = runRand amsfRand g

runAgentOut :: RandomGen g
            => g
            -> SIRABSState g
            -> SIRAgentCont g
            -> SIREvent
            -> SIRState
runAgentOut g as a e = ao
  where
    (_, _, _, ao) = runAgent g as a e

runDefaultAgent :: SIRAgentCont StdGen -> SIREvent -> SIRState
runDefaultAgent = runAgentOut g as
  where
    g  = mkStdGen 0
    as = mkAbsSirStateDefault 

runDefaultAgentState :: SIRAgentCont StdGen -> SIREvent -> SIRABSState StdGen
runDefaultAgentState a e = as'
  where
    g  = mkStdGen 0
    as = mkAbsSirStateDefault 
    (_, as', _, _) = runAgent g as a e
  
mkAbsSirStateDefault :: SIRABSState g
mkAbsSirStateDefault = ABSState {
    absEvtQueue    = PQ.empty
  , absTime        = 0
  , absAgents      = Map.empty
  , absAgentIds    = []
  , absEvtCount    = 0
  , absDomainState = (0,0,0)
  }