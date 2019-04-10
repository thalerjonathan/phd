{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Writer
import Data.MonadicStreamFunction
--import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIR.SIR

contactRate :: Int
contactRate = 5

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

newtype AllSIREvent = AllSIREvent SIREvent deriving Show
newtype ContactEvent = ContactEvent SIREvent deriving Show
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

instance Arbitrary ContactEvent where
  arbitrary :: Gen ContactEvent
  arbitrary = do
    s   <- elements [Susceptible, Infected, Recovered] 
    aid <- choose (0, 1000)
    let e = Contact aid s 
    return $ ContactEvent e

-- clear & stack test sir-event:sir-agent-test

-- main :: IO ()
-- main = do
--   let tests = [ --("Recovered agent stays recovered forever", prop_recovered_forever)
--               --, ("Recovered agent generates no events", prop_recovered_no_events)
--               --, ("Susceptible agent never recovered", prop_susceptible_noreceovered)
--               --, ("Infected agent never susceptible", prop_infected_neversusceptible)
--                ("Infected replies to contact", prop_infected_contact_reply)
--               ]

--   putStrLn ""
--   putStrLn "--------------------------------------------------------------------------------"
--   putStrLn "Running Agent Tests..."

--   mapM_ (\(tName, t) -> do
--     putStrLn ""
--     putStrLn $ "Testing " ++ tName ++ " ..."
--     quickCheckWith agentTestingArgs t) tests

--   putStrLn ""
--   putStrLn "Running Agent Tests finished."
--   putStrLn "--------------------------------------------------------------------------------"

-- agentTestingArgs :: Args
-- agentTestingArgs = stdArgs { maxSuccess = 1000      -- number successful tests
--                            --, maxFailPercent = 100 -- number of maximum failed tests
--                            --, maxShrinks = 0       
--                             --, replay = Just (mkQCGen 42, 0) -- use to replay reproducible
--                            }

main :: IO ()
main = defaultMain agentTests

agentTests :: TestTree 
agentTests = testGroup "Agent Tests"
              [ QC.testProperty "Recovered agent stays recovered forever" prop_recovered_forever
              , QC.testProperty "Recovered agent generates no events" prop_recovered_no_events
              , QC.testProperty "Susceptible agent never recovered" prop_susceptible_noreceovered
              , QC.testProperty "Infected agent never susceptible" prop_infected_neversusceptible
              , QC.testProperty "Infected replies to contact" prop_infected_contact_reply
              ]
--------------------------------------------------------------------------------
-- INVARIANT PROPERTIES

-- recovered stays recovered never susceptible or infected
prop_recovered_forever :: AllSIREvent -> Property
prop_recovered_forever ae@(AllSIREvent evt) 
    = label (show ae) (ao == Recovered)
  where
    ag = recoveredAgent
    ao = runDefaultAgentOut ag evt

-- recovered schedules no events
prop_recovered_no_events :: AllSIREvent -> Property
prop_recovered_no_events ae@(AllSIREvent evt) 
    = label (show ae) (null es)
  where
    ag = recoveredAgent
    es = runDefaultAgentEvents ag evt

-- susceptible can not become recovered (has to become infected first) 
prop_susceptible_noreceovered :: AllSIREvent -> Property
prop_susceptible_noreceovered ae@(AllSIREvent evt) 
    = label (show ae) (ao /= Recovered)
  where
    -- TODO: generate random population as well!
    -- TODO: why is it working with empty list of agents??
    ag = susceptibleAgent 0 contactRate infectivity illnessDuration
    ao = runDefaultAgentOut ag evt

--  infected never back to susceptible 
prop_infected_neversusceptible :: AllSIREvent -> Property
prop_infected_neversusceptible ae@(AllSIREvent evt) 
    = label (show ae) (ao /= Susceptible)
  where
    ag = infectedAgent 0
    ao = runDefaultAgentOut ag evt

-- an infected replies to Susceptible contacts only
prop_infected_contact_reply :: ContactEvent -> Property
prop_infected_contact_reply (ContactEvent evt) 
    = label (labelContactEvent evt) (case evt of 
                          (Contact aid Susceptible) -> 
                            length es == 1 &&
                            aid' == aid && 
                            e    == Contact ai Infected &&
                            t    == 0 -- TODO: randomise time
                          _ -> True)
  where
    labelContactEvent (Contact _ s) = "ContactEvent * " ++ show s
    labelContactEvent ce            = "ContactEvent * " ++ show ce -- should never happen

    ai = 42
    ag = infectedAgent ai
    es = runDefaultAgentEvents ag evt

    (QueueItem aid' (Event e) t :_) = es

--------------------------------------------------------------------------------
-- PROBABILITIES / DURATIONS PROPERTIES

-- infected agent recovering schedules event with average illnessduration
-- susceptible schedules on average contactrate events

--------------------------------------------------------------------------------
-- UTILS
runAgent :: RandomGen g
         => g
         -> SIRAgentCont g
         -> SIREvent
         -> Time
         -> [AgentId]
         -> (g, SIRAgentCont g, SIRState, [QueueItem SIREvent])
runAgent g a e t ais  = (g', a', ao, es)
  where
    aMsf       = unMSF a e
    aEvtWriter = runReaderT aMsf t
    aAisReader = runWriterT aEvtWriter
    aDomWriter = runReaderT aAisReader ais
    aRand      = runWriterT aDomWriter

    ((((ao, a'), es), _dus), g') = runRand aRand g

runAgentOut :: RandomGen g
            => g
            -> SIRAgentCont g
            -> SIREvent
            -> Time
            -> [AgentId]
            -> SIRState
runAgentOut g a e t ais = ao
  where
    (_, _, ao, _) = runAgent g a e t ais

runDefaultAgentOut :: SIRAgentCont StdGen -> SIREvent -> SIRState
runDefaultAgentOut a e = runAgentOut g a e 0 []
  where
    g = mkStdGen 0

runDefaultAgentEvents :: SIRAgentCont StdGen -> SIREvent -> [QueueItem SIREvent]
runDefaultAgentEvents a e = es
  where
    g = mkStdGen 0
    (_, _, _, es) = runAgent g a e 0 []