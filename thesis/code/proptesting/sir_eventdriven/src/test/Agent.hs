{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Maybe
import Text.Printf

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Writer
import Data.MonadicStreamFunction
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIR.SIR
import StatsUtils

contactRate :: Int
contactRate = 5

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

newtype ContactEvent = ContactEvent SIREvent deriving Show

instance Arbitrary SIREvent where
  arbitrary :: Gen SIREvent
  arbitrary = oneof [ return MakeContact 
                    , do 
                        s <- elements [Susceptible, Infected, Recovered] 
                        return $ Contact 0 s
                    , return Recover]

instance Arbitrary ContactEvent where
  arbitrary :: Gen ContactEvent
  arbitrary = do
    s   <- elements [Susceptible, Infected, Recovered] 
    aid <- choose (0, 1000)
    let e = Contact aid s 
    return $ ContactEvent e

-- clear & stack test sir-event:sir-agent-test --test-arguments="--quickcheck-tests=1000"

main :: IO ()
main = do
    let _tastyQCTests = testGroup "Agent Tests" 
                          [ 
                            QC.testProperty "Recovered agent stays recovered forever" prop_recovered_forever
                          , QC.testProperty "Recovered agent generates no events" prop_recovered_no_events
                          , QC.testProperty "Infected agent never susceptible" prop_infected_neversusceptible
                          , QC.testProperty "Infected replies to contact" prop_infected_contact_reply
                          ]

    let _maxFailPercTests = 
            [ ("Susceptible agent mean contact rate", prop_susceptible_meancontactrate)
            , ("Susceptible agent mean illness duration", prop_susceptible_meanIllnessDuration)
            , ("Susceptible agent mean infectivity", prop_susceptible_meanInfectivity)
            ]

   -- _quickCheckFailPercentageTests _maxFailPercTests
    defaultMain _tastyQCTests
    
  where
    _quickCheckFailPercentageTests :: [(String, Gen Property)] -> IO ()
    _quickCheckFailPercentageTests tests = do
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
      where
        agentTestingArgs :: Args
        agentTestingArgs = stdArgs { maxSuccess = 1000    -- number successful tests
                                   , maxFailPercent = 100 -- number of maximum failed tests
                                   --, maxShrinks = 0       
                                   --, replay = Just (mkQCGen 42, 0) -- use to replay reproducible
                                   }
--------------------------------------------------------------------------------
-- INVARIANT PROPERTIES

-- recovered schedules no events
prop_recovered_no_events :: SIREvent -> Property
prop_recovered_no_events evt
    = label (show evt) (null es)
  where
    ag = recoveredAgent
    es = runDefaultAgentEvents ag evt

-- an infected replies to Susceptible contacts only
prop_infected_contact_reply :: ContactEvent -> Positive Double -> Property
prop_infected_contact_reply (ContactEvent evt) (Positive t)
    = label (labelContactEvent evt) (case evt of 
                          -- Susceptible contact: reply
                          (Contact aid Susceptible) -> 
                            length es == 1 &&                     -- only one event scheduled
                            aid'      == aid &&                   -- receiver is sender of Contact event
                            e         == Contact ai Infected &&   -- event is Contact with infected agents id and Infected state
                            t'        == t                        -- time of scheduling is current time: immediate schedule with 0 delay
                          -- no reply to other Contacts
                          _ -> null es)
  where
    -- TODO: do we need a proper RNG here? 
    ai = 42
    ag = infectedAgent ai
    es = runDefaultTimeAgentEvents ag evt t
    (QueueItem aid' (Event e) t' :_) = es

    labelContactEvent :: SIREvent -> String
    labelContactEvent (Contact _ s) = "ContactEvent * " ++ show s
    labelContactEvent ce            = "ContactEvent * " ++ show ce -- should never happen

--------------------------------------------------------------------------------
-- PROBABILITIES / DURATIONS PROPERTIES

-- susceptible schedules on average contactrate events
-- TODO: can we randomise it further?
prop_susceptible_meancontactrate :: Gen Property
prop_susceptible_meancontactrate = do
    cs <- map fromIntegral <$> vectorOf 1000 susceptibleAgentMakeContactGen

    let csMean     = mean cs
        confidence = 0.95
        csTTest    = tTestSamples TwoTail (fromIntegral contactRate) (1 - confidence) cs

    return $ label (printf "%.1f" csMean) (fromMaybe True csTTest)
  where
    susceptibleAgentMakeContactGen :: Gen Int
    susceptibleAgentMakeContactGen = do
      g <- stdGenGen

      let ag  = susceptibleAgent 0 contactRate infectivity illnessDuration
          (_g', _ag', _ao, es)  = runAgent g ag MakeContact 0 []
          cnt = foldr countContacts 0 es

      return cnt

    countContacts :: QueueItem SIREvent -> Int -> Int 
    countContacts (QueueItem _ (Event (Contact 0 Susceptible)) _) n = n + 1
    countContacts _ n = n

-- infected agent recovering schedules event with average illsnessduration
prop_susceptible_meanIllnessDuration :: Gen Property
prop_susceptible_meanIllnessDuration = do
  let repls = 1000
  is <- catMaybes <$> vectorOf repls susceptibleAgentInfectedGen

  let isMean     = mean is
      confidence = 0.95
      csTTest    = tTestSamples TwoTail illnessDuration (1 - confidence) is

  return $ label (printf "%.1f" isMean) (fromMaybe True csTTest)

-- susceptible becomes infected on average with infectivity 
prop_susceptible_meanInfectivity :: Gen Property
prop_susceptible_meanInfectivity = do
  let repls = 100
  is <- vectorOf repls meanInfectivityGen

  let isMean     = mean is
      confidence = 0.95
      csTTest    = tTestSamples TwoTail infectivity (1 - confidence) is

  return $ label (printf "%.2f" isMean) (fromMaybe True csTTest)

--------------------------------------------------------------------------------
-- MULTI-EVENT BEHAVIOUR TODO

-- recovered stays recovered never susceptible or infected
prop_recovered_forever :: Gen Bool
prop_recovered_forever = do
  g  <- stdGenGen
  es <- vector 1000 

  let a   = recoveredAgent
      aos = runAgentEventsOut es g a 0 []

  return (all (==Recovered) aos)
  
-- infected never back to susceptible 
prop_infected_neversusceptible :: Gen Property
prop_infected_neversusceptible = do
  g  <- stdGenGen
  es <- vector 1

  let a   = infectedAgent 0
      aos = runAgentEventsOut es g a 0 []

  -- TODO: require that some infected becomes recovered 
  -- TODO split this up!
  return $ cover 10 (any (==Recovered) aos) "Infected recovered" (not $ any (==Susceptible) aos)

-- susceptible can not become recovered within 1 event (has to become infected first) 

--------------------------------------------------------------------------------
-- CUSTOM GENERATORS
stdGenGen :: Gen StdGen
stdGenGen = do
  seed <- choose (minBound, maxBound)
  return $ mkStdGen seed

meanInfectivityGen :: Gen Double
meanInfectivityGen = do
  let repls = 100
  is <- catMaybes <$> vectorOf repls susceptibleAgentInfectedGen

  let infectedCount = length is
      infectedRatio = (fromIntegral infectedCount / fromIntegral repls) :: Double

  return infectedRatio

susceptibleAgentInfectedGen :: Gen (Maybe Double)
susceptibleAgentInfectedGen = do
  g <- stdGenGen

  let ag  = susceptibleAgent 0 contactRate infectivity illnessDuration
      (_g', _ag', ao, es)  = runAgent g ag (Contact 42 Infected) 0 []

  case ao of
    Infected -> do
      -- in case it become infected, the agent only schedules a single 
      -- event: recovery
      let [QueueItem _ (Event Recover) t] = es
      return $ Just t
    _        -> return Nothing



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

runAgentEventsOut :: RandomGen g
                  => [SIREvent]
                  -> g
                  -> SIRAgentCont g
                  -> Time
                  -> [AgentId]
                  -> [SIRState]
runAgentEventsOut [] _ _ _ _ = [] 
runAgentEventsOut (e:es) g a t ais = ao : runAgentEventsOut es g' a' t ais
  where
    (g', a', ao, _es) = runAgent g a e 0 ais

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

runDefaultTimeAgentEvents :: SIRAgentCont StdGen 
                          -> SIREvent 
                          -> Time
                          -> [QueueItem SIREvent]
runDefaultTimeAgentEvents a e t = es
  where
    g = mkStdGen 0
    (_, _, _, es) = runAgent g a e t []
    
runDefaultAgentEvents :: SIRAgentCont StdGen -> SIREvent -> [QueueItem SIREvent]
runDefaultAgentEvents a e = es
  where
    g = mkStdGen 0
    (_, _, _, es) = runAgent g a e 0 []



-- main :: IO ()
-- main = do

