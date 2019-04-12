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

instance Arbitrary SIREvent where
  arbitrary :: Gen SIREvent
  arbitrary = oneof [ return MakeContact 
                    , do
                        s <- elements [Susceptible, Infected, Recovered]
                        (Positive ai) <- arbitrary
                        return $ Contact ai s
                    , return Recover]

-- clear & stack test sir-event:sir-agent-test --test-arguments="--quickcheck-tests=1000"

main :: IO ()
main = do
    let _tastyQCTests = testGroup "Agent Tests" 
                          [ QC.testProperty "Susceptible invariants" prop_susceptible_invariants
                          -- , QC.testProperty "Infected invariants" prop_infected_invariants
                          -- , QC.testProperty "Recovered agent invariants" prop_recovered_invariant

                          -- , QC.testProperty "Recovered agent stays recovered forever" prop_recovered_forever
                          -- , QC.testProperty "Infected agent never susceptible" prop_infected_neversusceptible

                          ]

    let _maxFailPercTests = 
            [ 
              ("Susceptible agent mean contact rate", prop_susceptible_meancontactrate)
            , ("Susceptible agent mean illness duration", prop_susceptible_meanIllnessDuration)
            , ("Susceptible agent mean infectivity", prop_susceptible_meanInfectivity)
            ]

    --_quickCheckFailPercentageTests _maxFailPercTests
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
        agentTestingArgs = stdArgs { maxSuccess = 100    -- number successful tests
                                   , maxFailPercent = 100 -- number of maximum failed tests
                                   --, maxShrinks = 0       
                                   --, replay = Just (mkQCGen 42, 0) -- use to replay reproducible
                                   }
--------------------------------------------------------------------------------
-- INVARIANT PROPERTIES

-- SUSCEPTIBLE INVARIANTS WHEN RECEIVING EVENTS
-- MakeContact: 
--    - replies with random number of Contact Susceptible events with senderId 
--      being the agentid itself and the receiver one from the existing agents
--      and the time being the same time (immediate scheduling)
--    - doesn't schedule any other events than Contact 
--    - output is Susceptible
-- Contact _ Infected:
--    - when getting infected, schedules Recover to itself 
--      into future and output is Infected
--    - when not infected, output is Susceptible
--    - doesn't schedule other events 
-- Recover:
--    - output is Susceptible
--    - doesn't schedule other events
prop_susceptible_invariants :: Gen Property
prop_susceptible_invariants = do
    -- need a random number generator
    g  <- genStdGen
    -- generate non-empty list of agent ids, we have at least one agent
    -- the susceptible agent itself
    ais <- genNonEmptyAgentIds
    -- generate positive time
    (Positive t) <- arbitrary
    -- the susceptible agents id is picked randomly from all empty agent ids
    ai <- elements ais 
    -- generate a random event
    evt <- genEvent ais

    -- create susceptible agent with agen id
    let a = susceptibleAgent ai contactRate infectivity illnessDuration
    -- run agent with given event and configuration
    let (_g', _a', ao, es) = runAgent g a evt t ais

    case evt of
      Recover -> return $ property (null es && ao == Susceptible)

      MakeContact -> do
        let ret = checkMakeContactInvariants ai ais t es
        return $ property (ret && ao == Susceptible)

      Contact _sender s -> 
        case s of
          -- this event will never be generated
          Recovered -> return $ property False 
          -- Susceptible does not reply to this
          Susceptible -> return $ property (null es && ao == Susceptible)
          -- might become infected
          Infected -> if ao /= Infected
                        -- not infected, nothing happens
                        then return $ property (null es && ao == Susceptible) 
                        -- infected, check invariants
                        -- TODO: use coverage
                        else do
                          let ret = checkInfectedInvariants ai t es
                          return $ label "Susceptible became Infected" ret
  where
    checkInfectedInvariants :: AgentId
                            -> Time
                            -> [QueueItem SIREvent]
                            -> Bool
    checkInfectedInvariants sender t 
        [QueueItem receiver (Event Recover) t'] -- expect exactly one Recovery event
      = sender == receiver && t' >= t -- receiver is sender (self) and scheduled into the future
    checkInfectedInvariants _ _ _ = False

    checkMakeContactInvariants :: AgentId
                               -> [AgentId]
                               -> Time
                               -> [QueueItem SIREvent]
                               -> Bool
    checkMakeContactInvariants sender ais t es
        -- make sure there has to be exactly one MakeContact event
        = uncurry (&&) ret
      where
        ret = foldr checkMakeContactInvariantsAux (True, False) es

        checkMakeContactInvariantsAux :: QueueItem SIREvent 
                                      -> (Bool, Bool)
                                      -> (Bool, Bool)
        checkMakeContactInvariantsAux 
            (QueueItem receiver (Event (Contact sender' Susceptible)) t') (b, mkb)
          = (b && sender == sender' && -- the sender in Contact must be the Susceptible agent
                  receiver `elem` ais  -- the receiver of Contact must be in the agent ids
                  && t == t', mkb)     -- the Contact event is scheduled immediately
        checkMakeContactInvariantsAux 
            (QueueItem receiver (Event MakeContact) t') (b, mkb) 
          = (b && receiver == sender &&  -- the receiver of MakeContact is the Susceptible agent itself
                  t' == t + 1.0 &&       -- the MakeContact event is scheduled 1 time-unit into the future
                  not mkb, True)         -- there can only be one MakeContact event
        checkMakeContactInvariantsAux evt (_, mkb) = error ("failure " ++ show evt) (False, mkb)

-- INFECTED INVARIANTS WHEN RECEIVING EVENTS
-- TODO
-- MakeContact: 
--    - doesn't schedule any events 
--    - output Infected
-- Contact _ Susceptible:
--    - schedules Contact Infected event back to the receiver with self agent id
--    - doesn't schedule other events 
--    - output Infected
-- Contact _ _:
--    - doesn't schedule any events 
--    - output Infected
-- Recover:
--    - doesn't schedule any events 
--    - output Recovered
-- prop_infected_invariants :: Gen Property
-- prop_infected_invariants evt  = do
--     g             <- genStdGen
--     (Positive t)  <- arbitrary
--     (Positive ai) <- arbitrary
--     ais <- genAgentIds

--     let a  = infectedAgent ai
--         es = runDefaultTimeAgentEvents ag evt t

--     case evt of 
--       (Contact aid Susceptible) -> do
--         [(QueueItem aid' (Event e) t')] = es
--         return length es == 1 &&                     -- only one event scheduled
--                             aid'      == aid &&                   -- receiver is sender of Contact event
--                             e         == Contact ai Infected &&   -- event is Contact with infected agents id and Infected state
--                             t'        == t                        -- time of scheduling is current time: immediate schedule with 0 delay
--         -- TODO: check agent out
--       (Recover) -> do
--         -- TODO: check agent out
--         null es
--        _ -> null es)
--   where
--     labelContactEvent :: SIREvent -> String
--     labelContactEvent (Contact _ s) = "ContactEvent * " ++ show s
--     labelContactEvent ce            = "ContactEvent * " ++ show ce -- should never happen

-- RECOVERED INVARIANTS WHEN RECEIVING EVENTS
-- MakeContact: 
--    - doesn't schedule any events 
--    - output Recovered
-- Contact _ _:
--    - doesn't schedule other events 
--    - output Recovered
-- Recover:
--    - doesn't schedule any events 
--    - output Recovered
prop_recovered_invariant :: Gen Property
prop_recovered_invariant = do
  g            <- genStdGen
  -- must be non-empty because we have at least one agent in the simulation:
  -- the recovered itself
  ais          <- genNonEmptyAgentIds 
  (Positive t) <- arbitrary
  evt          <- genEvent ais

  let a = recoveredAgent
      (_g', _a', ao, es) = runAgent g a evt t ais

  return $ label (labelSIREvent evt) (null es && ao == Recovered)

--------------------------------------------------------------------------------
-- RANDOM MULTI-STEP
-- TODO: they are not very useful, can be made more practical

-- recovered stays recovered never susceptible or infected
prop_recovered_forever :: Gen Bool
prop_recovered_forever = do
  g   <- genStdGen
  es  <- vector 1000 
  ais <- genAgentIds
  (Positive t) <- arbitrary

  let a   = recoveredAgent
      aos = runAgentEventsOut es g a t ais

  return (all (==Recovered) aos)
  
-- infected never back to susceptible 
prop_infected_neversusceptible :: Gen Bool
prop_infected_neversusceptible = do
  g   <- genStdGen
  es  <- vector 1000
  ais <- genAgentIds
  (Positive ai) <- arbitrary
  (Positive t)  <- arbitrary

  let a   = infectedAgent ai
      aos = runAgentEventsOut es g a t ais

  return (not $ any (==Susceptible) aos)

--------------------------------------------------------------------------------
-- TODO implement multi-event invariant which makes transition from Susceptible,
-- to Infected to Recovered, maybe quickcheck-statemachine can be of help here?

--------------------------------------------------------------------------------
-- PROBABILITIES / DURATIONS PROPERTIES

-- susceptible schedules on average contactrate events
prop_susceptible_meancontactrate :: Gen Property
prop_susceptible_meancontactrate = do
    cs <- map fromIntegral <$> vectorOf 1000 genSusceptibleAgentMakeContact

    let csMean     = mean cs
        confidence = 0.95
        csTTest    = tTestSamples TwoTail (fromIntegral contactRate) (1 - confidence) cs

    return $ label (printf "%.1f" csMean) (fromMaybe True csTTest)
  where
    genSusceptibleAgentMakeContact :: Gen Int
    genSusceptibleAgentMakeContact = do
      g            <- genStdGen
      ais          <- genAgentIds
      (Positive t) <- arbitrary

      let ag  = susceptibleAgent 0 contactRate infectivity illnessDuration
          (_g', _ag', _ao, es) = runAgent g ag MakeContact t ais
          cnt = foldr countContacts 0 es

      return cnt

    countContacts :: QueueItem SIREvent -> Int -> Int 
    countContacts (QueueItem _ (Event (Contact 0 Susceptible)) _) n = n + 1
    countContacts _ n = n

-- infected agent recovering schedules event with average illnessduration
prop_susceptible_meanIllnessDuration :: Gen Property
prop_susceptible_meanIllnessDuration = do
  let repls = 1000
  is <- catMaybes <$> vectorOf repls genSusceptibleAgentInfected

  let isMean     = mean is
      confidence = 0.95
      csTTest    = tTestSamples TwoTail illnessDuration (1 - confidence) is

  return $ label (printf "%.1f" isMean) (fromMaybe True csTTest)

-- susceptible becomes infected on average with infectivity 
prop_susceptible_meanInfectivity :: Gen Property
prop_susceptible_meanInfectivity = do
  let repls = 100
  is <- vectorOf repls genMeanInfectivity

  let isMean     = mean is
      confidence = 0.95
      csTTest    = tTestSamples TwoTail infectivity (1 - confidence) is

  return $ label (printf "%.2f" isMean) (fromMaybe True csTTest)

--------------------------------------------------------------------------------
-- CUSTOM GENERATORS

genNonEmptyAgentIds :: Gen [AgentId]
genNonEmptyAgentIds = listOf1 (do 
  (Positive t) <- arbitrary :: Gen (Positive Int)
  return t)

genAgentIds :: Gen [AgentId]
genAgentIds = map (\(Positive i) -> i) <$> (arbitrary :: Gen [Positive Int])

genEvent :: [AgentId] -> Gen SIREvent
genEvent []  = oneof [ return MakeContact , return Recover]
genEvent ais = oneof [ return MakeContact 
                     , do
                         -- NOTE: Contact is NEVER sent by a Recovered agent
                         s  <- elements [Susceptible, Infected]
                         ai <- elements ais
                         return $ Contact ai s
                     , return Recover]

genStdGen :: Gen StdGen
genStdGen = do
  seed <- choose (minBound, maxBound)
  return $ mkStdGen seed

genMeanInfectivity :: Gen Double
genMeanInfectivity = do
  let repls = 100
  is <- catMaybes <$> vectorOf repls genSusceptibleAgentInfected

  let infectedCount = length is
      infectedRatio = (fromIntegral infectedCount / fromIntegral repls) :: Double

  return infectedRatio

genSusceptibleAgentInfected :: Gen (Maybe Double)
genSusceptibleAgentInfected = do
  g             <- genStdGen
  (Positive t)  <- arbitrary
  ais           <- genAgentIds
  (Positive ai) <- arbitrary

  let a = susceptibleAgent ai contactRate infectivity illnessDuration
      (_g', _ag', ao, es) = runAgent g a (Contact ai Infected) t ais

  case ao of
    Infected -> do
      -- in case it become infected, the agent only schedules a single 
      -- event: recovery, to itself
      let [QueueItem _ (Event Recover) t'] = es
      return $ Just (t' - t)
    _        -> return Nothing

--------------------------------------------------------------------------------
-- LABELING
labelSIREvent :: SIREvent -> String
labelSIREvent (Contact _ s) = "Contact " ++ show s
labelSIREvent evt           = show evt

--------------------------------------------------------------------------------
-- RUNNERS

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