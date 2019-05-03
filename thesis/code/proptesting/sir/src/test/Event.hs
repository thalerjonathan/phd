{-# LANGUAGE InstanceSigs #-}
module Main where

-- import Text.Printf

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIR.Model
import SIR.Event
import Utils.GenEventSIR
import Utils.GenSIR

-- --quickcheck-replay=557780
-- --quickcheck-tests=1000
-- --quickcheck-verbose
-- --test-arguments=""
-- clear & stack test sir:sir-event-tests --test-arguments="--quickcheck-tests=1000"

main :: IO ()
main = do
  let t = testGroup "SIR Agent Specifications Tests" 
          [ 
            QC.testProperty "Susceptible invariants" prop_susceptible_invariants
          , QC.testProperty "Susceptible probabilities" prop_susceptible_proabilities
          , QC.testProperty "Infected invariants" prop_infected_invariants
          , QC.testProperty "Recovered agent invariants" prop_recovered_invariant
          ]

  defaultMain t
    
--------------------------------------------------------------------------------
-- INVARIANT PROPERTIES

-- NOTE: this specification implicitly covers that a susceptible agent never 
-- goes to recovered with 1 event - it either stays Susceptible or becomes
-- infected. This becomes clear from observing the output and making sure
-- that we have covered all cases
prop_susceptible_invariants :: Positive Int
                            -> Probability
                            -> Positive Double
                            -> Positive Double
                            -> NonEmptyList AgentId
                            -> Gen Property
prop_susceptible_invariants 
    (Positive cor) (P inf) (Positive ild) (Positive t) (NonEmpty ais) = do
    -- generate random event, needs the population agent ids
    evt <- genEvent ais
    -- run susceptible random agent with given parameters
    (ai, ao, es) <- genRunSusceptibleAgent cor inf ild t ais evt
    -- check properties
    return $ property $ susceptibleProps evt ao es ai
  where
    susceptibleProps :: SIREvent
                     -> SIRState
                     -> [QueueItem SIREvent]
                     -> AgentId
                     -> Bool
    -- received Recover => stay Susceptible, no event scheduled
    susceptibleProps Recover Susceptible es _ = null es
    -- received MakeContact => stay Susceptible, check events
    susceptibleProps MakeContact Susceptible es ai
      = checkMakeContactInvariants ai es cor 
    -- received Contact _ Recovered => stay Susceptible, no event scheduled
    susceptibleProps (Contact _ Recovered) Susceptible es _ = null es
    -- received Contact _ Susceptible => stay Susceptible, no event scheduled
    susceptibleProps (Contact _ Susceptible) Susceptible es _  = null es
    -- received Contact _ Infected, didn't get Infected, no event scheduled
    susceptibleProps (Contact _ Infected) Susceptible es _ = null es
    -- received Contact _ Infected AND got infected, check events
    susceptibleProps (Contact _ Infected) Infected es ai
      = checkInfectedInvariants ai es
    -- all other cases are invalid and result in a failed test-case
    susceptibleProps _ _ _ _ = False

    checkInfectedInvariants :: AgentId
                            -> [QueueItem SIREvent]
                            -> Bool
    checkInfectedInvariants sender 
        [QueueItem receiver (Event Recover) t'] -- expect exactly one Recovery event
      = sender == receiver && t' >= t -- receiver is sender (self) and scheduled into the future
    checkInfectedInvariants _ _ = False

    checkMakeContactInvariants :: AgentId
                               -> [QueueItem SIREvent]
                               -> Int
                               -> Bool
    checkMakeContactInvariants sender es contactRate
        -- make sure there has to be exactly one MakeContact event
        = invOK && hasMakeCont && numCont == contactRate
      where
        (invOK, hasMakeCont, numCont) 
          = foldr checkMakeContactInvariantsAux (True, False, 0) es

        checkMakeContactInvariantsAux :: QueueItem SIREvent 
                                      -> (Bool, Bool, Int)
                                      -> (Bool, Bool, Int)
        checkMakeContactInvariantsAux 
            (QueueItem receiver (Event (Contact sender' Susceptible)) t') (b, mkb, n)
          = (b && sender == sender'    -- the sender in Contact must be the Susceptible agent
               && receiver `elem` ais  -- the receiver of Contact must be in the agent ids
               && t == t', mkb, n+1)   -- the Contact event is scheduled immediately
        checkMakeContactInvariantsAux 
            (QueueItem receiver (Event MakeContact) t') (b, mkb, n) 
          = (b && receiver == sender   -- the receiver of MakeContact is the Susceptible agent itself
               && t' == t + 1.0        -- the MakeContact event is scheduled 1 time-unit into the future
               &&  not mkb, True, n)   -- there can only be one MakeContact event
        checkMakeContactInvariantsAux evt (_, mkb, _) = error ("failure " ++ show evt) (False, mkb)

prop_susceptible_proabilities :: Positive Double
                              -> NonEmptyList AgentId 
                              -> Property
prop_susceptible_proabilities (Positive t) (NonEmpty ais) = checkCoverage $ do
  let cor = 5
      inf = 0.05
      ild = 15.0

  let mkEvtFreq = 1
      -- will never happen as Recover will never be sent to a Susceptible
      -- agent, thus it can be set to 0, but setting it to > 1 ensures that 
      -- also the edge case is handled
      recEvtFreq = 0
      contEvtFreq = 5
      contSusEvtFreq = 1
      contInfEvtFreq = 3
      -- will never happen, as a Recovered agent does not send any event,
      -- thus it can be set to 0, but setting it to > 1 ensures that also
      -- the edge case is handled
      contRecEvtFreq = 0
      sirFreq    = (contSusEvtFreq, contInfEvtFreq, contRecEvtFreq)
      sirFreqSum = fromIntegral $ contSusEvtFreq + contInfEvtFreq + contRecEvtFreq
      evtFreqSum = fromIntegral $ mkEvtFreq + recEvtFreq + contEvtFreq

  -- compute probability for any Contac Event 
  let contEvtSplitProb = 100 * (fromIntegral contEvtFreq / evtFreqSum) 
      contInfProb      = contEvtSplitProb * (fromIntegral contInfEvtFreq / sirFreqSum)
      infProb          = contInfProb * inf

  -- generate a random event
  evt <- genEventFreq mkEvtFreq contEvtFreq recEvtFreq sirFreq ais
  -- run susceptible random agent with given parameters
  (_ai, ao, _es) <- genRunSusceptibleAgent cor inf ild t ais evt
  
  -- compute all probabilities
  let recoverPerc    = 100 * (fromIntegral recEvtFreq / evtFreqSum)
      makeContPerc   = 100 * (fromIntegral mkEvtFreq / evtFreqSum)
      contactRecPerc = contEvtSplitProb * (fromIntegral contRecEvtFreq / sirFreqSum)
      contactSusPerc = contEvtSplitProb * (fromIntegral contSusEvtFreq / sirFreqSum)
      contactInfSusPerc = contInfProb - infProb
      contactInfInfPerc = infProb
      
  return $ property $
    case evt of 
      Recover -> 
        cover recoverPerc True "Susceptible receives Recover" True
      MakeContact -> 
        cover makeContPerc True "Susceptible receives MakeContact" True
      (Contact _ Recovered) -> 
        cover contactRecPerc True "Susceptible receives Contact * Recovered" True
      (Contact _ Susceptible) -> 
        cover contactSusPerc True "Susceptible receives Contact * Susceptible" True
      (Contact _ Infected) -> 
        case ao of
          Susceptible ->
            cover contactInfSusPerc True "Susceptible receives Contact * Infected, stays Susceptible" True
          Infected ->
            cover contactInfInfPerc True "Susceptible receives Contact * Infected, becomes Infected" True
          _ ->
            cover 0 True "Susceptible recceives Contact * Infected, becomes Recovered, Impossible" False

-- NOTE: this specification implicitly covers that an infected agent never 
-- goes back to susceptible 
prop_infected_invariants :: Property
prop_infected_invariants = property $ do
    -- need a random number generator
    g  <- genStdGen
    -- generate non-empty list of agent ids, we have at least one agent
    -- the susceptible agent itself
    ais <- genNonEmptyAgentIds
    -- generate positive time
    (Positive t) <- arbitrary
    -- the infected agents id is picked randomly from all empty agent ids
    ai <- elements ais 
    -- generate a random event
    evt <- genEvent ais

    -- create susceptible agent with agent id
    let a = infectedAgent ai
    -- run agent with given event and configuration
    let (_g', _a', ao, es) = runAgent g a evt t ais

    case evt of
      Recover -> return (null es && ao == Recovered)
      
      (Contact sender Susceptible) ->
        return $ checkContactInvariants ai sender t es
      
      _ -> return (null es && ao == Infected)

  where
    checkContactInvariants :: AgentId
                           -> AgentId
                           -> Time
                           -> [QueueItem SIREvent]
                           -> Bool
    checkContactInvariants ai sender t 
        [QueueItem receiver (Event (Contact ai' Infected)) t'] -- expect exactly one Contact * Infected event
      = sender == receiver && -- receiver is the sender of the initial Contact event
        ai     == ai'      && -- agent id in Contact is the Infected agent
        t'     == t           -- scheduled immediately
    checkContactInvariants _ _ _ _  = False -- no other events expected


-- NOTE: this specification implicitly covers that the recovered agent will
-- stay recovered forever.
prop_recovered_invariant :: Property
prop_recovered_invariant = property $ do
  g            <- genStdGen
  -- must be non-empty because we have at least one agent in the simulation:
  -- the recovered itself
  ais          <- genNonEmptyAgentIds 
  (Positive t) <- arbitrary
  evt          <- genEvent ais

  let a = recoveredAgent
      (_g', _a', ao, es) = runAgent g a evt t ais

  return (null es && ao == Recovered)

--------------------------------------------------------------------------------
-- OBSOLETE TESTS
--------------------------------------------------------------------------------
-- infected agent recovering schedules event with average illnessduration
-- NOTE: not really useful, we showed the concept of coverage already 
-- prop_infected_meanIllnessDuration :: Property
-- prop_infected_meanIllnessDuration = checkCoverage $ do
--   let ild = 15.0
  
--   -- infectivity of 1 and higher contact rate to make sure high number of infections
--   durMay <- genSusceptibleAgentInfected 10 1 ild

--   let prob = 100 * expCDF (1 / ild) ild

--   return $ isJust durMay ==>
--       cover prob (fromJust durMay <= ild) 
--         ("infected have an illness duration of up to " ++ show ild ++ 
--         ", expected " ++ printf "%.2f" prob)  True

-- genSusceptibleAgentInfected :: Int
--                             -> Double
--                             -> Double
--                             -> Gen (Maybe Double)
-- genSusceptibleAgentInfected cor inf ild = do
--   g             <- genStdGen
--   (Positive t)  <- arbitrary
--   ais           <- genAgentIds
--   (Positive ai) <- arbitrary

--   let a = susceptibleAgent ai cor inf ild
--       (_g', _ag', ao, es) = runAgent g a (Contact ai Infected) t ais

--   case ao of
--     Infected -> do
--       -- in case it become infected, the agent only schedules a single 
--       -- event: recovery, to itself
--       let [QueueItem _ (Event Recover) t'] = es
--       return $ Just (t' - t)
--     _        -> return Nothing


-- recovered stays recovered never susceptible or infected
-- NOTE: already covered in recovered invariants!
-- prop_recovered_forever :: Gen Bool
-- prop_recovered_forever = do
--   g   <- genStdGen
--   ais <- genAgentIds
--   es  <- vectorOf 1000 (genEvent ais)
--   (Positive t) <- arbitrary

--   let a   = recoveredAgent
--       aos = runAgentEventsOut es g a t ais

--   return (all (==Recovered) aos)

-- -- infected never back to susceptible 
-- -- NOTE: already covered in infected invariants!
-- prop_infected_neversusceptible :: Gen Bool
-- prop_infected_neversusceptible = do
--   g   <- genStdGen
--   ais <- genAgentIds
--   es  <- vectorOf 1000 (genEvent ais) 
--   (Positive ai) <- arbitrary
--   (Positive t)  <- arbitrary

--   let a   = infectedAgent ai
--       aos = runAgentEventsOut es g a t ais

--   return (not $ any (==Susceptible) aos)


-- susceptible becomes infected on average with infectivity
-- NOTE: already covered in susceptible invariants!
-- prop_susceptible_meanInfectivity :: Property
-- prop_susceptible_meanInfectivity = checkCoverage $ do
--     let repls = 100
--     is <- vectorOf repls genMeanInfectivity

--     let confidence = 0.95
--         csTTest    = tTestSamples TwoTail infectivity (1 - confidence) is

--     return $ cover 95 (fromMaybe True csTTest) "mean infectivity" True
--   where
--     genMeanInfectivity :: Gen Double
--     genMeanInfectivity = do
--       let repls = 100
--       is <- catMaybes <$> vectorOf repls genSusceptibleAgentInfected

--       let infectedCount = length is
--           infectedRatio = (fromIntegral infectedCount / fromIntegral repls) :: Double

--       return infectedRatio

-- infected agent recovering schedules event with average illnessduration
-- NOTE: covered already by the much faster prop_infected_meanIllnessDuration test
-- prop_infected_meanIllnessDuration_ttest :: Property
-- prop_infected_meanIllnessDuration_ttest = checkCoverage $ do
--   let ild   = 15.0
--       repls = 1000

--   -- infectivity of 1 and higher contact rate to make sure high number of infections
--   is <- catMaybes <$> vectorOf repls (genSusceptibleAgentInfected 10 1 ild)

--   let confidence = 0.95
--       csTTest    = tTestSamples TwoTail ild (1 - confidence) is

--   return $ 
--     cover 95 (fromMaybe True csTTest) 
--       ("infected have a mean illness duration of " ++ show ild) True
