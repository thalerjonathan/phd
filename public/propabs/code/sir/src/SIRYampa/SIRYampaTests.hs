{-# LANGUAGE Arrows #-}
module SIRYampaTests 
  ( sirYampaTests
  ) where

import Data.List
import Data.Maybe
import Data.Void
import FRP.Yampa

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIRYampa

instance Arbitrary SIRState where
  -- arbitrary :: Gen SIRState
  arbitrary = elements [Susceptible, Infected, Recovered]

paramContactRate :: Double
paramContactRate = 5.0

paramInfectivity :: Double
paramInfectivity = 0.05

paramIllnessDuration :: Double
paramIllnessDuration = 15.0

sirYampaTests :: RandomGen g
                 => g 
                 -> TestTree
sirYampaTests g 
  = testGroup 
      "SIR Yampa Tests" 
      [ test_agents_init
      , test_agent_behaviour_quickgroup g
      --, test_agent_signal_quickgroup g
      ]

test_agents_init :: TestTree
test_agents_init 
  = testGroup "agents init" 
      [ QC.testProperty "init agents" prop_initAgents ]

test_agent_behaviour_quickgroup :: RandomGen g
                                => g 
                                -> TestTree
test_agent_behaviour_quickgroup g
  = testGroup "agent behaviour"
      [ --QC.testProperty "susceptible behaviour" (testCaseSusceptible g)
       QC.testProperty "infected behaviour" (testCaseInfected g) ]

test_agent_signal_quickgroup :: RandomGen g
                             => g 
                             -> TestTree
test_agent_signal_quickgroup g
  = testGroup "agent signal behaviour"
      [ QC.testProperty "susceptible signal behaviour" (testCaseSusceptibleSignal g)
      , QC.testProperty "infected signal behaviour" (testCaseInfectedSignal g)]

prop_initAgents :: NonNegative Int -> NonNegative Int -> Bool
prop_initAgents (NonNegative susceptibleCount) (NonNegative infectedCount)
    = length as == susceptibleCount + infectedCount &&
      sc        == susceptibleCount &&
      ic        == infectedCount &&
      notElem Recovered as
  where 
    as = initAgents susceptibleCount infectedCount 

    sc = length $ filter (==Susceptible) as
    ic = length $ filter (==Infected) as

-- | Testing whether a susceptible agent
-- behaves as a signal: does the susceptible agent
-- depend on time, more precicelsy does it NOT change
-- when time does not advance?
testCaseSusceptibleSignal :: RandomGen g
                          => g 
                          -> [SIRState]
                          -> Bool
testCaseSusceptibleSignal g0 as
    = 0 == countInfTotal
  where
    repls         = 10000
    countInfTotal = testSusceptibleSignalAux g0 repls 0

    testSusceptibleSignalAux :: RandomGen g 
                      => g
                      -> Int
                      -> Int
                      -> Int
    testSusceptibleSignalAux _ 0 countInf = countInf
    testSusceptibleSignalAux g n countInf
        = testSusceptibleSignalAux g'' (n-1) countInf'
      where
        (g', g'')   = split g

        dt          = 0
        stepsCount  = 100
        steps       = replicate stepsCount (dt, Nothing)

        ret         = embed (testSusceptibleSF as g') ((), steps)
        gotInfected = True `elem` ret

        countInf'   = if gotInfected then countInf + 1 else countInf

-- | Testing behaviour of susceptible agent
--    a susceptible agent makes on average contact
--    with contact rate other agents per time-unit
--    => when running N susceptible agents for 1.0
--    time-unit then
--    N * infectivity * contactRate * (infectedAgentsCount / totalAgentsCount) 
--    agents should become infected
--   NOTE: this also allows to estimate the dt: to 
--   achieve a sufficiently close match one selects
--   a very small epsilon and then reduces the dt
--   until the average falls into the epsilon environment
-- NOTE: this is black-box verification
testCaseSusceptible :: RandomGen g
                    => g 
                    -> [SIRState]
                    -> Bool
testCaseSusceptible g0 as = diff <= eps
  where
    repls = 10000 -- TODO: how to select a 'correct' number of runs? conjecture: n -> infinity lets go the error (eps) to 0
    eps   = 0.1   -- TODO: how to select a 'correct' epsilon? conjecture: probably it depends on the ratio between dt and contact rate?
    dt    = 0.125   -- NOTE: to find out a suitable dt use the test itself: start e.g. with 1.0 and always half when test fail until it succeeds
    diff  = testSusceptible

    infectivity = paramInfectivity
    contactRate = paramContactRate

    testSusceptible :: Double -- ^ returns difference to target
    testSusceptible = abs (target - countFract)
      where
        -- we have n other agents, each in one of the states
        -- this means, that this susceptible agent will pick
        -- on average an Infected with a probability of 1/n
        otherAgentsCount  = length as
        infOtherAgents    = length $ filter (Infected==) as
        infToNonInfRatio
          -- prevent division by zero
          = if 0 == otherAgentsCount 
              then 0
              else fromIntegral infOtherAgents / fromIntegral otherAgentsCount

        countTotal = testSusceptibleAux g0 repls 0
        countFract = fromIntegral countTotal / fromIntegral repls
        -- multiply with contactRate because we make on 
        -- average contact with contactRate other agents 
        -- per time-unit (we are running the agent for
        -- 1.0 time-unit)
        -- also multiply with ratio of infected to non-infected
        --   TODO: can we extract the formula of the SD approach
        --   here? should be possible, then we can prove that our
        --   ABS approach is indeed a valid SD approximation
        --   (due to averaging) 
        target = infectivity * contactRate * infToNonInfRatio

        testSusceptibleAux :: RandomGen g 
                          => g
                          -> Int
                          -> Int
                          -> Int
        testSusceptibleAux _ 0 countInf = countInf
        testSusceptibleAux g n countInf 
            = testSusceptibleAux g'' (n-1) countInf'
          where
            (g', g'')   = split g

            stepsCount  = floor (1.0 / dt)
            steps       = replicate stepsCount (dt, Nothing)

            ret         = embed (testSusceptibleSF as g') ((), steps)
            gotInfected = True `elem` ret

            countInf'   = if gotInfected then countInf + 1 else countInf

testSusceptibleSF :: RandomGen g 
                  => [SIRState]
                  -> g
                  -> SF () Bool
testSusceptibleSF otherAgents g = proc _ -> do
  ret <- susceptibleAgent paramContactRate paramInfectivity paramIllnessDuration g -< otherAgents
  case ret of 
    Susceptible -> returnA -< False
    Infected    -> returnA -< True
    Recovered   -> returnA -< False -- TODO: should never occur, can we test this? seems not so, but we can pretty easily guarantee it due to simplicity of code

-- | Testing signal behaviour of infected agent
testCaseInfectedSignal :: RandomGen g 
                       => g
                       -> [SIRState]
                       -> Bool
testCaseInfectedSignal g0 as 
    = 0 == countRecTotal
  where
    repls         = 10000
    countRecTotal = testInfectedSignal g0 repls 0

    testInfectedSignal :: RandomGen g 
                    => g
                    -> Int
                    -> Int
                    -> Int
    testInfectedSignal _ 0 countRec = countRec
    testInfectedSignal g n countRec 
        = testInfectedSignal g'' (n-1) countRec'
      where
        (g', g'')    = split g
        dt           = 0
        stepsCount   = 100
        steps        = replicate stepsCount (dt, Nothing)
        evts         = embed (testInfectedSF g' as) ((), steps)
        mayRec       = find isEvent evts
        countRec'    = if isJust mayRec then countRec + 1 else countRec

-- | Testing behaviour of infected agent
--   run test until agent recovers, which happens
--   on average after illnessDuration
--   we are running this test a larger number of 
--   times N and averaging the durations of all
--   agents until their recovery
--   should be within an epsilon of illnessDuration
-- NOTE: this is black-box verification
testCaseInfected :: RandomGen g 
                 => g
                 -> [SIRState]
                 -> Bool
testCaseInfected g0 as = diff <= eps
  where
    repls = 10000 -- TODO: how to select a 'correct' number of runs? conjecture: as n -> inf, so goes the error (eps) to 0
    eps   = 0.5   -- TODO: how to select a 'correct' epsilon? conjecture: probably it depends on ratio between dt and illnessduration?
    dt    = 0.25   -- NOTE: to find out a suitable dt use the test itself: start e.g. with 1.0 and always half when test fail until it succeeds
    diff  = testInfected

    testInfected :: Double   -- ^ difference to target
    testInfected = abs (target - durationsAvg)
      where
        durations    = testInfectedAux g0 repls []
        durationsAvg = sum durations / fromIntegral (length durations)
        
        target = paramIllnessDuration
        
        testInfectedAux :: RandomGen g 
                        => g
                        -> Int
                        -> [Double]
                        -> [Double]
        testInfectedAux _ 0 acc = acc
        testInfectedAux g n acc 
            = testInfectedAux g'' (n-1) acc'
          where
            (g', g'')    = split g
            steps        = repeat (dt, Nothing)
            evts         = embed (testInfectedSF g' as) ((), steps)
            -- assumption about implementation: there will always be an event, testInfectedSF will eventuall return an event 
            (Event t)    = fromJust $ find isEvent evts
            acc'         = t : acc

testInfectedSF :: RandomGen g 
              => g
              -> [SIRState]
              -> SF () (Event Time)
testInfectedSF g otherAgents = proc _ -> do
  -- note that the otheragents are fed into the infected agents
  -- but that they are ignored for checking whether the test
  -- has failed or not. from this we can infer, that they
  -- don't play a role in the infected agents behaviour at all
  ret <- infectedAgent paramIllnessDuration g -< otherAgents
  t <- time -< ()
  case ret of 
    Susceptible -> returnA -< NoEvent -- TODO: should never occur, can we test this? seems not so, but we can pretty easily guarantee it due to simplicity of code
    Infected    -> returnA -< NoEvent
    Recovered   -> returnA -< Event t 

-- | Testing behaviour of recovered agent
--   A correct recovered agent will stay recovered
--   forever, which cannot be tested through 
--   computation. We can reason that it is correct
--   simply by looking at the code, which 
--   is so simple (1 line) that it is correct 
--   by definition: its basically a constant function
--   We indicated by Void and undefined that this
--   test does not make sense.
_testCaseRecovered :: Void
_testCaseRecovered = undefined

-- TODO: integration tests: susceptible, infected and recovered all together / the whole simulation