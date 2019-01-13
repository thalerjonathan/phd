{-# LANGUAGE Arrows #-}
module SIRTests 
  ( sirPropTests
  ) where

import Data.List
import Data.Maybe
import Data.Void
import FRP.Yampa

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIR.SIR

import Debug.Trace

instance Arbitrary SIRState where
  -- arbitrary :: Gen SIRState
  arbitrary = elements [Susceptible, Infected, Recovered]

paramContactRate :: Double
paramContactRate = 5.0

paramInfectivity :: Double
paramInfectivity = 0.05

paramIllnessDuration :: Double
paramIllnessDuration = 15.0

sirPropTests :: RandomGen g
             => g 
             -> TestTree
sirPropTests g 
  = testGroup "SIR Simulation Tests" [ test_agent_behaviour_quickgroup g]

test_agent_behaviour_quickgroup :: RandomGen g
                                => g 
                                -> TestTree
test_agent_behaviour_quickgroup g
  = testGroup "agent behaviour"
      [ QC.testProperty "infected behaviour = recovery rate" (prop_recovery_rate g) 
      , QC.testProperty "susceptible behaviour = infection rate" (prop_infection_rate g)
      , QC.testProperty "infected behaviour = average duration" (prop_avg_duration g)
      ]

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
prop_infection_rate :: RandomGen g
                    => g 
                    -> [SIRState]
                    -> Bool
prop_infection_rate g0 as = diff <= eps
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

-- | Testing behaviour of infected agent
--   run test until agent recovers, which happens
--   on average after illnessDuration
--   we are running this test a larger number of 
--   times N and averaging the durations of all
--   agents until their recovery
--   should be within an epsilon of illnessDuration
-- NOTE: this is black-box verification
prop_avg_duration :: RandomGen g 
                  => g
                  -> [SIRState]
                  -> Bool
prop_avg_duration g0 as = diff <= eps
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

prop_recovery_rate :: RandomGen g 
                   => g
                   -> Bool
prop_recovery_rate g0 = trace ("target = " ++ show target ++ " actual = " ++ show actual) diff <= eps
  where
    inf   = 10000 
    repls = 100 :: Int -- TODO: how to select a 'correct' number of runs? conjecture: as n -> inf, so goes the error (eps) to 0
    eps   = 0.5   -- TODO: how to select a 'correct' epsilon? conjecture: probably it depends on ratio between dt and illnessduration?
    dt    = 0.1   -- NOTE: to find out a suitable dt use the test itself: start e.g. with 1.0 and always half when test fail until it succeeds
    
    actuals = testInfected g0 repls []
    avgActuals = fromIntegral (sum actuals) / fromIntegral (length actuals)

    -- TODO: maybe a t-test?

    target = fromIntegral inf / paramIllnessDuration
    actual = avgActuals / paramIllnessDuration
 
    diff = abs (target - actual)

    testInfected :: RandomGen g 
                 => g
                 -> Int
                 -> [Int]
                 -> [Int]
    testInfected _ 0 acc = acc
    testInfected g n acc = testInfected g'' (n-1) acc'
      where
        (g', g'') = split g
        recCount  = testInfectedAux g' inf 0
        acc'      = recCount : acc

        testInfectedAux :: RandomGen g 
                        => g
                        -> Int
                        -> Int
                        -> Int
        testInfectedAux _ 0 countRec = countRec
        testInfectedAux ga i countRec 
            = testInfectedAux ga'' (i-1) countRec'
          where
            (ga', ga'') = split ga

            stepsCount  = floor (1.0 / dt)
            steps       = replicate stepsCount (dt, Nothing)

            evts        = embed (testInfectedSF ga' []) ((), steps)
            recovered   = isJust $ find isEvent evts

            countRec'   = if recovered then countRec + 1 else countRec

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