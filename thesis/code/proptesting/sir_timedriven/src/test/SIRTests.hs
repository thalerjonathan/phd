module SIRTests 
  ( sirPropTests
  ) where

import Control.Parallel.Strategies hiding (r0)
import Data.Maybe
import FRP.Yampa

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIR.SIR
import SIR.Utils
import StatsUtils

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
  = testGroup "SIR Simulation Tests" [ test_agent_behaviour_quickgroup g ]

test_agent_behaviour_quickgroup :: RandomGen g
                                => g 
                                -> TestTree
test_agent_behaviour_quickgroup g
  = testGroup "agent behaviour"
      [ QC.testProperty "prop_sd_rates" (prop_sd_rates g)]

prop_sd_rates :: RandomGen g
              => g 
              -> [SIRState]
              -> Bool
prop_sd_rates g0 as = trace ("---------------------------------------------------------------------------------------" ++
                             "\n s0 = " ++ show s0 ++ ", i0 = " ++ show i0 ++ ", r0 = " ++ show r0 ++ ", n = " ++ show n ++
                             -- "\n ir = " ++ show ir ++ ", rr = " ++ show rr ++ 
                             "\n s = " ++ show s ++ ", i = " ++ show i ++ ", r = " ++ show r ++
                             "\n ss mean = " ++ show ssMean ++ ", is mean = " ++ show isMean ++ ", rs mean = " ++ show rsMean) 
                             allPass
  where
    s0 = fromIntegral $ length $ filter (==Susceptible) as
    i0 = fromIntegral $ length $ filter (==Infected) as
    r0 = fromIntegral $ length $ filter (==Recovered) as
    n  = s0 + i0 + r0

    beta  = paramContactRate
    gamma = paramInfectivity
    delta = paramIllnessDuration
    
    -- compute infection-rate according to SD specifications (will be 0 if no
    -- agents) from generated agent-population as (and fixed model parameters)
    ir = if n == 0 then 0 else (i0 * beta * s0 * gamma) / n
    -- recovery-rate according to SD specifications from generated 
    -- agent-population as (and fixed model parameters=
    rr = i0 / delta

    -- S value after 1 time-unit according to SD specification: 
    --    subtract infection-rate from initial S value
    s = s0 - ir
    -- I value after 1 time-unit according to SD specification:
    --    add infection-rate minus recovery-rate to initial I value
    i = i0 + (ir - rr)
    -- R value after 1 time-unit according to SD specifications:
    --    add recovery-rate to initial R value
    r = r0 + rr

    -- run for 1 time-unit
    dur = 1.0
    -- small dt: TODO: testing for a sufficiently small dt
    dt = 0.01
    -- need to run replications because ABS is stochastic
    repls = 1000
    -- generate random-number generator for each replication
    (rngs, _) = rngSplits g0 repls []
    -- compute simulated values for s, i and r
    sir  = map (last . runSIRFor dur dt as beta gamma delta) rngs
    -- 
    sir' = withStrategy (parListChunk 200 rseq) sir
    (ss, is, rs) = unzip3 sir'

    ssMean = mean ss
    isMean = mean is
    rsMean = mean rs
    
    -- Perform a 2-sided test because we test if the means are statistically equal
    -- put in other words: if the difference of the means is statistically insignificant.
    -- Thus use a 2-sided t-test because we check for hypothetical equality(EQ) of the mean
    confidence = 0.95
    sTest = tTest "susceptible mean t-test" ss s (1 - confidence) EQ
    iTest = tTest "infected mean t-test" is i (1 - confidence) EQ
    rTest = tTest "recovered mean t-test" rs r (1 - confidence) EQ

    sTestPass
      | isNothing sTest = True
      | isJust sTest    = True
      | otherwise       = trace "suscepible t-test failed!" False

    iTestPass
      | isNothing iTest = True
      | isJust iTest    = True
      | otherwise       = trace "infected t-test failed!" False

    rTestPass
      | isNothing rTest = True
      | isJust rTest    = True
      | otherwise       = trace "recovered t-test failed!" False

    allPass = sTestPass && iTestPass && rTestPass