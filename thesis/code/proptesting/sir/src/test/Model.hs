{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Maybe

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIR.Model
import SIR.SD
import Utils.GenSIR
import Utils.GenEventSIR
import Utils.GenTimeSIR
import Utils.Stats

import Debug.Trace

-- need to run replications because ABS is stochastic
replications :: Int
replications = 100

-- --quickcheck-replay=557780
-- --quickcheck-tests=1000
-- --quickcheck-verbose
-- --test-arguments=""
-- clear & stack test sir:sir-model-tests

main :: IO ()
main = do
  let t = testGroup "SIR Spec Tests" 
          [ 
          --  QC.testProperty "SIR time-driven" prop_sir_time_spec
            QC.testProperty "SIR event-driven" prop_sir_event_spec
          ]

  defaultMain t

-- TODO: compare time-driven and event-driven with each other, but how? 
-- need a different type of t-test! 

-- OK (5784.64s)
--     +++ OK, passed 100 tests (71% SIR time-driven passes t-test with simulated SD).
    
--     Only 71% SIR time-driven passes t-test with simulated SD, but expected 90%

-- NOTE: don't run with checkCoverage for now because each test-case can take
-- a considerable amount of time, so restrict to 100 runs to estimate a rough
-- coverage
prop_sir_time_spec :: Positive Double  -- ^ contact rate
                   -> UnitRange        -- ^ infectivity, within range (0,1)
                   -> Positive Double  -- ^ illness duration
                   -> TimeRange        -- ^ time to run
                   -> Property
prop_sir_time_spec 
    (Positive cor) (UnitRange inf) (Positive ild) (TimeRange t) = property $ do
  let repls = 100

  -- generate large random population
  as <- resize 1000 (listOf genSIRState)
  -- run replications
  (ss, is, rs) <- unzip3 <$> genTimeSIRRepls repls as cor inf ild 0.01 t
  -- check if they match 
  let prop = compareSDToABS as ss is rs cor inf ild t

  -- we expect 80% to pass, use checkCoverage to get statistical robust result
  -- Note that we return True in every case, we are only interested in the
  -- coverage and allow failure!
  return $ trace (show prop) 
    cover 90 prop "SIR time-driven passes t-test with simulated SD" True

-- OK (3339.98s)
--     +++ OK, passed 100 tests (37% SIR event-driven passes t-test with simulated SD).
    
--     Only 37% SIR event-driven passes t-test with simulated SD, but expected 90%

-- NOTE: don't run with checkCoverage for now because each test-case can take
-- a considerable amount of time, so restrict to 100 runs to estimate a rough
-- coverage
prop_sir_event_spec :: Positive Double  -- ^ contact rate
                    -> UnitRange        -- ^ infectivity, within range (0,1)
                    -> Positive Double  -- ^ illness duration
                    -> TimeRange        -- ^ time to run
                    -> Property
prop_sir_event_spec 
    (Positive cor) (UnitRange inf) (Positive ild) (TimeRange t) = property $ do
  let repls = 100

  -- generate large random population
  as <- resize 1000 (listOf genSIRState)
  -- run replications
  (ss, is, rs) <- unzip3 <$> genEventSIRRepls repls as cor inf ild (-1) t
  -- check if they match 
  let prop = compareSDToABS as ss is rs cor inf ild t

  -- we expect 80% to pass, use checkCoverage to get statistical robust result
  -- Note that we return True in every case, we are only interested in the
  -- coverage and allow failure!
  return $ trace (show prop) 
    cover 90 prop "SIR event-driven passes t-test with simulated SD" True

-- NOTE: need to iterate using a (correct) SD implementation, just computing 
-- the expected after 1.0 time without iterating does NOT WORK! OTherwise we 
-- could simply compute the value at 150 bcs it is linear but obviously that
-- does not work because we have feedback (integral!)
compareSDToABS :: [SIRState] 
               -> [Int]
               -> [Int]
               -> [Int]
               -> Double
               -> Double
               -> Double
               -> Double
               -> Bool
compareSDToABS as ssI isI rsI cor inf ild t = allPass
  where
    (s0,i0,r0) = int3ToDbl3 $ aggregateSIRStates as
    (s, i, r)  = snd . last $ runSIRSD s0 i0 r0 cor inf ild t    

    -- Perform a 2-tailed t-test with H0 (null hypothesis) that the means are 
    -- equal with a confidence of 99%: the probability of observing an extreme
    -- test statistics, rejecting the H0 hypothesis, should be 1%. Put other
    -- wise: we only reject true null hypotheses with a chance of 1%
    -- By increasing confidence, we lower the willingness to make Type I errors
    -- but increase the risk of making Type II errors:
    --  Type I error rejects a true null hypothesis: leading to a failed test
    --    claiming the means are NOT equal when in fact they are equal
    --  Type II error fails to reject a false null hypothesis: leading to an
    --    accepted test claiming that the mans are equal when in fact they are NOT
    confidence = 0.95
    -- transform data from Int to Double
    ss = map fromIntegral ssI
    is = map fromIntegral isI
    rs = map fromIntegral rsI
    -- run t-tests
    sTest = tTestSamples TwoTail s (1 - confidence) ss
    iTest = tTestSamples TwoTail i (1 - confidence) is
    rTest = tTestSamples TwoTail r (1 - confidence) rs
    -- pass if all 3 tests pass
    allPass = fromMaybe True sTest &&
              fromMaybe True iTest &&
              fromMaybe True rTest

simulateSD :: Double 
           -> Double 
           -> Double
           -> Double 
           -> Double
           -> Double
           -> Double
           -> (Double, Double, Double)
simulateSD s0 i0 r0 beta gamma delta t 
  = snd . last $ runSIRSD s0 i0 r0 beta gamma delta t

-- -- TIME, RANDOM, LARGE, INTERPOLATED: 0% pass, all fail, because interpolation does not
-- -- work in large populations and random model parameters
-- prop_sir_time_rand_interp_spec :: Positive Double  -- ^ contact rate
--                         -> UnitRange        -- ^ infectivity, within range (0,1)
--                         -> Positive Double  -- ^ illness duration
--                         -> TimeRange  -- ^ time to run
--                         -> Property
-- prop_sir_time_rand_interp_spec 
--     (Positive cor) (UnitRange inf) (Positive ild) (TimeRange t) = property $ do
--   as <- resize 1000 (listOf genSIRState)

--   (ss, is, rs) <- unzip3 . map snd <$> vectorOf replications (genLastTimeSIR as cor inf ild 0.01 t)
--   let prop = compareSDToABS as ss is rs cor inf ild Nothing
--   return $ trace (show prop) cover 90 prop "SIR random model, large population, time-driven passes t-test with interpolated SD" True


-- -- TIME, FIXED, SMALL, INTERPOLATED: around 84% pass
-- prop_sir_time_spec :: Property
-- prop_sir_time_spec  = checkCoverage $ do
--   let t = 1.0
--       cor = 5
--       inf = 0.05
--       ild = 15
--   as <- listOf genSIRState

--   (ss, is, rs) <- unzip3 . map snd <$> vectorOf replications (genLastTimeSIR as cor inf ild 0.01 t)
--   let prop = checkSirSDSpec as ss is rs cor inf ild Nothing
--   return $ cover 90 prop "SIR time-driven passes t-test with SD averages" True

-- -- TIME, FIXED, SMALL, SIMULATED: around 82% pass
-- prop_sir_time_sd_spec :: Property
-- prop_sir_time_sd_spec = checkCoverage $ do
--   let t = 1.0
--       cor = 5
--       inf = 0.05
--       ild = 15
--   as <- listOf genSIRState

--   (ss, is, rs) <- unzip3 . map snd <$> vectorOf replications (genLastTimeSIR as cor inf ild 0.01 t)
--   let prop = checkSirSDSpec as ss is rs cor inf ild (Just t)
--   return $ cover 90 prop "SIR time-driven passes t-test with SD runs" True

-- -- OK: 0% pass or 1% in case its empty
-- prop_sir_sd_spec_random_interpolated :: Property
-- prop_sir_sd_spec_random_interpolated = property $ do
--     as <- resize 1000 (listOf genSIRState)

--     let cor = 5
--         inf = 0.05
--         ild = 15
--         n = length as

--     (ss, is, rs) <- unzip3 <$> vectorOf replications (sirSimRandom n)

--     let prop = checkSirSDSpec as ss is rs cor inf ild Nothing

--     return $ cover 0 prop "SIR random correlated, interpolated SD" True

-- -- OK: 0% pass
-- prop_sir_sd_spec_random_sim :: Property
-- prop_sir_sd_spec_random_sim = property $ do
--     as <- resize 1000 (listOf genSIRState)

--     let t = 1.0
--         cor = 5
--         inf = 0.05
--         ild = 15
--         n = length as

--     (ss, is, rs) <- unzip3 <$> vectorOf replications (sirSimRandom n)

--     let prop = checkSirSDSpec as ss is rs cor inf ild (Just t)

--     return $ cover 0 prop "SIR random correlated, simulated SD" True

-- sirSimRandom :: Int -> Gen (Int, Int, Int)
-- sirSimRandom n = do
--   s <- choose (0, n)
--   i <- choose (0, n - s)
--   let r = n - s - i
--   return (s, i, r)

-- -- NOTE: this doesn't work! we are integrating over infinitesimal small steps
-- -- thus the larger the step the more inaccurate. If this would work, we would
-- -- not need the feedback/integral and it would be a simple linear equation 
-- -- problem...
-- interpolateSD :: Double 
--               -> Double 
--               -> Double
--               -> Double 
--               -> Double
--               -> Double
--               -> (Double, Double, Double)
-- interpolateSD s0 i0 r0 beta gamma delta = (s, i, r)
--   where
--     n = s0 + i0 + r0

--     -- compute infection-rate according to SD specifications (will be 0 if no
--     -- agents) from generated agent-population as (and fixed model parameters)
--     ir = if n == 0 then 0 else (i0 * beta * s0 * gamma) / n
--     -- recovery-rate according to SD specifications from generated 
--     -- agent-population as (and fixed model parameters=
--     rr = i0 / delta

--     -- S value after 1 time-unit according to SD specification: 
--     --    subtract infection-rate from initial S value
--     s = s0 - ir
--     -- I value after 1 time-unit according to SD specification:
--     --    add infection-rate minus recovery-rate to initial I value
--     i = i0 + (ir - rr)
--     -- R value after 1 time-unit according to SD specifications:
--     --    add recovery-rate to initial R value
--     r = r0 + rr