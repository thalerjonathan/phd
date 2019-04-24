{-# LANGUAGE InstanceSigs #-}
module Main where

--import Data.Maybe

import Test.Tasty
import Test.Tasty.QuickCheck as QC

--import SIR.Event
import SIR.Model
import SIR.SD
--import SIR.Time
import Utils.GenSIR
import Utils.GenEventSIR
--import Utils.GenTimeSIR
import Utils.Stats

--import Debug.Trace

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
            QC.testProperty "SIR event-driven" prop_sir_event_spec
          ]

  defaultMain t

-- TODO: need to iterate using a (correct) SD implementation, just computing 
-- the expected after 1.0 time without iterating does NOT WORK! OTherwise we 
-- could simply compute the value at 150 bcs it is linear but obviously that
-- does not work because we have feedback (integral!)

-- TODO: question is how to compare? using t-test? using mean and using nearlyEqual?

-- TODO: compare time-driven and event-driven with each other

prop_sir_event_spec :: Positive Int 
                    -> Positive Double 
                    -> Positive Double 
                    -> Positive Double 
                    -> Property
prop_sir_event_spec (Positive cor) (Positive inf) (Positive ild) (Positive _t) = checkCoverage $ do
  let t = 1.0
  as <- resize 1000 (listOf genSIRState)
  (ss, is, rs) <- unzip3 . map snd . last <$> vectorOf replications (genEventSIR as cor inf ild (-1) t)

  let prop = checkSirSDSpec as ss is rs cor inf ild t

  return $ cover 90 prop "ABS averages SIR spec" True


-- prop_sir_time_random :: Property
-- prop_sir_time_random = checkCoverage $ do
--   as <- listOf genSIRState
--   (ss, is, rs) <- unzip3 <$> vectorOf replications (genLastSir as)
--   let prop = checkSirSDSpec as ss is rs

--   return $ cover 90 prop "ABS averages SIR spec" True

-- prop_sir_sd :: Gen Bool
-- prop_sir_sd = trace (show $ nearlyEqual 100 99.9 0.0005) $ do
--   as <- resize 1000 (listOf genSIRState)
--   let s0 = fromIntegral $ length (filter (==Susceptible) as)
--   let i0 = fromIntegral $ length (filter (==Infected) as)
--   let r0 = fromIntegral $ length (filter (==Recovered) as)

--   let (ss, is, rs) = snd $ last $ runSIRSD s0 i0 r0 (fromIntegral contactRate) infectivity illnessDuration 1 0.001

--   let (s, i, r) = sdSpec s0 i0 r0 (fromIntegral contactRate) infectivity illnessDuration

--   let epsilon = 0.005

--   let prop = nearlyEqual ss s epsilon && 
--              nearlyEqual is i epsilon && 
--              nearlyEqual rs r epsilon

--   return prop

-- prop_sir_sd_random_size :: Property
-- prop_sir_sd_random_size = checkCoverage $ do
--   as <- listOf genSIRState
--   (ss, is, rs) <- unzip3 <$> vectorOf replications (genSIRLast 0.01 as)
--   let prop = checkSirSDspec as ss is rs

--   return $ cover 90 prop "ABS averages SIR spec" True

-- prop_sir_sd_spec_random_correlated_sir ::  [SIRState] -> Gen Bool
-- prop_sir_sd_spec_random_correlated_sir as = do
--     let n = length as
--     (ss, is, rs) <- unzip3 <$> vectorOf replications (sirSimRandomCorrelated n)
--     return $ checkSirSDspec as ss is rs
--   where
--     sirSimRandomCorrelated :: Int -> Gen (Int, Int, Int)
--     sirSimRandomCorrelated n = do
--         s <- choose (0, n)
--         i <- choose (0, n - s)
--         let r = n - s - i
--         return (s, i, r)

-- prop_sir_sd_spec_random_uncorrelated_sir ::  [SIRState] -> Gen Bool
-- prop_sir_sd_spec_random_uncorrelated_sir as = do
--     let n = length as
--     (ss, is, rs) <- unzip3 <$> vectorOf replications (sirSimRandomUncorr n)
--     return $ checkSirSDspec as ss is rs
--   where
--     sirSimRandomUncorr :: Int -> Gen (Int, Int, Int)
--     sirSimRandomUncorr n = do
--         s <- choose (0, n)
--         i <- choose (0, n)
--         r <- choose (0, n)
--         return (s, i, r)

-- TODO: this doesn't work! we are integrating over infinitesimal small steps
-- thus the larger the step the more inaccurate
sdSpec :: Double 
       -> Double 
       -> Double
       -> Double 
       -> Double
       -> Double
       -> (Double, Double, Double)
sdSpec s0 i0 r0 beta gamma delta = (s, i, r)
  where
    n = s0 + i0 + r0

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

sdRun :: Double 
      -> Double 
      -> Double
      -> Double 
      -> Double
      -> Double
      -> Double
      -> (Double, Double, Double)
sdRun s0 i0 r0 beta gamma delta t
  = snd . last $ runSIRSD s0 i0 r0 beta gamma delta t 0.001

checkSirSDSpec :: [SIRState] 
               -> [Int]
               -> [Int]
               -> [Int]
               -> Int
               -> Double
               -> Double
               -> Double
               -> Bool
checkSirSDSpec as ssI isI rsI cor inf ild t = allPass
  -- = trace ( "---------------------------------------------------------------------------------------" ++
  --           "\n s0 = " ++ show s0 ++ ", \t i0 = " ++ show i0 ++ ", \t r0 = " ++ show r0 ++ ", \t n = " ++ show n ++
  --           "\n s  = " ++ printf "%.2f" s ++ ", \t i  = " ++ printf "%.2f" i ++ ", \t r  = " ++ printf "%.2f" r ++ 
  --           "\n ss = " ++ printf "%.2f" _ssMean ++ ", \t is = " ++ printf "%.2f" _isMean ++ ", \t rs = " ++ printf "%.2f" _rsMean) 
  --           allPass
  where
    s0 = fromIntegral $ length $ filter (==Susceptible) as
    i0 = fromIntegral $ length $ filter (==Infected) as
    r0 = fromIntegral $ length $ filter (==Recovered) as
    
    (s, i, r) = sdRun s0 i0 r0 (fromIntegral cor) inf ild t

    -- transform data from Int to Double
    ss = map fromIntegral ssI
    is = map fromIntegral isI
    rs = map fromIntegral rsI
    
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
    -- confidence = 0.95
    -- sTest = tTestSamples TwoTail s (1 - confidence) ss
    -- iTest = tTestSamples TwoTail i (1 - confidence) is
    -- rTest = tTestSamples TwoTail r (1 - confidence) rs

    -- allPass = fromMaybe True sTest &&
    --           fromMaybe True iTest &&
    --           fromMaybe True rTest

    ssMean = mean ss
    isMean = mean is
    rsMean = mean rs

    epsilon = 0.001

    allPass = nearlyEqual ssMean s epsilon && 
              nearlyEqual isMean i epsilon && 
              nearlyEqual rsMean r epsilon


-- genLastSir :: [SIRState] -> Gen (Int, Int, Int)
-- genLastSir as = do
--   ret <- map snd <$> genSimulationSIR as contactRate infectivity illnessDuration (-1) 1.0 
--   if null ret
--     then do
--       let s = length (filter (==Susceptible) as)
--       let i = length (filter (==Infected) as)
--       let r = length (filter (==Recovered) as)
--       return (s,i,r)
--     else return (last ret)