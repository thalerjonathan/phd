{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Maybe
import System.Random

import Test.QuickCheck
-- import Test.QuickCheck.Random

import SIR.SIR
import StatsUtils

instance Arbitrary SIRState where
  arbitrary :: Gen SIRState
  arbitrary = elements [Susceptible, Infected, Recovered]
  -- arbitrary = frequency [ (3, return Susceptible)
  --                       , (2, return Infected)
  --                       , (1, return Recovered) ]

contactRate :: Double
contactRate = 5.0

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

-- need to run replications because ABS is stochastic
replications :: Int
replications = 100

-- clear & stack test sir:sir-model-test

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 10000      -- number successful tests
                              , maxFailPercent = 100    -- number of maximum failed tests
                              , maxShrinks = 0          -- NO SHRINKS, doesn't make sense
                              --, replay = Just (mkQCGen 42, 0) -- use to replay reproducible
                              } prop_sir_sd_spec_random_size

prop_sir_sd_spec_random_size :: [SIRState] -> Gen Property
prop_sir_sd_spec_random_size as = do
  let dt = 0.01
  (ss, is, rs) <- unzip3 <$> vectorOf replications (sir as dt)
  return $ label (show $ length as) $ property $ checkSirSDspec as ss is rs

prop_sir_sd_spec_fixed_size :: Gen Property
prop_sir_sd_spec_fixed_size = do
  let dt = 0.01

  as           <- vector 100
  (ss, is, rs) <- unzip3 <$> vectorOf replications (sir as dt)

  return $ label (show $ length as) $ property $ checkSirSDspec as ss is rs

prop_sir_sd_spec_random_correlated_sir ::  [SIRState] -> Gen Bool
prop_sir_sd_spec_random_correlated_sir as = do
    let n = length as
    (ss, is, rs) <- unzip3 <$> vectorOf replications (sirSimRandomCorrelated n)
    return $ checkSirSDspec as ss is rs
  where
    sirSimRandomCorrelated :: Int -> Gen (Int, Int, Int)
    sirSimRandomCorrelated n = do
        s <- choose (0, n)
        i <- choose (0, n - s)
        let r = n - s - i
        return (s, i, r)

prop_sir_sd_spec_random_uncorrelated_sir ::  [SIRState] -> Gen Bool
prop_sir_sd_spec_random_uncorrelated_sir as = do
    let n = length as
    (ss, is, rs) <- unzip3 <$> vectorOf replications (sirSimRandomUncorr n)
    return $ checkSirSDspec as ss is rs
  where
    sirSimRandomUncorr :: Int -> Gen (Int, Int, Int)
    sirSimRandomUncorr n = do
        s <- choose (0, n)
        i <- choose (0, n)
        r <- choose (0, n)
        return (s, i, r)

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

checkSirSDspec :: [SIRState] 
               -> [Int]
               -> [Int]
               -> [Int]
               -> Bool
checkSirSDspec as ssI isI rsI = allPass
  where
    s0 = fromIntegral $ length $ filter (==Susceptible) as
    i0 = fromIntegral $ length $ filter (==Infected) as
    r0 = fromIntegral $ length $ filter (==Recovered) as

    (s, i, r) = sdSpec s0 i0 r0 contactRate infectivity illnessDuration

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
    confidence = 0.95
    sTest = tTestSamples TwoTail s (1 - confidence) ss
    iTest = tTestSamples TwoTail i (1 - confidence) is
    rTest = tTestSamples TwoTail r (1 - confidence) rs

    allPass = fromMaybe True sTest &&
              fromMaybe True iTest &&
              fromMaybe True rTest