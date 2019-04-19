{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Maybe
import System.Random

import Test.QuickCheck
--import Test.QuickCheck.Random

import SIR.SIR
import StatsUtils

--import Debug.Trace
import Text.Printf

instance Arbitrary SIRState where
  arbitrary :: Gen SIRState
  arbitrary = elements [Susceptible, Infected, Recovered]
  -- arbitrary = frequency [ (3, return Susceptible)
  --                       , (2, return Infected)
  --                       , (1, return Recovered) ]
contactRate :: Int
contactRate = 5

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

-- need to run replications because ABS is stochastic
replications :: Int
replications = 100

-- clear & stack test sir-event:sir-model-test

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 100        -- number successful tests
                              , maxFailPercent = 100    -- number of maximum failed tests
                              , maxShrinks = 0          -- NO SHRINKS, they count towards successful tests, biasing the percentage
                              --, replay = Just (mkQCGen 42, 0) -- use to replay reproducible
                              } prop_sir_sd_random_size

prop_sir_sd_random_size :: [SIRState] -> Gen Bool
prop_sir_sd_random_size as = do
  -- TODO: use resize 1000 (listOf genSIRState) to ensure larger population sizes
  (ss, is, rs) <- unzip3 <$> vectorOf replications (sir as)
  return $ checkSirSDSpec as ss is rs

prop_sir_sd_fixed_size :: Gen Bool
prop_sir_sd_fixed_size = do
  -- TODO: use resize 1000 (listOf genSIRState)  to ensure larger population sizes
  as           <- vector 100
  (ss, is, rs) <- unzip3 <$> vectorOf replications (sir as)
  return $ checkSirSDSpec as ss is rs

labelPopulation :: [SIRState] -> String
labelPopulation as = ss ++ ", " ++ is ++ ", " ++ rs
  where
    s = fromIntegral $ length $ filter (==Susceptible) as
    i = fromIntegral $ length $ filter (==Infected) as
    r = fromIntegral $ length $ filter (==Recovered) as
    n = fromIntegral $ length as

    ss = printf "%.2f" ((s / n) :: Double)
    is = printf "%.2f" (i / n)
    rs = printf "%.2f" (r / n)

sir :: [SIRState] -> Gen (Int, Int, Int)
sir as = do
    seed <- choose (minBound, maxBound)
    let g = mkStdGen seed
    return $ snd $
            last $ 
            fst $ runSIR as contactRate infectivity illnessDuration (-1) 1.0 g

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

checkSirSDSpec :: [SIRState] 
               -> [Int]
               -> [Int]
               -> [Int]
               -> Bool
checkSirSDSpec as ssI isI rsI = allPass
  -- = trace ( "---------------------------------------------------------------------------------------" ++
  --           "\n s0 = " ++ show s0 ++ ", \t i0 = " ++ show i0 ++ ", \t r0 = " ++ show r0 ++ ", \t n = " ++ show n ++
  --           "\n s  = " ++ printf "%.2f" s ++ ", \t i  = " ++ printf "%.2f" i ++ ", \t r  = " ++ printf "%.2f" r ++ 
  --           "\n ss = " ++ printf "%.2f" _ssMean ++ ", \t is = " ++ printf "%.2f" _isMean ++ ", \t rs = " ++ printf "%.2f" _rsMean) 
  --           allPass
  where
    s0 = fromIntegral $ length $ filter (==Susceptible) as
    i0 = fromIntegral $ length $ filter (==Infected) as
    r0 = fromIntegral $ length $ filter (==Recovered) as
    
    (s, i, r) = sdSpec s0 i0 r0 (fromIntegral contactRate) infectivity illnessDuration

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

    -- _ssMean = mean ss
    -- _isMean = mean is
    -- _rsMean = mean rs
    