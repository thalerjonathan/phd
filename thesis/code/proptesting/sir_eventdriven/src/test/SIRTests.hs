module Main where

-- import Control.Parallel.Strategies hiding (r0)
import Data.Maybe
import System.Random

import Test.QuickCheck
--import Test.QuickCheck.Random

import SIR.SIR
import StatsUtils

--import Debug.Trace
import Text.Printf

instance Arbitrary SIRState where
  -- arbitrary :: Gen SIRState
  arbitrary = elements [Susceptible, Infected, Recovered]
  -- arbitrary = frequency [ (3, return Susceptible)
  --                       , (2, return Infected)
  --                       , (1, return Recovered) ]

paramContactRate :: Int
paramContactRate = 5

paramInfectivity :: Double
paramInfectivity = 0.05

paramIllnessDuration :: Double
paramIllnessDuration = 15.0

-- need to run replications because ABS is stochastic
replications :: Int
replications = 100

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 10000        -- number successful tests
                              , maxFailPercent = 100    -- number of maximum failed tests
                              , maxShrinks = 0          -- NO SHRINKS, they count towards successful tests, biasing the percentage
                              --, replay = Just (mkQCGen 42, 0) -- use to replay reproducible
                              } prop_sir_sd_random_size

prop_sir_sd_random_size :: [SIRState] -> Gen Property
prop_sir_sd_random_size as = do
    -- dont use vector as it will generate Int values quite close to each other
    -- without enough range => the probability of picking same values increases
    -- which does result in same dynamics, resultin in less variance.
    -- Therefore we use minBound and maxBound to go explicitly over the full
    -- Int range!
    seeds <- vectorOf replications (choose (minBound, maxBound))
    return $ property (prop_sir_sd_spec_aux as seeds)
    -- label (labelPopulation as) $ 

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

prop_sir_sd_fixed_size :: Gen Bool
prop_sir_sd_fixed_size = do
    seeds <- vectorOf replications (choose (minBound, maxBound))
    as    <- vector 100
    return $ prop_sir_sd_spec_aux as seeds

prop_sir_sd_spec_aux :: [SIRState] -> [Int] -> Bool
prop_sir_sd_spec_aux as seeds = allPass
  -- = trace ( "---------------------------------------------------------------------------------------" ++
  --           "\n s0 = " ++ show s0 ++ ", \t i0 = " ++ show i0 ++ ", \t r0 = " ++ show r0 ++ ", \t n = " ++ show n ++
  --           "\n s  = " ++ printf "%.2f" s ++ ", \t i  = " ++ printf "%.2f" i ++ ", \t r  = " ++ printf "%.2f" r ++ 
  --           "\n ss = " ++ printf "%.2f" _ssMean ++ ", \t is = " ++ printf "%.2f" _isMean ++ ", \t rs = " ++ printf "%.2f" _rsMean) 
  --           allPass
  where
    s0 = fromIntegral $ length $ filter (==Susceptible) as
    i0 = fromIntegral $ length $ filter (==Infected) as
    r0 = fromIntegral $ length $ filter (==Recovered) as
    n  = s0 + i0 + r0

    beta  = fromIntegral paramContactRate
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
    -- generate random-number generator for each replication
    rngs = map mkStdGen seeds
    -- compute simulated values for s, i and r
    sir = map (tripleIntToDouble . 
              snd . 
              last . 
              runSIR as paramContactRate paramInfectivity paramIllnessDuration (-1) dur) rngs
    -- apply evaluation parallelism to speed up
    -- NOTE: doesn't add anything
    -- (ss, is, rs) = unzip3 $ withStrategy (parList rdeepseq) sir
    (ss, is, rs) = unzip3 sir

    _ssMean = mean ss
    _isMean = mean is
    _rsMean = mean rs
    
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
    
    tripleIntToDouble :: (Int, Int, Int) -> (Double, Double, Double)
    tripleIntToDouble (x,y,z) = (fromIntegral x, fromIntegral y, fromIntegral z)