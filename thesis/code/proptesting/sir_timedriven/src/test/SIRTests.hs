module Main where

-- import Control.Parallel.Strategies hiding (r0)
import Data.Maybe
import System.Random

import Test.QuickCheck
-- import Test.QuickCheck.Random

import SIR.SIR
import StatsUtils

--import Debug.Trace
--import Text.Printf

instance Arbitrary SIRState where
  -- arbitrary :: Gen SIRState
  arbitrary = elements [Susceptible, Infected, Recovered]
  -- arbitrary = frequency [ (3, return Susceptible)
  --                       , (2, return Infected)
  --                       , (1, return Recovered) ]

paramContactRate :: Double
paramContactRate = 5.0

paramInfectivity :: Double
paramInfectivity = 0.05

paramIllnessDuration :: Double
paramIllnessDuration = 15.0

-- need to run replications because ABS is stochastic
replications :: Int
replications = 100

type SirSim = (StdGen -> (Double, Double, Double))

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 100        -- number successful tests
                              , maxFailPercent = 100    -- number of maximum failed tests
                              , maxShrinks = 0          -- NO SHRINKS, they count towards successful tests, biasing the percentage
                              --, replay = Just (mkQCGen 42, 0) -- use to replay reproducible
                              } prop_sir_sd_spec_random_size -- prop_sir_sd_spec_random_sir

prop_sir_sd_spec_random_size :: [SIRState] -> Gen Bool
prop_sir_sd_spec_random_size as = do
    -- dont use vector as it will generate Int values quite close to each other
    -- without enough range => the probability of picking same values increases
    -- which does result in same dynamics, resultin in less variance.
    -- Therefore we use minBound and maxBound to go explicitly over the full
    -- Int range!
    seeds <- vectorOf replications (choose (minBound, maxBound))

    -- run for 1 time-unit
    let dur = 1.0
    -- dt
    let dt     = 0.01
        sirSim = tripleIntToDouble . last . runSIRFor dur dt as paramContactRate paramInfectivity paramIllnessDuration

    return $ prop_sir_sd_spec sirSim as seeds

prop_sir_sd_spec_random_sir ::  [SIRState] -> Gen Bool
prop_sir_sd_spec_random_sir as = do
    seeds <- vectorOf replications (choose (minBound, maxBound))
    
    let sirSim = _sirSimRandomCorrelated (length as)

    return $ prop_sir_sd_spec sirSim as seeds
  where
    _sirSimRandomCorrelated :: Int -> StdGen -> (Double, Double, Double)
    _sirSimRandomCorrelated n g0 = (fromIntegral s, fromIntegral i, fromIntegral r)
      where
        (s, g1) = randomR (0, n) g0
        (i, _)  = randomR (0, n - s) g1
        r       = n-s-i

    _sirSimRandomUncorr :: Int -> StdGen -> (Double, Double, Double)
    _sirSimRandomUncorr n g0 = (fromIntegral s, fromIntegral i, fromIntegral r)
      where
        (s, g1) = randomR (0, n) g0
        (i, g2) = randomR (0, n) g1
        (r, _)  = randomR (0, n) g2

prop_sir_sd_spec_fixed_size :: Gen Bool
prop_sir_sd_spec_fixed_size = do
    seeds <- vectorOf replications (choose (minBound, maxBound))
    as    <- vector 100

    -- run for 1 time-unit
    let dur = 1.0
    -- small dt: TODO: testing for a sufficiently small dt
    -- decreasing this value reduces t-test failures!
    let dt = 0.01

    let sirSim = tripleIntToDouble . last . runSIRFor dur dt as paramContactRate paramInfectivity paramIllnessDuration

    return $ prop_sir_sd_spec sirSim as seeds

prop_sir_sd_spec :: SirSim -> [SIRState] -> [Int] -> Bool
prop_sir_sd_spec sirSim as seeds = allPass
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

    -- generate random-number generator for each replication
    rngs = map mkStdGen seeds
    -- compute simulated values for s, i and r
    sir = map sirSim rngs
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
    confidence = 0.99
    sTest = tTestSamples TwoTail s (1 - confidence) ss
    iTest = tTestSamples TwoTail i (1 - confidence) is
    rTest = tTestSamples TwoTail r (1 - confidence) rs

    allPass = fromMaybe True sTest &&
              fromMaybe True iTest &&
              fromMaybe True rTest

tripleIntToDouble :: (Int, Int, Int) -> (Double, Double, Double)
tripleIntToDouble (x,y,z) = (fromIntegral x, fromIntegral y, fromIntegral z)

    -- NOTE: if we return True in all cases, the compiler infers that 
    -- this is a constant expression and evaluates it only once, thus we
    -- don't get a debug output in case of a failure. As a remedy, we
    -- use a proposition which is always True but cannot be inferred by
    -- the compiler 
    -- sTestPass
    --   | isNothing sTest = True
    --   | fromJust sTest  = True
    --   | otherwise       = trace ("susceptible t-test failed with ss = \n" ++ show ss ) (not $ s0 /= 0 && s == 0)

    -- iTestPass
    --   | isNothing iTest = True
    --   | fromJust iTest  = True
    --   | otherwise       = trace ("infected t-test failed with is = \n" ++ show is) (not $ s0 /= 0 && s == 0)

    -- rTestPass
    --   | isNothing rTest = True
    --   | fromJust rTest  = True
    --   | otherwise       = trace ("recovered t-test failed with rs = \n" ++ show rs) (not $ s0 /= 0 && s == 0)

    -- allPass True True True  = True 
    -- allPass True True False = False
    -- allPass True False True = False
    -- allPass True False False = False
    -- allPass False True True = False
    -- allPass False True False = False
    -- allPass False False True = False
    -- allPass False False False = False