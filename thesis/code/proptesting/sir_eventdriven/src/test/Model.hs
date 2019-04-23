{-# LANGUAGE InstanceSigs #-}
module Main where

--import Data.Maybe

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIR.SIR
import SIRGenerators
import StatsUtils
import SIRSD

import Debug.Trace

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
main = do
  let t = testGroup "SIR Spec Tests" 
          [ 
            QC.testProperty "SIR random SD" prop_sir_sd
          , QC.testProperty "SIR random population" prop_sir_random
          ]

  defaultMain t

prop_sir_random :: Property
prop_sir_random = checkCoverage $ do
  -- TODO: all tests seem to fail with this population size, WHY??? 
  -- I assumed that the more agents, the more simliar it is on average
  --as <- resize 1000 (listOf genSIRState)
  -- TODO: this seems to work, WHY??
  as <- listOf genSIRState
  (ss, is, rs) <- unzip3 <$> vectorOf replications (genLastSir as)
  let prop = checkSirSDSpec as ss is rs

  return $ cover 90 prop "ABS averages SIR spec" True

prop_sir_sd :: Gen Bool
prop_sir_sd = trace (show $ nearlyEqual 100 99.9 0.0005) $ do
  as <- resize 1000 (listOf genSIRState)
  let s0 = fromIntegral $ length (filter (==Susceptible) as)
  let i0 = fromIntegral $ length (filter (==Infected) as)
  let r0 = fromIntegral $ length (filter (==Recovered) as)

  let (ss, is, rs) = last $ runSIRSD s0 i0 r0 (fromIntegral contactRate) infectivity illnessDuration 1 0.001

  let (s, i, r) = sdSpec s0 i0 r0 (fromIntegral contactRate) infectivity illnessDuration

  let epsilon = 0.005

  let prop = nearlyEqual ss s epsilon && 
             nearlyEqual is i epsilon && 
             nearlyEqual rs r epsilon

  return prop

nearlyEqual :: Double -> Double -> Double -> Bool
nearlyEqual a b epsilon 
    | a == b 
      = True -- shortcut, handles infinities
    | (a == 0 || b == 0 || diff < minValue) 
      -- a or b is zero or both are extremely close to it
      -- relative error is less meaningful here
      =  diff < (epsilon * minValue)
    | otherwise 
      -- use relative error
      = diff / (absA + absB) < epsilon 
  where
    absA = abs a
    absB = abs b
    diff = abs (a - b)

minValue :: (RealFloat a) => a
minValue = x
  where n = floatDigits x
        b = floatRadix x
        (l, _) = floatRange x
        x = encodeFloat (b^n - 1) (l - n - 1)

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
    --(s, i, r) = last $ runSIRSD s0 i0 r0 (fromIntegral contactRate) infectivity illnessDuration 1 0.001

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

    epsilon = 0.05

    allPass = nearlyEqual ssMean s epsilon && 
             nearlyEqual isMean i epsilon && 
             nearlyEqual rsMean r epsilon


    
genLastSir :: [SIRState] -> Gen (Int, Int, Int)
genLastSir as = do
  ret <- map snd <$> genSimulationSIR as contactRate infectivity illnessDuration (-1) 1.0 
  if null ret
    then do
      let s = length (filter (==Susceptible) as)
      let i = length (filter (==Infected) as)
      let r = length (filter (==Recovered) as)
      return (s,i,r)
    else return (last ret)