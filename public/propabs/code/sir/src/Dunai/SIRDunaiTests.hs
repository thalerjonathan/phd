module SIRDunaiTests 
  ( sirDunaiPropTests
  , prop_dunai_sir
  ) where

import Control.Monad.Random
import Data.List
import Data.Maybe
import Data.Void
import FRP.Yampa
import Debug.Trace

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIR
import SIRSDDunai
import SIRDunai
import StatsUtils
import Utils

instance Arbitrary SIRState where
  -- arbitrary :: Gen SIRState
  arbitrary = elements [Susceptible, Infected, Recovered]

paramContactRate :: Double
paramContactRate = 5.0

paramInfectivity :: Double
paramInfectivity = 0.05

paramIllnessDuration :: Double
paramIllnessDuration = 15.0

sirDunaiPropTests :: RandomGen g
                  => g 
                  -> TestTree
sirDunaiPropTests g 
  = testGroup 
      "SIR Dunai Simulation Tests" 
      [ QC.testProperty "SIR sim behaviour" (forAll (listOf1 arbitrary) $ prop_dunai_sir g) ]

prop_dunai_sir :: RandomGen g 
               => g
               -> [SIRState]
               -> Bool
prop_dunai_sir g0 as 
    = trace ("\n  as: " ++ show as ++ 

             "\n, sus0 " ++ show sus0 ++ 
             "\n, inf0 " ++ show inf0 ++ 
             "\n, rec0 " ++ show rec0 ++

             "\n, infectionRateSim = " ++ show infectionRateSim ++ 
             "\n, recoveryRateSim = " ++ show recoveryRateSim ++ 

             "\n, susTarget " ++ show susTarget ++ 
             "\n, infTarget " ++ show infTarget ++ 
             "\n, recTarget " ++ show recTarget) passTTest
  where
    t  = 1.0
    dt = 0.01

    n = length as

    -- compute initial numbers the rates and expected values according to SD
    sus0 = (fromIntegral $ length $ filter (==Susceptible) as) :: Double
    inf0 = (fromIntegral $ length $ filter (==Infected) as) :: Double
    rec0 = (fromIntegral $ length $ filter (==Recovered) as) :: Double

    infectionRateSim = (inf0 * paramContactRate * sus0 * paramInfectivity) / fromIntegral n
    recoveryRateSim  = inf0 / paramIllnessDuration

    susTarget = sus0 - infectionRateSim
    infTarget = inf0 + (infectionRateSim - recoveryRateSim)
    recTarget = rec0 + recoveryRateSim
    
    -- run the ABS simulation 10.000 times (large number for statistical robustness)
    repls   = 10000
    (gs, _) = rngSplits g0 repls []
    sirData   = foldr (\g' acc -> runSIR g' : acc) ([] :: [[(Double, Double, Double)]]) gs

    -- do a 1-sided t-test
    passTTest = sirTTest sirData

    -- run SD for 1.0 time-unit to create same number of samples and compare if they are from the same distribution
    -- but in the end ABS will always show the variance in the behaviour than the average
    sdDyn = runDunaiSD sus0 inf0 rec0 paramContactRate paramInfectivity paramIllnessDuration t dt

    -- TODO: use Wilcoxon and Mann-Whiteny tests https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test, https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test
    -- to ceck if sdDyn and ABS are from same distribution, but need to return the number of agents over the whole run not just the last one

    -- TODO: export to file which plots SD against 10.000 ABS runs in Matlab/Octave

    sirTTest :: [[(Double, Double, Double)]] -> Bool
    sirTTest ass 
        = trace ( "\n, susAvg " ++ show susAvg ++ 
             "\n, infAvg " ++ show infAvg ++ 
             "\n, recAvg " ++ show recAvg ++

             --"\n, sus' " ++ show sus' ++ 
             --"\n, inf' " ++ show inf' ++ 
             --"\n, rec' " ++ show rec' ++

             "\n, susTTest " ++ show susTTest ++ 
             "\n, infTTest " ++ show infTTest ++ 
             "\n, recTTest " ++ show recTTest ++
             
             "\n, susProp " ++ show susProp ++ 
             "\n, infProp " ++ show infProp ++ 
             "\n, recProp " ++ show recProp) susProp && infProp && recProp
      where
        finalAggr          = map last ass
        (sus', inf', rec') = unzip3 finalAggr

        -- using a 95% confidence interval for 1-sided t-test
        -- TODO: seems to be too strict
        alpha = 0.1

        -- NOTE: t-test returns False when it is successful => invert it
        susTTest = Just not <*> tTest "sus" sus' susTarget alpha
        infTTest = Just not <*> tTest "inf" inf' infTarget alpha
        recTTest = Just not <*> tTest "rec" rec' recTarget alpha

        -- in case there is no variance (all samples same) we simply compare
        -- the averages within a given epsilon of a 90% interval
        eps = 0.1

        susAvg = mean sus'
        infAvg = mean inf'
        recAvg = mean rec'

        susProp = fromMaybe (avgTest sus' susTarget eps) susTTest
        infProp = fromMaybe (avgTest inf' infTarget eps) infTTest
        recProp = fromMaybe (avgTest rec' recTarget eps) recTTest

    runSIR :: RandomGen g 
           => g
           -> [(Double, Double, Double)]
    runSIR g = runSIRDunaiUntil g t dt as paramContactRate paramInfectivity paramIllnessDuration