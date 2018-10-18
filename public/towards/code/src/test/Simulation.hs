module Simulation 
  ( simTests
  ) where

import Control.Monad.Random
import Control.Parallel.Strategies
import Data.List
import Data.Maybe

import Test.Tasty
import Test.Tasty.HUnit      as Unit
import Test.Tasty.QuickCheck as QC

import SugarScape.Agent
import SugarScape.AgentMonad
import SugarScape.Discrete
import SugarScape.Environment
import SugarScape.Init
import SugarScape.Model
import SugarScape.Random
import SugarScape.Simulation

-- need this import for Arbitrary instance of SugEnvironment but Haskell thinks its unused => turn off unused import :(
import Agent
import Environment
import StatsUtils

import Debug.Trace

simTests :: RandomGen g 
         => g
         -> TestTree 
simTests g = testGroup 
              "Simulation Tests" 
              [ --Unit.testCase "Terracing" $ prop_terracing g,
              -- Unit.testCase "Carrying Capacity" $ prop_carrying_cap g ,
               Unit.testCase "Wealth Distribution" $ prop_wealth_dist g ]

prop_terracing :: RandomGen g => g -> IO ()
prop_terracing g0 = assertBool 
                      ("Terracing proportions not within 95% confidence of " ++ show terrMean ++ " / " ++ show statMean)
                      (pass terraceTest && pass staticTest)
  where
    runs        = 100
    steps       = 100
    staticAfter = 50 :: Int

    expTerraceRatio = 0.45 :: Double
    expStaticRatio  = 0.99 :: Double

    (rngs, _)   = rngSplits g0 runs
    sugParams   = mkParamsTerracing

    ret         = parMap rpar genPopulationTerracingStats rngs

    (trs, srs)  = unzip ret
    terrMean    = mean trs
    statMean    = mean srs

    terraceTest = tTest "terracing" trs expTerraceRatio (2 * 0.05) -- need a 1-sided t-test
    staticTest  = tTest "statiic" srs expStaticRatio (2 * 0.05) -- TODO: need a 1-sided t-test

    genPopulationTerracingStats :: RandomGen g 
                                => g
                                -> (Double, Double)
    genPopulationTerracingStats g 
        = trace ("terraceRatio = " ++ show terraceRatio ++ " terraceNumbers = " ++  show terraceNumbers ++ " staticRatio = " ++ show staticRatio ++ " staticNumbers = " ++ show staticNumbers) 
            (terraceRatio, staticRatio)
      where
        (simState, _) = initSimulationRng g sugParams
        sos           = simulateUntil steps simState

        ((_, _, aosStable) : sos') = drop staticAfter sos
        (_, envFinal, aosFinal) = last sos

        terraceNumbers = length $ filter (onTheEdge envFinal) aosFinal
        staticNumbers  = sum $ map (\(_, _, aos) -> fromIntegral (length $ filter (sameCoord aosStable) aos) / fromIntegral (length aos)) sos'
        
        terraceRatio  = fromIntegral terraceNumbers / fromIntegral (length aosFinal)
        staticRatio   = staticNumbers / fromIntegral (length sos')

        -- note ao is always in aos
        sameCoord :: [AgentObservable SugAgentObservable]
                  -> AgentObservable SugAgentObservable
                  -> Bool
        sameCoord aos (aid, ao) = sugObsCoord ao == sugObsCoord ao'
          where
            -- we can safely assume that we will always find the agent
            -- because if ao is in the final set it has to be in the stable set
            (_, ao') = fromJust $ find (\(aid', _) -> aid' == aid) aos 

        onTheEdge :: SugEnvironment
                  -> AgentObservable SugAgentObservable
                  -> Bool
        onTheEdge env (_, ao) = not sameLvls
          where
            coord       = sugObsCoord ao
            selfCell    = cellAt coord env
            selfCellLvl = sugEnvSugarLevel selfCell
            cells       = neighbourCells coord False env
            sameLvls    = any (\c -> sugEnvSugarLevel c /= selfCellLvl) cells

prop_carrying_cap :: RandomGen g => g -> IO ()
prop_carrying_cap g0 = assertBool ("Carrying Capacity Mean not within 95% confidence of " ++ show expMean) $ pass tTestRet
  where
    runs        = 100
    steps       = 400
    stableAfter = 100

    _maxVariance = 4 :: Double
    expMean     = 204 :: Double

    (rngs, _)   = rngSplits g0 runs
    sugParams   = mkParamsCarryingCapacity

    ret         = parMap rpar genPopulationSizeStats rngs

    (_vs, ms, _mds) = unzip3 ret
    tTestRet    = tTest "carrying cap" ms expMean 0.05

    -- TODO: perform a 1-sided https://en.wikipedia.org/wiki/Chi-squared_test on the variances to check if they are less then _maxVariance

    genPopulationSizeStats :: RandomGen g 
                           => g
                           -> (Double, Double, Double)
    genPopulationSizeStats g 
        = trace ("popSizeVariance = " ++ show popSizeVariance ++ " popSizeMean = " ++ show popSizeMean ++ " popSizeMedian = " ++ show popSizeMedian) 
            (popSizeVariance, popSizeMean, popSizeMedian)
      where
        (simState, _)   = initSimulationRng g sugParams
        sos             = simulateUntil steps simState
        sos'            = drop stableAfter sos
        popSizes        = map (\(_, _, aos) -> fromIntegral $ length aos) sos'
        popSizeVariance = std popSizes
        popSizeMean     = mean popSizes
        popSizeMedian   = median popSizes

pass :: Maybe Bool -> Bool
pass = fromMaybe False

prop_wealth_dist :: RandomGen g => g -> IO ()
prop_wealth_dist g0 = assertBool ("Wealth Distribution average skewness less than " ++ show expSkew) $ pass tTestSkew && pass tTestKurt && pass tTestGini
  where
    runs      = 100
    steps     = 200

    expSkew   = 1.5 :: Double
    expKurt   = 2.0 :: Double
    expGini   = 0.48 :: Double

    (rngs, _) = rngSplits g0 runs
    sugParams = mkParamsWealthDistr

    ret       = parMap rpar genPopulationWealthStats rngs

    (sks, ks, gs) = unzip3 ret

    tTestSkew     = tTest "skewness wealth distr" sks expSkew 0.05
    tTestKurt     = tTest "kurtosis wealth distr" ks expKurt 0.05
    tTestGini     = tTest "gini wealth distr" gs expGini 0.05

    genPopulationWealthStats :: RandomGen g 
                             => g
                             -> (Double, Double, Double)
    genPopulationWealthStats g 
        = trace ("skewness = " ++ show skew ++ ", kurtosis = " ++ show kurt ++ " gini = " ++ show gini) 
            (skew, kurt, gini)
      where
        (simState, _)    = initSimulationRng g sugParams
        sos              = simulateUntil steps simState
        (_, _, finalAos) = last sos
        agentWealths     = map (fromIntegral . sugObsSugLvl . snd) finalAos

        skew = skewness agentWealths
        kurt = kurtosis agentWealths
        gini = giniCoeff agentWealths

giniCoeff :: [Double]
          -> Double
giniCoeff xs = numer / denom
  where
    n = fromIntegral $ length xs
    
    numer = sum [abs (x_i - x_j) | x_i <- xs, x_j <- xs] 
    denom = 2 * n * sum xs