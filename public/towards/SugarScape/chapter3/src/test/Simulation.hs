module Simulation 
  ( simTests
  ) where

import Data.List
import Data.Maybe

import Control.Monad.Random
import Control.Parallel.Strategies
import Test.Tasty
import Test.Tasty.HUnit      as Unit

import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Random
import SugarScape.Simulation
-- need this import for Arbitrary instance of SugEnvironment but Haskell thinks its unused => turn off unused import :(
-- import Agent
-- import Environment
import StatsUtils

import Debug.Trace

simTests :: RandomGen g 
         => g
         -> TestTree 
simTests g = testGroup 
              "Simulation Tests" 
              [ Unit.testCase "Trading Dynamics" $ prop_trading_dynamics g
              -- Unit.testCase "Cultural Dynamics" $ prop_culture_dynamics g 
              -- Unit.testCase "Inheritance Gini" $ prop_inheritance_gini g
              -- Unit.testCase "Terracing" $ prop_terracing g,
              -- Unit.testCase "Carrying Capacity" $ prop_carrying_cap g ,
              -- Unit.testCase "Wealth Distribution" $ prop_wealth_dist g 
              ]

prop_trading_dynamics ::  RandomGen g => g -> IO ()
prop_trading_dynamics g0 = do
    putStrLn $ "trading-prices std = " ++ show tradingPricesStd
    assertBool ("Trading Prices standard deviation too high, should be below 0.05, is " ++ show tradingPricesStd) tradingDynamicsPass
  where
    -- according to sugarscape around this time, trading-prices standard deviation is LTE 0.05
    steps                 = 1000 
    tradingPricesStdLimit = 0.05
    params   = mkParamsFigureIV_3

    (simState, _)    = initSimulationRng g0 params
    (_, _, _, aos)   = simulateUntilLast steps simState  -- must not use simulateUntil because would store all steps => run out of memory if scenario is too complex e.g. chapter III onwards
    trades           = concatMap (sugObsTrades . snd) aos
    tradingPricesStd = std $ map tradingPrice trades

    tradingDynamicsPass = tradingPricesStd <= tradingPricesStdLimit
    
    tradingPrice :: TradeInfo -> Double
    tradingPrice (TradeInfo price _ _ _ ) = price
    
prop_culture_dynamics ::  RandomGen g => g -> IO ()
prop_culture_dynamics g0 = do
    putStrLn $ "zeros-ratio = " ++ show zeroRatio
    assertBool ("Cultures not converged sufficiently, zeros-ratio = " ++ show zeroRatio) cultureDynamicsPass
  where
    steps    = 2700 -- according to sugarscape around this time, culture-dynamics converge
    -- always a few agents in level 1 which dont move => not participating in culture dynamics and keep initial ones
    ratioDominate = 0.95 -- either one culture (red/blue) dominates completely on both hills ...
    ratioEqual    = 0.45 -- ... or each hill has a different culture
    params   = mkParamsAnimationIII_6

    tagLength      = fromJust $ spCulturalProcess params
    (simState, _)  = initSimulationRng g0 params
    (_, _, _, aos) = simulateUntilLast steps simState  -- must not use simulateUntil because would store all steps => run out of memory if scenario is too complex e.g. chapter III onwards
    agentCultTags  = map (sugObsCultureTag . snd) aos

    zeroRatio      = zeroCultureRatio agentCultTags
    -- we don't care who is dominating: zeros or ones, it just has to come to a (near) equilibrium
    cultureDynamicsPass = if zeroRatio >= ratioDominate 
                            then True -- zeros dominate
                            else if zeroRatio <= (1 - ratioDominate)
                              then True -- ones dominate
                              else if zeroRatio >= ratioEqual || (1 - zeroRatio) <= ratioEqual
                                then True -- both dominate equally, each on one hill
                                else False

    zeroCultureRatio :: [CultureTag] -> Double
    zeroCultureRatio tags = fromIntegral zeros / fromIntegral n
      where
        zeros = length $ filter zeroDominate tags
        n     = length tags

        zeroDominate :: CultureTag -> Bool
        zeroDominate tag = zeroCount > (tagLength - zeroCount)
          where
            zeroCount = length $ filter (==False) tag

prop_inheritance_gini :: RandomGen g => g -> IO ()
prop_inheritance_gini g0 = assertBool ("Gini Coefficient less than " ++ show expGini) $ gini >= expGini
  where
    steps        = 1000
    expGini      = 0.30 :: Double -- conservative guess, is around 0.35, would need to take average of multiple runs but takes long time
    sugParams    = mkParamsFigureIII_7
    (_, _, gini) = genPopulationWealthStats sugParams steps g0

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

        ((_, _, _, aosStable) : sos') = drop staticAfter sos
        (_, _, envFinal, aosFinal)    = last sos

        terraceNumbers = length $ filter (onTheEdge envFinal) aosFinal
        staticNumbers  = sum $ map (\(_, _, _, aos) -> fromIntegral (length $ filter (sameCoord aosStable) aos) / fromIntegral (length aos)) sos'
        
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
            selfCellLvl = sugEnvSiteSugarLevel selfCell
            cells       = neighbourCells coord False env
            sameLvls    = any (\c -> sugEnvSiteSugarLevel c /= selfCellLvl) cells

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
        popSizes        = map (\(_, _, _, aos) -> fromIntegral $ length aos) sos'
        popSizeVariance = std popSizes
        popSizeMean     = mean popSizes
        popSizeMedian   = median popSizes

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

    ret       = parMap rpar (genPopulationWealthStats sugParams steps) rngs

    (sks, ks, gs) = unzip3 ret

    tTestSkew     = tTest "skewness wealth distr" sks expSkew 0.05
    tTestKurt     = tTest "kurtosis wealth distr" ks expKurt 0.05
    tTestGini     = tTest "gini wealth distr" gs expGini 0.05

genPopulationWealthStats :: RandomGen g 
                          => SugarScapeParams
                          -> Int
                          -> g
                          -> (Double, Double, Double)
genPopulationWealthStats params steps g 
    = trace ("skewness = " ++ show skew ++ ", kurtosis = " ++ show kurt ++ " gini = " ++ show gini) 
        (skew, kurt, gini)
  where
    (simState, _)  = initSimulationRng g params
    (_, _, _, aos) = simulateUntilLast steps simState  -- must not use simulateUntil because would store all steps => run out of memory if scenario is too complex e.g. chapter III onwards
    agentWealths   = map (sugObsSugLvl . snd) aos

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

pass :: Maybe Bool -> Bool
pass = fromMaybe False