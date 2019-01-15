module Model.Model 
  ( prop_disease_dynamics_allrecover
  , prop_disease_dynamics_minorityrecover
  , prop_trading_dynamics
  , prop_culture_dynamics
  , prop_inheritance_gini
  , prop_terracing
  , prop_carrying_cap
  , prop_wealth_dist
  ) where

import Data.List
import Data.Maybe

import Control.Monad.Random
import Control.Parallel.Strategies
import Test.Tasty.HUnit as Unit
import Test.Tasty.QuickCheck as QC

import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Random
import SugarScape.Core.Scenario
import SugarScape.Core.Simulation
-- need this import for Arbitrary instance of SugEnvironment but Haskell thinks its unused => turn off unused import :(
-- import Agent
-- import Environment
import Utils.StatsUtils

import Debug.Trace

instance Arbitrary StdGen where
  -- arbitrary :: Gen StdGen
  arbitrary = do
    seed <- arbitrary 
    return $ mkStdGen seed

-- OK, is using averaging
prop_disease_dynamics_allrecover ::  RandomGen g => g -> Int -> Double IO ()
prop_disease_dynamics_allrecover g0 repls confidence = 
    assertBool "Population should have recovered fully from diseases but still infected left" (allRecoveredConfidence >= confidence)
  where
    ticks  = 100
    params = mkParamsAnimationV_1

    (rngs, _) = rngSplits repls g0
    allRecovered = parMap rpar genAllRecovered rngs

    allRecoveredConfidence = fromIntegral (length $ filter (==True) allRecovered) / fromIntegral repls

    genAllRecovered :: RandomGen g => g -> Bool
    genAllRecovered g = infected == 0
      where
        (simState, _, _) = initSimulationRng g params
        (_, _, _, aos)   = simulateUntilLast ticks simState  -- must not use simulateUntil because would store all steps => run out of memory if scenario is too complex e.g. chapter III onwards
        infected         = length $ filter (==False) $ map (null . sugObsDiseases . snd) aos
    
-- OK, is using averaging
prop_disease_dynamics_minorityrecover ::  RandomGen g => g -> Int -> Double -> IO ()
prop_disease_dynamics_minorityrecover g0 repls confidence = 
    assertBool "Population should have recovered fully from diseases but too many recovered" (infectedDominateConfidence >= confidence)
  where
    ticks  = 1000
    params = mkParamsAnimationV_2

    (rngs, _) = rngSplits repls g0
    infectedDominate = parMap rpar genInfectedDominate rngs

    infectedDominateConfidence = fromIntegral (length $ filter (==True) infectedDominate) / fromIntegral repls

    genInfectedDominate :: RandomGen g => g -> Bool
    genInfectedDominate g = infected >= infectedMajority
      where
        (simState, _, _) = initSimulationRng g params
        -- must not use simulateUntil because would store all steps => run out of memory if scenario is too complex e.g. chapter III onwards
        (_, _, _, aos)   = simulateUntilLast ticks simState  
        infected         = length $ filter (==False) $ map (null . sugObsDiseases . snd) aos
        n                = fromIntegral $ length aos :: Double
        infectedMajority = ceiling $ n / 2

-- OK, is using averaging
prop_culture_dynamics ::  RandomGen g => g -> Int -> Double -> IO ()
prop_culture_dynamics g0 repls confidence
    = assertBool "Cultures not converged sufficiently" (culturalDynamicsPassConfidence >= confidence)
  where
    ticks    = 2700 -- according to sugarscape around this time, culture-dynamics converge
    -- always a few agents in level 1 which dont move => not participating in culture dynamics and keep initial ones
    ratioDominate = 0.95 -- either one culture (red/blue) dominates completely on both hills ...
    ratioEqual    = 0.45 -- ... or each hill has a different culture
    params   = mkParamsAnimationIII_6

    (rngs, _) = rngSplits repls g0
    culturalDynamicsPass = parMap rpar genCulturalDynamicsPass rngs

    culturalDynamicsPassConfidence = fromIntegral (length $ filter (==True) culturalDynamicsPass) / fromIntegral repls

    genCulturalDynamicsPass :: RandomGen g => g -> Bool
    genCulturalDynamicsPass g = cultureDynamicsPass
      where
        tagLength        = fromJust $ spCulturalProcess params
        (simState, _, _) = initSimulationRng g params
        -- must not use simulateUntil because would store all steps => run out of memory if scenario is too complex e.g. chapter III onwards
        (_, _, _, aos)   = simulateUntilLast ticks simState  
        agentCultTags    = map (sugObsCultureTag . snd) aos

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

-- OK, uses a t-test
prop_inheritance_gini :: RandomGen g => g -> Int -> Double -> IO ()
prop_inheritance_gini g0 repls confidence
    = assertBool ("Gini Coefficient should be on average equals " ++ show expGini) (pass tTestRet)
  where
    ticks        = 1000
    expGini      = 0.35 :: Double -- conservative guess, is around 0.35, would need to take average of multiple runs but takes long time
    sugParams    = mkParamsFigureIII_7
    
    (rngs, _) = rngSplits repls g0
    priceStds = parMap rpar genGiniCoefficients rngs

    -- perform a 2-sided test because we expect it to be equal
    tTestRet = tTest "gini coefficient avg" priceStds expGini (1 - confidence) EQ

    genGiniCoefficients :: RandomGen g => g -> Double
    genGiniCoefficients g = gini
      where
        (_, _, gini) = genPopulationWealthStats sugParams ticks g

-- OK with t-test
prop_trading_dynamics :: RandomGen g => g -> Int -> Double -> IO ()
prop_trading_dynamics g0 repls confidence
    = assertBool ("Average Trading Prices std should be on average less than " ++ show maxTradingPricesStdAvg) (pass tTestRet)
  where
    -- according to sugarscape after 1000 ticks, trading-prices standard deviation is LTE 0.05
    ticks                  = 1000 
    maxTradingPricesStdAvg = 0.05
    params = mkParamsFigureIV_3

    (rngs, _) = rngSplits repls g0
    priceStds = parMap rpar genTradingStdPrices rngs

    -- perform a 1-sided test because if the trading-prices average is lower then we accept it as well
    tTestRet = tTest "trading prices avg" priceStds maxTradingPricesStdAvg (1 - confidence) LT

    genTradingStdPrices :: RandomGen g => g -> Double
    genTradingStdPrices g 
        = trace ("tradingPricesStd = " ++ show tradingPricesStd) tradingPricesStd
      where
        (simState, _, _) = initSimulationRng g params
         -- must not use simulateUntil because would store all ticks => run out of memory if scenario is too complex e.g. chapter III onwards
        (_, _, _, aos)   = simulateUntilLast ticks simState 
        trades           = concatMap (sugObsTrades . snd) aos
        tradingPricesStd = std $ map tradingPrice trades

        tradingPrice :: TradeInfo -> Double
        tradingPrice (TradeInfo price _ _ _ ) = price

-- OK: does a t-test
prop_terracing :: RandomGen g => g -> Int -> Double -> IO ()
prop_terracing g0 repls confidence
    = assertBool 
        ("Terracing proportions not within 95% confidence of " ++ show terrMean ++ " / " ++ show statMean)
        (pass terraceTest && pass staticTest)
  where
    ticks       = 100
    staticAfter = 50 :: Int

    maxTerraceRatio = 0.45 :: Double
    maxStaticRatio  = 0.99 :: Double

    (rngs, _)   = rngSplits repls g0
    sugParams   = mkParamsAnimationII_1

    ret         = parMap rpar genPopulationTerracingStats rngs

    (trs, srs)  = unzip ret
    terrMean    = mean trs
    statMean    = mean srs

    -- both tests are 1-sided because if the ratio is lower, then we accept it as well
    terraceTest = tTest "terracing" trs maxTerraceRatio (1 - confidence) LT
    staticTest  = tTest "statiic" srs maxStaticRatio (1 - confidence) LT

    genPopulationTerracingStats :: RandomGen g => g -> (Double, Double)
    genPopulationTerracingStats g 
        = trace ("terraceRatio = " ++ show terraceRatio ++ " terraceNumbers = " ++  show terraceNumbers ++ " staticRatio = " ++ show staticRatio ++ " staticNumbers = " ++ show staticNumbers) 
            (terraceRatio, staticRatio)
      where
        (simState, _, _) = initSimulationRng g sugParams
        sos              = simulateUntil ticks simState

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

-- OK, uses t-test
prop_carrying_cap :: RandomGen g => g -> Int -> Double -> IO ()
prop_carrying_cap g0 repls confidence
    = assertBool ("Carrying Capacity Mean not within 95% confidence of " ++ show expMean) $ pass tTestRet
  where
    ticks       = 400
    stableAfter = 100

    _maxVariance = 4 :: Double
    expMean      = 204 :: Double

    (rngs, _)   = rngSplits repls g0
    sugParams   = mkParamsAnimationII_2

    ret         = parMap rpar genPopulationSizeStats rngs

    (_vs, ms, _mds) = unzip3 ret
    -- we use a 2-sided t-test because we expecet it to be equal
    tTestRet = tTest "carrying cap" ms expMean (1 - confidence) EQ

    -- TODO: perform a 1-sided https://en.wikipedia.org/wiki/Chi-squared_test 
    -- on the variances to check if they are less then _maxVariance
    -- this would be an additional ensurance

    genPopulationSizeStats :: RandomGen g 
                           => g
                           -> (Double, Double, Double)
    genPopulationSizeStats g 
        = trace ("popSizeVariance = " ++ show popSizeVariance ++ " popSizeMean = " ++ show popSizeMean ++ " popSizeMedian = " ++ show popSizeMedian) 
            (popSizeVariance, popSizeMean, popSizeMedian)
      where
        (simState, _, _) = initSimulationRng g sugParams
        sos              = simulateUntil ticks simState
        sos'             = drop stableAfter sos
        popSizes         = map (\(_, _, _, aos) -> fromIntegral $ length aos) sos'
        popSizeVariance  = std popSizes
        popSizeMean      = mean popSizes
        popSizeMedian    = median popSizes

-- OK does a t-test
prop_wealth_dist :: RandomGen g => g -> Int -> Double -> IO ()
prop_wealth_dist g0 repls confidence
    = assertBool ("Wealth Distribution average skewness less than " ++ show expSkew) $ pass tTestSkew && pass tTestKurt && pass tTestGini
  where
    ticks     = 200

    expSkew   = 1.5 :: Double
    expKurt   = 2.0 :: Double
    expGini   = 0.48 :: Double

    (rngs, _) = rngSplits repls g0
    sugParams = mkParamsAnimationII_3

    ret       = parMap rpar (genPopulationWealthStats sugParams ticks) rngs

    (sks, ks, gs) = unzip3 ret

    -- all are 2-sided t-tests because has to be the expected mean
    tTestSkew     = tTest "skewness wealth distr" sks expSkew (1 - confidence) EQ
    tTestKurt     = tTest "kurtosis wealth distr" ks expKurt (1 - confidence) EQ
    tTestGini     = tTest "gini wealth distr" gs expGini (1 - confidence) EQ

genPopulationWealthStats :: RandomGen g 
                          => SugarScapeScenario
                          -> Int
                          -> g
                          -> (Double, Double, Double)
genPopulationWealthStats params ticks g 
    = trace ("skewness = " ++ show skew ++ ", kurtosis = " ++ show kurt ++ " gini = " ++ show gini) 
        (skew, kurt, gini)
  where
    (simState, _, _) = initSimulationRng g params
    -- must not use simulateUntil because would store all steps => run out of memory if scenario is too complex e.g. chapter III onwards
    (_, _, _, aos)   = simulateUntilLast ticks simState  
    agentWealths     = map (sugObsSugLvl . snd) aos

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