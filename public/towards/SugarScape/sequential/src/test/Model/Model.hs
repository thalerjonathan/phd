module Model.Model 
  ( prop_disease_allrecover
  , prop_disease_norecover
  , prop_trading
  , prop_culture
  , prop_gini
  , prop_terracing
  , prop_carrying
  , prop_wealth
  ) where

import Data.List
import Data.Maybe
import Text.Printf

import Control.Monad.Random
import Test.QuickCheck

import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import SugarScape.Core.Simulation
import Utils.StatsUtils

import Debug.Trace

--------------------------------------------------------------------------------
-- PROPERTIES
--------------------------------------------------------------------------------
-- Tests the hypothesis, that for the parameter-configuration of AnimationV-1
-- ALL agents will recover after 100 ticks.
prop_disease_allrecover :: Property
prop_disease_allrecover = property $ do
  let ticks  = 100
      params = mkParamsAnimationV_1

  (_, _, _, aos) <- sugarscapeLast ticks params
  let infected = length $ filter (==False) $ map (null . sugObsDiseases . snd) aos
  return $ infected == 0
  
-- Tests the hypothesis, that for the parameter-configuration of AnimationV-2
-- less than half (a minority) will recover until 1000.
prop_disease_norecover :: Property
prop_disease_norecover = property $ do
    let ticks  = 1000
        params = mkParamsAnimationV_2
    (_, _, _, aos) <- sugarscapeLast ticks params

    let infected = length $ filter (==False) $ map (null . sugObsDiseases . snd) aos
        n        = fromIntegral $ length aos :: Double
        infMaj   = ceiling $ n / 2 -- majority is more than 50%

    return $ infected > infMaj

-- Testing the hypothesis, that when using the parameter-configuration of
-- FigureIV-3, after 1000 ticks, the standard deviation of the trading prices
-- is less or equal 0.05.
prop_trading :: Property
prop_trading = property $ do
    -- according to sugarscape after 1000 ticks, trading-prices standard 
    -- deviation is LTE 0.05
    let ticks  = 1000 
        params = mkParamsFigureIV_3

    out <- sugarscapeLast ticks params
      
    let maxTradingPricesStdAvg = 0.05
        prices    = tradingPrices out
        pricesStd = std prices

    return $ pricesStd <= maxTradingPricesStdAvg
  where
    tradingPrices :: SimStepOut -> [Double]
    tradingPrices (_, _, _, aos) = map tradingPrice trades 
      where
        trades = concatMap (sugObsTrades . snd) aos
 
        tradingPrice :: TradeInfo -> Double
        tradingPrice (TradeInfo price _ _ _ ) = price

-- Tests the hypothesis that for parameter configuration AnimationIII-6, after
-- 2700 either one culture dominates the other by 95% of all agents being of 
-- the respective culture, or both are roughly equivalent 45%.
-- There are always a few agents in level 1 which dont move and dos not participate 
-- in culture dynamics and keep initial ones, thus use 95 instead of 100 and
-- 45 instead of 50.
prop_culture :: Property
prop_culture = property $ do
    let ticks         = 2700 -- according to sugarscape book around this time, culture-dynamics converge
        ratioDominate = 0.95 -- either one culture (red/blue) dominates completely on both hills ...
        ratioEqual    = 0.45 -- ... or each hill has a different culture
        params        = mkParamsAnimationIII_6
        tagLength     = fromJust $ spCulturalProcess params

    (_, _, _, aos) <- sugarscapeLast ticks params

    let agentCultTags = map (sugObsCultureTag . snd) aos
        zeroRatio     = zeroCultureRatio tagLength agentCultTags

    if zeroRatio >= ratioDominate || zeroRatio <= (1 - ratioDominate)
      then return True -- zeros OR ones dominate
      else if zeroRatio >= ratioEqual || (1 - zeroRatio) <= ratioEqual 
        then return True -- both dominate equally, each on one hill
        else return False -- none dominates, none is equally dominant
  where
    zeroCultureRatio :: Int -> [CultureTag] -> Double
    zeroCultureRatio tagLength tags = fromIntegral zeros / fromIntegral n
      where
        zeros = length $ filter zeroDominate tags
        n     = length tags

        zeroDominate :: CultureTag -> Bool
        zeroDominate tag = zeroCount > (tagLength - zeroCount)
          where
            zeroCount = length $ filter (==False) tag

-- Testing the hypothesis, that when using the parameter-configuration of
-- Fiture III-7, where agents reproduce and can die of age then inheritance 
-- of their wealth leads to an unequal wealth distribution.
-- Gini of 0.35 is a onservative guess, is around 0.35, would need to take 
-- average of multiple runs but takes long time
prop_gini :: Int -> Double -> Property
prop_gini repls confidence = property $ do
  let ticks   = 1000
      expGini = 0.35 :: Double 
      params  = mkParamsFigureIII_7
  
  (_, _, gini) <- unzip3 <$> vectorOf repls (genPopulationWealthStats ticks params)

  -- perform a two-tailed test because we expect it to be equal
  let tTestRet = tTestSamples TwoTail expGini (1 - confidence) gini

  return $ fromMaybe True tTestRet

-- When agents don't mate nor can die from age (chapter II), due to the 
-- environment, there is a maximum carrying capacity of agents the environment
-- can sustain. The capacity should be reached after 100 ticks and should be 
-- stable from then on.
prop_carrying :: Int -> Double -> Property
prop_carrying repls confidence = property $ do
    let ticks        = 400
        stableAfter  = 100
        params       = mkParamsAnimationII_2
        _maxVariance = 4 :: Double
        expMean      = 204 :: Double
        
    (_vs, ms, _mds) <- unzip3 <$> vectorOf repls (genPopulationSizeStats ticks params stableAfter)

    -- we use a two-tailed t-test because we expecet it to be equal
    let tTestRet = tTestSamples TwoTail expMean (1 - confidence) ms

    -- TODO: perform a 1-sided https://en.wikipedia.org/wiki/Chi-squared_test 
    -- on the variances to check if they are less then _maxVariance
    -- this would be an additional ensurance

    return (fromMaybe True tTestRet)
  where
    genPopulationSizeStats :: Int
                           -> SugarScapeScenario
                           -> Int
                           -> Gen (Double, Double, Double)
    genPopulationSizeStats ticks params stableAfter = do
      sos <- sugarscapeUntil ticks params

      let sos'            = drop stableAfter sos
          popSizes        = map (\(_, _, _, aos) -> fromIntegral $ length aos) sos'
          popSizeVariance = std popSizes
          popSizeMean     = mean popSizes
          popSizeMedian   = median popSizes

      return $ trace ("popSizeVariance = " ++ printf "%.2f" popSizeVariance ++ 
                      " popSizeMean = "    ++ printf "%.2f" popSizeMean ++ 
                      " popSizeMedian = "  ++ printf "%.2f" popSizeMedian) 
          (popSizeVariance, popSizeMean, popSizeMedian)

-- Testing the hypothesis, that when using the parameter-configuration of
-- AnimationII-1, after 100 ticks, the terracing is table for 50 ticks
-- NOTE: 'once' because we are doing replications anyway
prop_terracing :: Int -> Double -> Property
prop_terracing repls confidence = property $ do 
    (trs, srs) <- unzip <$> vectorOf repls genPopulationTerracingStats

    let trMean = 0.45 :: Double  -- terracing ratio expcected mean
        srMean = 0.95 :: Double  -- static ratio expected mean

        trPass = tTestSamples TwoTail trMean (1 - confidence) trs
        srPass = tTestSamples TwoTail srMean (1 - confidence) srs

        allPass = fromMaybe True trPass &&
                  fromMaybe True srPass 

    return allPass

  where
    genPopulationTerracingStats :: Gen (Double, Double)
    genPopulationTerracingStats = do
        let ticks       = 100
            staticAfter = 50 :: Int
            params      = mkParamsAnimationII_1

        sos <- sugarscapeUntil ticks params

        let ((_, _, _, aosStable) : sos') = drop staticAfter sos
            (_, _, envFinal, aosFinal)    = last sos

            terraceNumbers = length $ filter (onTheEdge envFinal) aosFinal
            staticNumbers  = sum $ map (\(_, _, _, aos) -> fromIntegral (length $ filter (sameCoord aosStable) aos) / fromIntegral (length aos)) sos'
        
            tr = fromIntegral terraceNumbers / fromIntegral (length aosFinal)
            sr  = staticNumbers / fromIntegral (length sos')

        --return (tr, sr)
        return $ trace ("terraceRatio = "    ++ printf "%.2f" tr ++  
                        " terraceNumbers = " ++ show terraceNumbers ++ 
                        " staticRatio = "    ++ printf "%.2f" sr ++ 
                        " staticNumbers = "  ++ show staticNumbers) (tr, sr)
      where
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
            cs          = neighbourCells coord False env
            sameLvls    = any (\c -> sugEnvSiteSugarLevel c /= selfCellLvl) cs

-- TODO describe
prop_wealth :: Int -> Double -> Property
prop_wealth repls confidence = once $ do
  let ticks   = 200
      expSkew = 1.5 :: Double
      expKurt = 2.0 :: Double
      expGini = 0.48 :: Double
      params  = mkParamsAnimationII_3

  (sks, ks, gs) <- unzip3 <$> vectorOf repls (genPopulationWealthStats ticks params)

  -- all are two-tailed t-tests because has to be the expected mean
  let tTestSkew = tTestSamples TwoTail expSkew (1 - confidence) sks
      tTestKurt = tTestSamples TwoTail expKurt (1 - confidence) ks
      tTestGini = tTestSamples TwoTail expGini (1 - confidence) gs

  return $ fromMaybe True tTestSkew &&
           fromMaybe True tTestKurt &&
           fromMaybe True tTestGini

--------------------------------------------------------------------------------
-- GENERATORS & UTILITIES
-------------------------------------------------------------------------------- 
-- generates a random sugarscape with given scenario and runs it for given
-- number of ticks and returns the output of the LAST step
sugarscapeLast :: Int -> SugarScapeScenario -> Gen SimStepOut
sugarscapeLast ticks params = do
  seed <- choose (minBound, maxBound)
  let g                = mkStdGen seed
      (simState, _, _) = initSimulationRng g params
  return $ simulateUntilLast ticks simState  
    
-- generates a random sugarscape with given scenario and runs it for given
-- number of ticks and returns all output until some step
sugarscapeUntil :: Int -> SugarScapeScenario -> Gen [SimStepOut]
sugarscapeUntil ticks params = do
  seed <- choose (minBound, maxBound)
  let g                = mkStdGen seed
      (simState, _, _) = initSimulationRng g params
  return $ simulateUntil ticks simState

genPopulationWealthStats :: Int
                         -> SugarScapeScenario
                         -> Gen (Double, Double, Double)
genPopulationWealthStats ticks params = do
    (_, _, _, aos) <- sugarscapeLast ticks params

    let agentWealths = map (sugObsSugLvl . snd) aos
        skew         = skewness agentWealths
        kurt         = kurtosis agentWealths
        gini         = giniCoeff agentWealths

    return $ trace ("skewness = "   ++ printf "%.2f" skew ++ 
                    ", kurtosis = " ++ printf "%.2f" kurt ++ 
                    ", gini = "     ++ printf "%.2f" gini) 
            (skew, kurt, gini)

-- formula taken from https://en.wikipedia.org/wiki/Gini_coefficient#Definition
giniCoeff :: [Double] -> Double
giniCoeff xs = numer / denom
  where
    n     = fromIntegral $ length xs
    numer = sum [abs (x_i - x_j) | x_i <- xs, x_j <- xs] 
    denom = 2 * n * sum xs