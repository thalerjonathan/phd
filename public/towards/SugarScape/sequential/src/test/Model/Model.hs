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

prop_disease_dynamics_allrecover ::  RandomGen g => g -> IO ()
prop_disease_dynamics_allrecover g0 = 
    assertBool "Population should have recovered fully from diseases but still infected left" (infected == 0)
  where
    steps  = 100 
    params = mkParamsAnimationV_1

    (simState, _, _) = initSimulationRng g0 params
    (_, _, _, aos)   = simulateUntilLast steps simState  -- must not use simulateUntil because would store all steps => run out of memory if scenario is too complex e.g. chapter III onwards
    infected         = length $ filter (==False) $ map (null . sugObsDiseases . snd) aos
    
prop_disease_dynamics_minorityrecover ::  RandomGen g => g -> IO ()
prop_disease_dynamics_minorityrecover g0 = 
    assertBool "Population should have recovered fully from diseases but still infected left" (infected >= infectedMajority)
  where
    steps  = 1000
    params = mkParamsAnimationV_2

    (simState, _, _) = initSimulationRng g0 params
    (_, _, _, aos)   = simulateUntilLast steps simState  -- must not use simulateUntil because would store all steps => run out of memory if scenario is too complex e.g. chapter III onwards
    infected         = length $ filter (==False) $ map (null . sugObsDiseases . snd) aos
    n                = fromIntegral $ length aos :: Double
    infectedMajority = ceiling $ n / 2

prop_trading_dynamics ::  RandomGen g => g -> IO ()
prop_trading_dynamics g0 = do
    putStrLn $ "trading-prices std = " ++ show tradingPricesStd
    assertBool ("Trading Prices standard deviation too high, should be below 0.05, is " ++ show tradingPricesStd) tradingDynamicsPass
  where
    -- according to sugarscape around this time, trading-prices standard deviation is LTE 0.05
    steps                 = 1000 
    tradingPricesStdLimit = 0.05
    params   = mkParamsFigureIV_3

    (simState, _, _) = initSimulationRng g0 params
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

    tagLength        = fromJust $ spCulturalProcess params
    (simState, _, _) = initSimulationRng g0 params
    (_, _, _, aos)   = simulateUntilLast steps simState  -- must not use simulateUntil because would store all steps => run out of memory if scenario is too complex e.g. chapter III onwards
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

    (rngs, _)   = rngSplits runs g0
    sugParams   = mkParamsAnimationII_1

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
        (simState, _, _) = initSimulationRng g sugParams
        sos              = simulateUntil steps simState

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

    (rngs, _)   = rngSplits runs g0
    sugParams   = mkParamsAnimationII_2

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
        (simState, _, _) = initSimulationRng g sugParams
        sos              = simulateUntil steps simState
        sos'             = drop stableAfter sos
        popSizes         = map (\(_, _, _, aos) -> fromIntegral $ length aos) sos'
        popSizeVariance  = std popSizes
        popSizeMean      = mean popSizes
        popSizeMedian    = median popSizes

prop_wealth_dist :: RandomGen g => g -> IO ()
prop_wealth_dist g0 = assertBool ("Wealth Distribution average skewness less than " ++ show expSkew) $ pass tTestSkew && pass tTestKurt && pass tTestGini
  where
    runs      = 100
    steps     = 200

    expSkew   = 1.5 :: Double
    expKurt   = 2.0 :: Double
    expGini   = 0.48 :: Double

    (rngs, _) = rngSplits runs g0
    sugParams = mkParamsAnimationII_3

    ret       = parMap rpar (genPopulationWealthStats sugParams steps) rngs

    (sks, ks, gs) = unzip3 ret

    tTestSkew     = tTest "skewness wealth distr" sks expSkew 0.05
    tTestKurt     = tTest "kurtosis wealth distr" ks expKurt 0.05
    tTestGini     = tTest "gini wealth distr" gs expGini 0.05

genPopulationWealthStats :: RandomGen g 
                          => SugarScapeScenario
                          -> Int
                          -> g
                          -> (Double, Double, Double)
genPopulationWealthStats params steps g 
    = trace ("skewness = " ++ show skew ++ ", kurtosis = " ++ show kurt ++ " gini = " ++ show gini) 
        (skew, kurt, gini)
  where
    (simState, _, _) = initSimulationRng g params
    (_, _, _, aos)   = simulateUntilLast steps simState  -- must not use simulateUntil because would store all steps => run out of memory if scenario is too complex e.g. chapter III onwards
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

  
{-
  let xs = [219.9468438538206
            , 202.1627906976744
            , 205.74418604651163 
            , 198.2890365448505
            , 205.99003322259136
            , 205.57142857142858 
            , 203.1860465116279
            , 202.77408637873754
            , 200.62458471760797
            , 204.48504983388705
            , 208.156146179402
            , 196.58139534883722
            , 206.09634551495017
            , 196.9435215946844
            , 206.49169435215947
            , 204.1063122923588
            , 207.66445182724252
            , 216.734219269103
            , 214.82059800664453
            , 204.7375415282392
            , 203.64119601328903
            , 201.6013289036545
            , 192.10963455149502
            , 198.55149501661128
            , 202.58471760797343
            , 211.54817275747507
            , 196.3687707641196 
            , 205.51162790697674
            , 204.19601328903656
            , 216.35215946843854 
            , 203.41860465116278
            , 198.3953488372093
            , 208.91029900332225
            , 211.95016611295682
            , 214.42857142857142 
            , 190.80730897009965 
            , 200.3156146179402 
            , 195.27242524916943 
            , 203.29900332225913 
            , 201.3654485049834 
            , 207.34883720930233 
            , 195.68106312292358 
            , 213.33554817275748 
            , 194.78737541528238 
            , 200.1029900332226 
            , 213.14617940199335 
            , 219.531561461794 
            , 201.265780730897 
            , 213.32558139534885]

  print $ tTest "carrying caps" xs 200 0.05

  --print $ tTest "test1" [1..10] 5.5 0.05
  --print $ tTest "test2" [1..10] 0.0 0.05
-}

{-
  putStrLn ""
  x <- generate arbitrary :: IO (Discrete2d SugEnvCell)
  print x
-}