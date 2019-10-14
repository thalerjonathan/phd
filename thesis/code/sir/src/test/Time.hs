module Main where

import Data.List
import Data.Maybe
import Text.Printf

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIR.Time
import Utils.GenSIR
import Utils.GenTimeSIR
import Utils.Stats

-- import Debug.Trace

-- --quickcheck-replay=557780
-- --quickcheck-tests=1000
-- --quickcheck-verbose
-- --test-arguments=""
-- clear & stack test sir:sir-time-tests

main :: IO ()
main = defaultMain $ testGroup "SIR Agent Specifications Tests" 
          [ 
            QC.testProperty 
              "Susceptible agents invariants"
              prop_susceptible_invariants

          , QC.testProperty 
             "Infected agent invariants"
             prop_infected_invariants

          , QC.testProperty 
             "Recovered agent invariants"
             prop_recovered_invariants
          ]

--------------------------------------------------------------------------------
-- PROPERTIES
--------------------------------------------------------------------------------
prop_susceptible_invariants :: Positive Double  -- ^ beta, contact rate
                            -> Probability      -- ^ gamma, infectivity within (0,1) range
                            -> Positive Double  -- ^ delta, illness duration
                            -> TimeRange        -- ^ duration, within (0,50) range
                            -> [SIRState]       -- ^ population
                            -> Property
prop_susceptible_invariants
      (Positive cor) (P inf) (Positive ild) (T t) as = property $ do  
    -- population contains an infected agent True/False
    let infInPop = Infected `elem` as

    -- run a random susceptible agent for random time-units with 
    -- sampling rate dt 0.01 and return its stream of output
    aos <- genSusceptible cor inf ild as t 0.01

    return $
        -- label all test-cases
        label (labelTestCase aos) 
        -- check invariants on output stream
        (property $ susceptibleInvariants aos infInPop)
  where
    labelTestCase :: [SIRState] -> String
    labelTestCase aos
      | Recovered `elem` aos = "Susceptible -> Infected -> Recovered"
      | Infected `elem` aos  = "Susceptible -> Infected"
      | otherwise            = "Susceptible"

    susceptibleInvariants :: [SIRState] -> Bool -> Bool
    susceptibleInvariants aos infInPop
        -- Susceptible became Infected and then Recovered
        | isJust recIdxMay 
          = infIdx < recIdx &&
            all (==Susceptible) (take infIdx aos) &&
            all (==Infected) (take (recIdx - infIdx) (drop infIdx aos)) &&
            all (==Recovered) (drop recIdx aos) &&
            infInPop -- can only happen if there are infected in the population

        -- Susceptible became Infected
        | isJust infIdxMay 
          = all (==Susceptible) (take infIdx aos) &&
            all (==Infected) (drop infIdx aos) &&
            infInPop -- can only happen if there are infected in the population

        -- Susceptible stayed Susceptible
        | otherwise = all (==Susceptible) aos
      where
        infIdxMay = elemIndex Infected aos
        recIdxMay = elemIndex Recovered aos

        infIdx = fromJust infIdxMay
        recIdx = fromJust recIdxMay

-- NOTE: can't use random illness duration, otherwise coverage would not work
prop_infected_invariants :: [SIRState] -> Property
prop_infected_invariants as = checkCoverage $ do
     -- delta, illnes duration
    let illnessDuration = 15.0
    -- compute perc of agents which recover in less or equal 
    -- illnessDuration time-units. Follows the exponential distribution
    -- thus we use the CDF to compute the probability.
    let prob = 100 * expCDF (1 / illnessDuration) illnessDuration
    -- fixed sampling rate
    let dt = 0.01

    -- run a random infected agent without time-limit (0) and sampling rate
    -- of 0.01 and return its infinite output stream 
    aos <- genInfected illnessDuration as 0 dt

    -- compute the recovery time
    let dur = infectedInvariant aos dt

    return $ cover prob (fromJust dur <= illnessDuration)
              ("infected agents have an illness duration of " ++ show illnessDuration ++
              " or less, expected " ++ printf "%.2f" prob) (isJust dur)
  where
    infectedInvariant :: [SIRState] -> Double -> Maybe Double
    infectedInvariant aos dt  = do
      recIdx <- elemIndex Recovered aos

      if all (==Infected) (take recIdx aos) 
        then Just (dt * fromIntegral recIdx)
        else Nothing

prop_recovered_invariants :: TimeRange  -- ^ duration, within (0,50) range
                          -> [SIRState] -- ^ population
                          -> Property
prop_recovered_invariants (T t) as = property $ do
  aos <- genRecovered as t 0.01
  return $ all (==Recovered) aos

--------------------------------------------------------------------------------
-- OBSOLETE TESTS
--------------------------------------------------------------------------------
-- NOTE: this does not work because it follows a bi-modal probability
-- prop_susceptible_infected :: Property
-- prop_susceptible_infected = checkCoverage $ do  
--     let cor = 5    -- beta, contact rate
--         inf = 0.05 -- gamma, infectivity
--         ild = 1.0  -- delta, illness duration NOTE: doesn't matter in this test, set to 1
      
--     let as = [Susceptible, Infected, Recovered]

--     let i = length (filter (==Infected) as)
--         n = length as

--     let dt = 0.01

--     -- use CDF to get probability that up to contactRate contacts have occurred
--     -- because making contact events are distributed with the exponential distribution
--     let expContactProb = 100 * expCDF (1 / cor) cor
--         infectedRatio  = if n == 0 then 0 else fromIntegral i / fromIntegral n

--     -- compute expected probability
--     let expProb  = expContactProb * inf * infectedRatio

--     -- run a random susceptible agent for 1.0 time-unit and return true if infected
--     aos <- genSusceptible cor inf ild as 1 dt

--     -- expect given percentage of Susceptible agents to have become infected
--     return $ cover expProb (Infected `elem` aos) 
--            ("susceptible agents became infected, expected at least " 
--              ++ printf "%.2f" expProb ++ "%") True

-- NOTE: this is covered already by infected invariant 
-- prop_infected_meanIllnessDuration_ttest:: Property
-- prop_infected_meanIllnessDuration_ttest = checkCoverage $ do
--     let illnessDuration = 15.0 -- delta
--     -- run 100 random infected agents until they recover and return the 
--     -- duration they were ill
--     ids <- vectorOf 100 (genInfectedIllnessDuration illnessDuration)
    
--     let confidence = 0.95
--         idsTTest   = tTestSamples TwoTail illnessDuration (1 - confidence) ids

--     return $ cover 95 (fromMaybe True idsTTest) 
--              ("infected agents have a mean illness duration of " ++ show illnessDuration) True
--   where
--     genInfectedIllnessDuration :: Double -> Gen Double
--     genInfectedIllnessDuration illnessDuration = do
--       g  <- genStdGen
--       ss <- listOf genSIRState

--       let a   = infectedAgent illnessDuration g
--           dt  = 0.5 
--           dts = repeat (dt, Nothing)
--           aos = embed a (ss, dts)
      
--       -- we are searching in a potentially infinite list...
--       let mrec = elemIndex Recovered aos
--       -- ... t hus when elemIndex returns it will have Just
--       let recIdx = fromJust mrec 

--       return $ dt * fromIntegral recIdx
