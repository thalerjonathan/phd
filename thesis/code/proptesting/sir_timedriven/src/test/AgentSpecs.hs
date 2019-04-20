module Main where

import Data.List
import Data.Maybe
import Text.Printf

import Control.Monad.Random
import FRP.Yampa
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIR.SIR
import StatsUtils

-- import Debug.Trace

-- clear & stack test sir-time:sir-agentspecs-test

main :: IO ()
main = defaultMain $ testGroup "SIR Agent Specifications Tests" 
          [ 
          --   QC.testProperty 
          --     "Susceptible agents invariants"
          --     prop_susceptible_invariants

           QC.testProperty 
               "Susceptible agent infected"
               prop_susceptible_infected

          -- , QC.testProperty 
          --    "Infected agent invariants"
          --    prop_infected_invariants

          -- , QC.testProperty 
          --    "Recovered agent invariants"
          --    prop_recovered_invariants
          
          -- , QC.testProperty 
          --    "Infected agent mean illness duration t-test"
          --    prop_infected_meanIllnessDuration_ttest
          ]

--------------------------------------------------------------------------------
-- PROPERTIES
--------------------------------------------------------------------------------
prop_susceptible_invariants :: Property
prop_susceptible_invariants = checkCoverage $ do  
    -- NOTE: these parameteres can be varied arbitrarily and the test will
    -- still work. Also note that we have to fix the population instead of taking
    -- a random one, otherwise percentage of coverage would change in every
    -- test-case, which would not make sense.
    let cor = 5    -- beta, contact rate
        inf = 0.05 -- gamma, infectivity
        ild = 1.0  -- delta, illness duration NOTE: doesn't matter in this test, set to 1
      
    -- generate population with size of up to 1000
    as <- resize 1000 (listOf genSIRState)

    let dt = 0.01
        t  = 1

    -- run a random susceptible agent for 1.0 time-unit and return its outputs
    aos <- genSusceptible cor inf ild as t dt

    return (susceptibleInvariants aos)
  where
    susceptibleInvariants :: [SIRState] -> Bool
    susceptibleInvariants aos 
        -- Susceptible became Infected and then Recovered
        | isJust recIdxMay 
          = infIdx < recIdx &&
            all (==Susceptible) (take infIdx aos) &&
            all (==Infected) (take (recIdx - infIdx) (drop infIdx aos)) &&
            all (==Recovered) (drop recIdx aos)

        -- Susceptible became Infected
        | isJust infIdxMay 
          = all (==Susceptible) (take infIdx aos) &&
            all (==Infected) (drop infIdx aos)

        -- Susceptible stayed Susceptible
        | otherwise = all (==Susceptible) aos
      where
        infIdxMay = elemIndex Infected aos
        recIdxMay = elemIndex Recovered aos

        infIdx = fromJust infIdxMay
        recIdx = fromJust recIdxMay

-- TODO: find formula for susceptible became infected
prop_susceptible_infected :: Property
prop_susceptible_infected = checkCoverage $ do  
    let cor = 5    -- beta, contact rate
        inf = 0.05 -- gamma, infectivity
        ild = 1.0  -- delta, illness duration NOTE: doesn't matter in this test, set to 1
      
    let as = [Susceptible, Infected, Infected, Recovered]

    let i = length (filter (==Infected) as)
        n = length as

    let dt = 0.01

    -- use CDF to get probability that up to contactRate contacts have occurred
    -- because making contact events are distributed with the exponential distribution
    let expContactProb = 100 * expCDF (1 / cor) cor
        infectedRatio  = if n == 0 then 0 else fromIntegral i / fromIntegral n

    -- compute expected probability
    let expProb  = expContactProb * inf * infectedRatio

    -- run a random susceptible agent for 1.0 time-unit and return true if infected
    aos <- genSusceptible cor inf ild as 1 dt

    -- expect given percentage of Susceptible agents to have become infected
    return $ cover expProb (Infected `elem` aos) 
           ("susceptible agents became infected, expected at least " 
             ++ printf "%.2f" expProb ++ "%") True

prop_infected_invariants :: Property
prop_infected_invariants = checkCoverage $ do
    let illnessDuration = 15.0 -- delta, illnes duration
    let prob = 100 * expCDF (1 / illnessDuration) illnessDuration
    let dt = 0.01
        t = 0 -- forever

    -- generate population with size of up to 1000
    as <- resize 1000 (listOf genSIRState)

    aos <- genInfected illnessDuration as t dt

    let dur = infectedInvariant aos dt

    return $ cover prob (fromJust dur <= illnessDuration)
              ("infected agents have an illness duration of  " ++ show illnessDuration ++
              " or less, expected " ++ printf "%.2f" prob) (isJust dur)
  where
    infectedInvariant :: [SIRState] -> Double -> Maybe Double
    infectedInvariant aos dt  = do
      -- an infected agent WILL recover after finite time. 
      -- we search in an infinite list...
      recIdx <- elemIndex Recovered aos

      if all (==Infected) (take recIdx aos) 
        then Just (dt * fromIntegral recIdx)
        else Nothing

prop_infected_meanIllnessDuration_ttest:: Property
prop_infected_meanIllnessDuration_ttest = checkCoverage $ do
    let illnessDuration = 15.0 -- delta
    -- run 100 random infected agents until they recover and return the 
    -- duration they were ill
    ids <- vectorOf 100 (genInfectedIllnessDuration illnessDuration)
    
    let confidence = 0.95
        idsTTest   = tTestSamples TwoTail illnessDuration (1 - confidence) ids

    return $ cover 95 (fromMaybe True idsTTest) 
             ("infected agents have a mean illness duration of " ++ show illnessDuration) True
  where
    genInfectedIllnessDuration :: Double -> Gen Double
    genInfectedIllnessDuration illnessDuration = do
      g  <- genStdGen
      ss <- listOf genSIRState

      let a   = infectedAgent illnessDuration g
          dt  = 0.5 
          dts = repeat (dt, Nothing)
          aos = embed a (ss, dts)
      
      -- we are searching in a potentially infinite list...
      let mrec = elemIndex Recovered aos
      -- ... t hus when elemIndex returns it will have Just
      let recIdx = fromJust mrec 

      return $ dt * fromIntegral recIdx

prop_recovered_invariants :: Property
prop_recovered_invariants = property $ do
  let dt = 0.01
      t  = 1000

  -- generate population with size of up to 1000
  as <- resize 1000 (listOf genSIRState)

  aos <- genRecovered as t dt
  return $ all (==Recovered) aos

--------------------------------------------------------------------------------
-- CUSTOM GENERATORS
--------------------------------------------------------------------------------
genStdGen :: Gen StdGen
genStdGen = do
  seed <- choose (minBound, maxBound)
  return $ mkStdGen seed

genSIRState :: Gen SIRState 
genSIRState = elements [Susceptible, Infected, Recovered]

genAgent :: (StdGen -> SIRAgent)
         -> [SIRState]
         -> Double
         -> Double
         -> Gen [SIRState]
genAgent a as tMax dt = do
  g <- genStdGen

  let ag  = a g
      n   = floor (tMax / dt)
      dts = if n == 0 then repeat (dt, Nothing) else replicate n (dt, Nothing)
      aos = embed ag (as, dts)

  return aos 

genSusceptible :: Double
               -> Double 
               -> Double
               -> [SIRState]
               -> Double
               -> Double
               -> Gen [SIRState]
genSusceptible cor inf ild as tMax dt = do
  let a = susceptibleAgent cor inf ild
  genAgent a as tMax dt

genInfected :: Double
            -> [SIRState]
            -> Double
            -> Double
            -> Gen [SIRState]
genInfected ild as tMax dt = do
  let a = infectedAgent ild
  genAgent a as tMax dt

genRecovered :: [SIRState]
             -> Double
             -> Double
             -> Gen [SIRState]
genRecovered as tMax dt = do
  let a = const recoveredAgent
  genAgent a as tMax dt