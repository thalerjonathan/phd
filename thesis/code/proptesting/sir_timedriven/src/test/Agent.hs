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

-- clear & stack test sir:sir-agent-test

main :: IO ()
main = defaultMain $ testGroup "Agent Tests" 
          [ 
            QC.testProperty 
              "Susceptible agent mean infected"
              prop_susceptible_meaninfected_ttest

          , QC.testProperty 
              "Infected agent mean illness duration"
              prop_infected_meanIllnessDuration

          , QC.testProperty 
              "Susceptible agent mean infected ttest"
              prop_susceptible_meaninfected_ttest
          ]

--------------------------------------------------------------------------------
-- PROPERTIES
--------------------------------------------------------------------------------
-- NOTE: this needs a ridiculous amount of time due to very high number of
-- replications needed...
prop_susceptible_meaninfected :: Property
prop_susceptible_meaninfected = checkCoverage $ do
  let contactRate     = 5
      infectivity     = 0.05
      illnessDuration = 15.0
    -- NOTE: no random population but fix population to Infected only, because
    -- need a fixed percentage for cover
  let pop = [Infected]
  -- compute expected percentage becoming infected: is always the same 
  -- across all test-cases, otherwise would not make sense
  let expPercInf = contactRate * infectivity

  i <- genSusceptibleInfected contactRate infectivity illnessDuration pop

  -- expect given percentage of i to be True
  return $ cover expPercInf i "mean infectivity" True

-- NOTE: this needs a ridiculous amount of time due to very high number of
-- replications needed...
prop_susceptible_meaninfected_ttest :: Property
prop_susceptible_meaninfected_ttest = checkCoverage $ do
    let repls           = 1000
    let contactRate     = 5
    let infectivity     = 0.05
    let illnessDuration = 15.0

    -- pop <- listOf genSIRState
    let pop  = [Infected]
    let infs = length (filter (==Infected) pop) 
    -- NOTE: we cannot check the infectivity parameter only because it is
    -- tied to the contactRate, therefore we can only check the combined
    -- value. If we also use a random population then we need to divide
    -- by the number of infected agents because in each contact event generated
    -- by contactRate one agent is picked randomly uniformly from the population
    let expected = if infs == 0 then 0 else (contactRate * infectivity) / fromIntegral infs

    irs <- vectorOf repls (genSusceptibleInfectedRatio contactRate infectivity illnessDuration pop repls)

    let confidence = 0.95
        idsTTest   = tTestSamples TwoTail expected (1 - confidence) irs

    return $ label (printf "epexted %.2f, was %.2f" expected (mean irs)) $ 
             cover 95 (fromMaybe True idsTTest) "mean infectivity" True
  where
    genSusceptibleInfectedRatio :: Double
                                -> Double 
                                -> Double
                                -> [SIRState]
                                -> Int
                                -> Gen Double
    genSusceptibleInfectedRatio contactRate infectivity illnessDuration pop repls = do
      is <- vectorOf repls (genSusceptibleInfected contactRate infectivity illnessDuration pop)
      let ir = fromIntegral (length (filter (==True) is)) / fromIntegral repls
      return ir

prop_infected_meanIllnessDuration :: Property
prop_infected_meanIllnessDuration = checkCoverage $ do
    let illnessDuration = 15.0 -- select a large enough value e.g. 15

    ids <- vectorOf 100 (genInfectedIllnessDuration illnessDuration)
    
    let confidence = 0.95
        idsTTest   = tTestSamples TwoTail illnessDuration (1 - confidence) ids

    return $ cover 95 (fromMaybe True idsTTest) ("mean illness duration of " ++ show illnessDuration) True
  where
    genInfectedIllnessDuration :: Double -> Gen Double
    genInfectedIllnessDuration illnessDuration = do
      g  <- genStdGen
      ss <- listOf genSIRState

      let a   = infectedAgent illnessDuration g
          dt  = 0.5 -- NOTE: can sample with considerably large dt because if illnessDuration is large
          dts = repeat (dt, Nothing)
          aos = embed a (ss, dts)
      
      -- we are searching in a potentially infinite list...
      let mrec = elemIndex Recovered aos
      -- ... thus when elemIndex returns it will have Just
      let recIdx = fromJust mrec 

      return $ dt * fromIntegral recIdx

--------------------------------------------------------------------------------
-- CUSTOM GENERATORS
--------------------------------------------------------------------------------
genStdGen :: Gen StdGen
genStdGen = do
  seed <- choose (minBound, maxBound)
  return $ mkStdGen seed

genSIRState :: Gen SIRState 
genSIRState = elements [Susceptible, Infected, Recovered]

genSusceptibleInfected :: Double
                       -> Double 
                       -> Double
                       -> [SIRState]
                       -> Gen Bool
genSusceptibleInfected contactRate infectivity illnessDuration pop = do
  g <- genStdGen

  let a   = susceptibleAgent contactRate infectivity illnessDuration g
      dt  = 0.01 -- need very small dt of 0.01 due to sampling issues
      n   = floor (1 / dt)
      dts = replicate n (dt, Nothing)
      aos = embed a (pop, dts)

  return $ Infected `elem` aos