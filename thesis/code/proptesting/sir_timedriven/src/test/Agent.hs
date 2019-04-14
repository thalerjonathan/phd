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
              "Susceptible agents become infected"
              prop_susceptible_infected

          , QC.testProperty 
             "Infected agent mean illness duration"
             prop_infected_meanIllnessDuration
          ]

--------------------------------------------------------------------------------
-- PROPERTIES
--------------------------------------------------------------------------------
prop_susceptible_infected :: Property
prop_susceptible_infected = checkCoverage $ do  
  -- NOTE: these parameteres can be varied arbitrarily and the test will
  -- still work. Also note that we have to fix the population instead of taking
  -- a random one, otherwise percentage of coverage would change in every
  -- test-case, which would not make sense.
  let contactRate     = 5
      infectivity     = 0.05
      illnessDuration = 1.0 -- NOTE: doesn't matter in this test, set to 1
      population      = [Susceptible, Infected, Recovered]  -- NOTE: also empty population and population with 0 infected works
      i               = length (filter (==Infected) population)
      n               = length population

  -- curiously a large dt works here...
  let dt = 1.0

  -- NOTE: the probability of agents to become infected follows the CDF of the
  -- exponential distribution due to the use of occasionally which follows
  -- the exponential distribution.
  let expContactProb = 100 * expCDF (1 / contactRate) contactRate
      infectedRatio  = if n == 0 then 0 else fromIntegral i / fromIntegral n

  -- compute expected probability
  let expProb  = expContactProb * infectivity * infectedRatio

  -- run a random susceptible agent for 1.0 time-unit and return true if infected
  infected <- genSusceptibleInfected contactRate infectivity illnessDuration population dt

  -- expect given percentage of Susceptible agents to have become infected
  return $ cover expProb infected 
          ("susceptible agents became infected, expected at least " ++ printf "%.2f" expProb) True

prop_infected_meanIllnessDuration :: Property
prop_infected_meanIllnessDuration = checkCoverage $ do
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
                       -> Double
                       -> Gen Bool
genSusceptibleInfected contactRate infectivity illnessDuration pop dt = do
  g <- genStdGen

  let a   = susceptibleAgent contactRate infectivity illnessDuration g
      n   = floor (1 / dt)
      dts = replicate n (dt, Nothing)
      aos = embed a (pop, dts)

  return $ Infected `elem` aos