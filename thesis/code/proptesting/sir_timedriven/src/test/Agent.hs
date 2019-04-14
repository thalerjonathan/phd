{-# LANGUAGE InstanceSigs #-}
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

contactRate :: Double
contactRate = 5

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

-- clear & stack test sir:sir-agent-test --test-arguments="--quickcheck-tests=1000"

main :: IO ()
main = do
    let tastyQCTests = testGroup "Agent Tests" 
                          [ 
                            QC.testProperty "Susceptible agent mean infected" prop_susceptible_meaninfected
                          , QC.testProperty "Infected agent mean illness duration" prop_infected_meanIllnessDuration
                          ]

    defaultMain tastyQCTests
    
--------------------------------------------------------------------------------
-- PROPERTIES
--------------------------------------------------------------------------------
prop_susceptible_meaninfected :: Property
prop_susceptible_meaninfected = checkCoverage $ do
    pop <- listOf genSIRState
    irs <- vectorOf 100 (genSusceptibleInfectedRatio pop)
   
    let infs = length (filter (==Infected) pop) 
    -- NOTE: we cannot check the infectivity parameter only because it is
    -- tied to the contactRate, therefore we can only check the combined
    -- value. If we also use a random population then we need to divide
    -- by the number of infected agents because in each contact event generated
    -- by contactRate one agent is picked randomly uniformly from the population
    let expected = (contactRate * infectivity) / fromIntegral infs

    let confidence = 0.95
        idsTTest   = tTestSamples TwoTail expected (1 - confidence) irs

    return $ label (printf "epexted %.2f, was %.2f" expected (mean irs)) $ 
             cover 95 (fromMaybe True idsTTest) "mean infectivity" True
  where
    genSusceptibleInfectedRatio :: [SIRState] -> Gen Double
    genSusceptibleInfectedRatio pop = do
        let repls = 1000
        is <- vectorOf repls genSusceptibleInfected
        let ir = fromIntegral (length (filter (==True) is)) / fromIntegral repls
        return ir
      where
        genSusceptibleInfected :: Gen Bool
        genSusceptibleInfected = do
          g  <- genStdGen

          let a   = susceptibleAgent contactRate infectivity illnessDuration g
              dt  = 0.01 -- need very small dt of 0.01 due to sampling issues
              n   = floor (1 / dt)
              dts = replicate n (dt, Nothing)
              aos = embed a (pop, dts)
              --aos = embed a ([Infected], dts)

          return $ Infected `elem` aos

prop_infected_meanIllnessDuration :: Property
prop_infected_meanIllnessDuration = checkCoverage $ do
    ids <- vectorOf 100 genInfectedIllnessDuration
    
    let confidence = 0.95
        idsTTest   = tTestSamples TwoTail illnessDuration (1 - confidence) ids

    return $ cover 95 (fromMaybe True idsTTest) ("mean illness duration of " ++ show illnessDuration) True
  where
    genInfectedIllnessDuration :: Gen Double
    genInfectedIllnessDuration = do
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