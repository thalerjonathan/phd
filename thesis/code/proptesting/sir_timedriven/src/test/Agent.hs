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

-- clear & stack test sir:sir-agent-test --test-arguments="--quickcheck-tests=1000"

main :: IO ()
main = defaultMain $ testGroup "Agent Tests" 
          [ 
            QC.testProperty 
              "Susceptible agent mean infected"
              prop_susceptible_meaninfected
          , QC.testProperty 
              "Infected agent mean illness duration"
              prop_infected_meanIllnessDuration
          ]

--------------------------------------------------------------------------------
-- PROPERTIES
--------------------------------------------------------------------------------
susceptible_meaninfected_repls :: Int
susceptible_meaninfected_repls = 1000

-- TODO: this doesn't seem to work propertly, the sampling issue is really
-- annoying...
prop_susceptible_meaninfected :: Property
prop_susceptible_meaninfected = checkCoverage $ do
    let repls = susceptible_meaninfected_repls
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

    irs <- vectorOf repls (genSusceptibleInfectedRatio contactRate infectivity illnessDuration pop)

    let confidence = 0.95
        idsTTest   = tTestSamples TwoTail expected (1 - confidence) irs

    return $ label (printf "epexted %.2f, was %.2f" expected (mean irs)) $ 
             cover 95 (fromMaybe True idsTTest) "mean infectivity" True
  where
    genSusceptibleInfectedRatio :: Double
                                -> Double 
                                -> Double
                                -> [SIRState]
                                -> Gen Double
    genSusceptibleInfectedRatio contactRate infectivity illnessDuration pop = do
        let repls = susceptible_meaninfected_repls
        is <- vectorOf repls genSusceptibleInfected
        let ir = fromIntegral (length (filter (==True) is)) / fromIntegral repls
        return ir
      where
        genSusceptibleInfected :: Gen Bool
        genSusceptibleInfected = do
          g <- genStdGen

          let a   = susceptibleAgent contactRate infectivity illnessDuration g
              dt  = 0.01 -- need very small dt of 0.01 due to sampling issues
              n   = floor (1 / dt)
              dts = replicate n (dt, Nothing)
              aos = embed a (pop, dts)

          return $ Infected `elem` aos

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