module Simulation 
  ( simTests
  ) where

import Control.Monad.Random

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SugarScape.Agent
import SugarScape.AgentMonad
import SugarScape.Environment
import SugarScape.Init
import SugarScape.Model
import SugarScape.Simulation

-- need this import for Arbitrary instance of SugEnvironment but Haskell thinks its unused => turn off unused import :(
import Agent
import Environment
import StatsUtils

import Debug.Trace

data SugarScapeSimulation = SugarScapeSimulation [SugAgentState] SugEnvironment deriving Show

--instance Show (SugarScapeSimulation g) where
--  show (SugarScapeSimulation as env) = ""

instance Arbitrary SugarScapeSimulation where
  -- arbitrary :: Gen SugarScapeSimulation
  arbitrary = do
    (env : _) <- vector 1
    ss        <- vector 10
    return $ SugarScapeSimulation ss env

simTests :: RandomGen g 
         => g
         -> TestTree 
simTests g = testGroup "Simulation Tests" [ QC.testProperty "Carrying Capacity" $ prop_carrying_cap g ]

-- what we are doing is replicating sugarscape
-- https://www2.le.ac.uk/departments/interdisciplinary-science/research/replicating-sugarscape

-- TODO: ok this is testing a property of the simulation but is NOT using QuickCheck to generate
-- the random data
-- TODO: also make assumptions about the absolute number of agents at which it stabelises
prop_carrying_cap :: RandomGen g 
                  => g
                  -> Bool
prop_carrying_cap = testPopulationSizeVariance runs
  where
    runs        = 100
    steps       = 400
    stableAfter = 100
    maxVariance = 4

    sugParams   = mkParamsCarryingCapacity

    -- TODO: dont we need to perform a https://en.wikipedia.org/wiki/Chi-squared_test ?
    testPopulationSizeVariance :: RandomGen g 
                               => Int
                               -> g
                               -> Bool
    testPopulationSizeVariance 0 _ = True
    testPopulationSizeVariance n g 
        = trace ("popSizeVariance = " ++ show popSizeVariance ++ " popSizeMean = " ++ show popSizeMean ++ " popSizeMedian = " ++ show popSizeMedian) 
            popSizeVariance <= maxVariance && 
            testPopulationSizeVariance (n - 1) g''
      where
        -- initial RNG
        (g', shuffleRng) = split g
        -- initial agents and environment
        ((initAs, initEnv), g'') = runRand (createSugarScape sugParams) g'
        -- initial simulation state
        (initAis, initSfs) = unzip initAs

        initSimState = mkSimState (simStepSF initAis initSfs (sugEnvironment sugParams) shuffleRng) (mkAbsState $ maximum initAis) initEnv g' 0
        sos          = simulateUntil steps initSimState

        sos'            = drop stableAfter sos
        popSizes        = map (\(_, _, aos) -> fromIntegral $ length aos) sos'
        popSizeVariance = std popSizes
        popSizeMean     = mean popSizes
        popSizeMedian   = median popSizes