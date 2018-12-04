module Agent.Tests 
  ( agentTests
  ) where

import Control.Monad.Random
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Agent.Agent
import Agent.Ageing
import Agent.Metabolism
import Environment.Environment

agentTests :: RandomGen g 
           => g
           -> TestTree 
agentTests g = testGroup "Agent Tests"
                [ QC.testProperty "Ageing by DTime" prop_agent_ageing
                , QC.testProperty "Die Of Age" $ \a -> do
                                                          age <- choose (60, 100)
                                                          return $ prop_agent_dieOfAge a age

                , QC.testProperty "Starved To Death Sugar only" $ prop_agent_starved_sugaronly g
                , QC.testProperty "Starved To Death Sugar and Spice" $ prop_agent_starved_sugarandspice g
                , QC.testProperty "Metabolism Sugar only" $ prop_agent_metabolism_sugaronly g
                ] --, QC.testProperty "Metabolism" $ prop_agent_metabolism g
