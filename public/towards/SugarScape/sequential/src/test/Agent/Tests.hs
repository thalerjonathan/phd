module Agent.Tests 
  ( agentTests
  ) where

import Control.Monad.Random
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Agent.Ageing
import Agent.Metabolism

agentTests :: RandomGen g 
           => g
           -> TestTree 
agentTests g = testGroup "Agent Tests"
                [ QC.testProperty "Starved To Death" $ prop_agent_starved g
                , QC.testProperty "Die Of Age" $ \a -> do
                                                          age <- choose (60, 100)
                                                          return $ prop_agent_dieOfAge g a age
                ] --, QC.testProperty "Metabolism" $ prop_agent_metabolism g]
