module Agent
  ( agentTests
  ) where

import Control.Monad.Random

import Test.Tasty
import Test.Tasty.QuickCheck as QC

--import SugarScape.AgentMonad
import SugarScape.Model

instance Arbitrary SugAgentState where
  -- arbitrary :: Gen SugAgentState
  arbitrary = do
    randSugarMetab     <- choose sugarMetabolismRange
    randVision         <- choose  visionRange
    randSugarEndowment <- choose sugarEndowmentRange
  
    return SugAgentState {
      sugAgCoord      = (0, 0)
    , sugAgSugarMetab = randSugarMetab
    , sugAgVision     = randVision
    , sugAgSugarLevel = randSugarEndowment
    , sugAgSugarInit  = randSugarEndowment
    }

agentTests :: RandomGen g 
           => g
           -> TestTree 
agentTests g = testGroup "Agent Tests"
            [ QC.testProperty "bla" $ prop_agent_bla g ]

prop_agent_bla :: RandomGen g 
               => g
               -> SugAgentState
               -> Bool
prop_agent_bla g ss = True