module Agent.Mating 
  ( prop_agent_acceptMatingRequest
  ) where

import Control.Monad.State.Strict
import Test.Tasty.QuickCheck as QC

import Agent.Agent
import SugarScape.Agent.Mating 
import SugarScape.Core.Model
import SugarScape.Core.Scenario

instance Arbitrary AgentGender where
  -- arbitrary :: Gen AgentGender
  arbitrary = elements [Male, Female]

-- NOTE: no need to test isFertile, is done implicitly with prop_agent_acceptMatingRequest 

-- NOTE: need quite some customisation of agent state => run in Gen
prop_agent_acceptMatingRequest :: AgentGender -> Gen Bool
prop_agent_acceptMatingRequest otherGender = do
    as0 <- arbitraryAgentStateFromScenario mkParamsAnimationIII_1

    -- NOTE: need adjust sugar level, spice level, and age, otherwise always false
    randSugLvl <- choose (0, 150)
    randSpiLvl <- choose (0, 150)
    randAge    <- choose (0, 100)
    let as = as0 { sugAgSugarLevel = randSugLvl
                 , sugAgSpiceLevel = randSpiLvl
                 , sugAgAge        = randAge }

    return $ prop_agent_acceptMatingRequest_prop as
  where
    prop_agent_acceptMatingRequest_prop :: SugAgentState -> Bool
    prop_agent_acceptMatingRequest_prop as 
        = accept == (genderDiffer && agentFertile)
      where
        (accept, as')    = runState (acceptMatingRequest otherGender) as

        genderDiffer     = otherGender /= sugAgGender as

        agentFertile     = withinFertileAge && hasFertileWealth
        withinFertileAge = sugAgAge as >= fst (sugAgFertAgeRange as)
                            && sugAgAge as <= snd (sugAgFertAgeRange as)
        hasFertileWealth = sugAgSugarLevel as >= sugAgInitSugEndow as 
                            && sugAgSpiceLevel as >= sugAgInitSpiEndow as
