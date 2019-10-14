module Agent.Mating
  ( prop_agent_acceptMatingRequest
  , prop_agent_handleMatingRequest
  ) where

import Test.Tasty.QuickCheck as QC

import Agent.Agent
import SugarScape.Agent.Interface
import SugarScape.Agent.Mating 
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import Utils.Runner

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
            && agentStateUnchanged
            && aoEmpty
      where
        (accept, as', ao) = runAgentMonadDefaultConst (acceptMatingRequest otherGender) as Nothing Nothing Nothing

        genderDiffer     = otherGender /= sugAgGender as

        agentFertile     = withinFertileAge && hasFertileWealth
        withinFertileAge = sugAgAge as >= fst (sugAgFertAgeRange as)
                            && sugAgAge as <= snd (sugAgFertAgeRange as)
        hasFertileWealth = sugAgSugarLevel as >= sugAgInitSugEndow as 
                            && sugAgSpiceLevel as >= sugAgInitSpiEndow as
        -- agent-state must not change during this function call
        agentStateUnchanged = as == as'
        -- agent out must not change
        aoEmpty = ao == mkAgentOut

prop_agent_handleMatingRequest :: AgentGender -> Gen Bool
prop_agent_handleMatingRequest otherGender = do
    as0 <- arbitraryAgentStateFromScenario mkParamsAnimationIII_1

    -- NOTE: need adjust sugar level, spice level, and age, otherwise never fertile
    randSugLvl <- choose (0, 150)
    randSpiLvl <- choose (0, 150)
    randAge    <- choose (0, 100)
    let as = as0 { sugAgSugarLevel = randSugLvl
                 , sugAgSpiceLevel = randSpiLvl
                 , sugAgAge        = randAge }

    return $ prop_agent_handleMatingRequest_prop as
  where
    prop_agent_handleMatingRequest_prop :: SugAgentState -> Bool
    prop_agent_handleMatingRequest_prop as 
        | accept    = ao == aoExpAccept
        | otherwise = ao == aoExpRefuse
      where
        -- need to know if the agent has accepted the request, output depends on
        -- it - acceptMatingRequest is tested in a different property test.
        -- Does NOT change the agent-state, can ignore it using evalState
        (accept, _, _) = runAgentMonadDefaultConst (acceptMatingRequest otherGender) as Nothing Nothing Nothing

        senderId  = 42
        (_, as', ao) = runAgentMonadDefaultConst (handleMatingRequest senderId otherGender) as Nothing Nothing Nothing

        expAccept   = (sugAgSugarLevel as / 2, 
                       sugAgSpiceLevel as / 2, 
                       sugAgSugarMetab as, 
                       sugAgVision as, 
                       sugAgCultureTag as,
                       sugAgImSysGeno as)
        aoExpAccept = sendEventToAo senderId (MatingReply $ Just expAccept)
        aoExpRefuse = sendEventToAo senderId (MatingReply Nothing)

-- TODO: handleMatingTx