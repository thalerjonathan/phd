module Agent.Mating 
  ( prop_agent_acceptMatingRequest
  ) where

import Test.Tasty.QuickCheck as QC

import Agent.Agent
import SugarScape.Core.Model
import SugarScape.Core.Scenario

-- TODO: test isAgentFertileAge
-- TODO: test isAgentFertile

-- NOTE: need quite some customisation of agent state => run in Gen
prop_agent_acceptMatingRequest :: Gen Bool
prop_agent_acceptMatingRequest = do
    as0 <- arbitraryAgentStateFromScenario mkParamsAnimationIII_1
    return $ prop_agent_acceptMatingRequest_prop as0
  where
    prop_agent_acceptMatingRequest_prop :: SugAgentState -> Bool
    prop_agent_acceptMatingRequest_prop _as = False -- TODO: implement 