{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- disable warning for unused imports, need to import Arbitrary instances
module Agent.Ageing
  ( prop_agent_ageing
  , prop_agent_dieOfAge
  ) where

import Data.Maybe

import Test.Tasty.QuickCheck as QC

import Agent.Agent
import SugarScape.Agent.Ageing
import SugarScape.Agent.Interface
import SugarScape.Core.Common
import SugarScape.Core.Model
import Utils.Runner

prop_agent_ageing :: SugAgentState -> DTime -> Bool
prop_agent_ageing as0 dt = as' == asExp && aoEmpty
  where
    (_, as', ao) = runAgentMonadDefaultConst (agentAgeing dt) as0 Nothing Nothing Nothing
    -- maybe not very useful to repeat the implementation in the test but 
    -- for completeness we test it
    asExp = as0 { sugAgAge = sugAgAge as0 + dt } 
    -- agentout must not have changed
    aoEmpty = ao == mkAgentOut

-- NOTE: this property-test needs an adjusted agent state => runs in Gen
-- NOTE: dieOfAge is not AgentLocalMonad but just MonadState => can be ran conveniently with runState
prop_agent_dieOfAge :: SugAgentState -> Gen Bool
prop_agent_dieOfAge as0 = do
    age <- choose (60, 100)
    let as = as0 { sugAgAge = age }
    return $ prop_agent_dieOfAge_prop as age
  where
    prop_agent_dieOfAge_prop :: SugAgentState
                             -> Int
                             -> Bool
    prop_agent_dieOfAge_prop as age
        = died == (age >= asMaxAge) && asUnchanged && aoEmpty
      where
        asMaxAge    = fromJust $ sugAgMaxAge as
        (died, as', ao) = runAgentMonadDefaultConst dieOfAge as Nothing Nothing Nothing
        asUnchanged = as == as'
        -- agentout must not have changed
        aoEmpty = ao == mkAgentOut