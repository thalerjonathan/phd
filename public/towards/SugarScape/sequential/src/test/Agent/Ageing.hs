module Agent.Ageing
  ( prop_agent_ageing
  , prop_agent_dieOfAge
  ) where

import Data.Maybe

import Control.Monad.State.Strict
import Test.Tasty.QuickCheck as QC

import SugarScape.Agent.Ageing
import SugarScape.Core.Common
import SugarScape.Core.Model

prop_agent_ageing :: SugAgentState -> DTime -> Bool
prop_agent_ageing as0 dt = as' == asExp
  where
    as' = execState (agentAgeing dt) as0
    -- maybe not very useful to repeat the implementation in the test but 
    -- for completeness we test it
    asExp = as0 { sugAgAge = sugAgAge as0 + dt } 

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
        = died == (age >= asMaxAge) && asUnchanged
      where
        asMaxAge    = fromJust $ sugAgMaxAge as
        (died, as') = runState dieOfAge as
        asUnchanged = as == as'