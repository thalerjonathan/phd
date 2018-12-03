module Agent.Ageing
  ( prop_agent_dieOfAge
  , prop_agent_ageing
  ) where

import Data.Maybe

import Control.Monad.State.Strict

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

prop_agent_dieOfAge :: SugAgentState
                    -> Int
                    -> Bool
prop_agent_dieOfAge asInit age 
    = died == (age >= asMaxAge) && asUnchanged
  where
    asMaxAge = fromJust $ sugAgMaxAge asInit

    as0 = asInit { sugAgAge = age }

    (died, as') = runState dieOfAge as0
    asUnchanged = as0 == as'

-- no need to run agent monad, state is monad enough
-- no need for environment and absstate
{-
prop_agent_dieOfAge :: RandomGen g 
                    => g
                    -> SugAgentState
                    -> Int
                    -> Bool
prop_agent_dieOfAge g0 asInit age 
    = died == (age >= asMaxAge) &&
      asUnchanged &&
      absStateUnchanged &&
      envUnchanged
  where
    asMaxAge = fromJust $ sugAgMaxAge asInit

    as0       = asInit { sugAgAge = age }
    absState0 = defaultAbsState
    env0      = emptyEnvironment

    (died, as', absState', env', _) = runAgentMonad dieOfAge as0 absState0 env0 g0
    asUnchanged       = as0 == as'
    absStateUnchanged = absState0 == absState'
    envUnchanged      = env0 == env'
-}