module Agent.Ageing
  ( prop_agent_dieOfAge
  ) where

import Data.Maybe

import Control.Monad.Random

import Agent.Agent
import SugarScape.Agent.Ageing
import SugarScape.Core.Common
import SugarScape.Core.Model
import Utils.Runner

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

    as0 = asInit { sugAgAge = age }
    absState0 = defaultAbsState
    env0      = emptyEnvironment

    (died, as', absState', env', _) = runAgentMonad dieOfAge as0 absState0 env0 g0
    asUnchanged       = as0 == as'
    absStateUnchanged = absState0 == absState'
    envUnchanged      = env0 == env'