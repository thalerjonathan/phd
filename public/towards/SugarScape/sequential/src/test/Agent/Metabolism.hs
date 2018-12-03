module Agent.Metabolism
  ( prop_agent_starved
  ) where

import Control.Monad.Random

import Agent.Agent
import SugarScape.Agent.Metabolism
import SugarScape.Core.Common
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import Utils.Runner

prop_agent_starved :: RandomGen g 
                   => g
                   -> SugAgentState
                   -> Double
                   -> Bool
prop_agent_starved g0 asInit sugLvl 
    = starved == (sugLvl <= 0) &&
      asUnchanged &&
      absStateUnchanged &&
      envUnchanged
  where
    as0 = asInit { sugAgSugarLevel = sugLvl }
    absState0 = defaultAbsState
    env0      = emptyEnvironment

    (starved, as', absState', env', _) = runAgentMonad (starvedToDeath mkSugarScapeScenario) as0 absState0 env0 g0
    asUnchanged       = as0 == as'
    absStateUnchanged = absState0 == absState'
    envUnchanged      = env0 == env'


{-
prop_agent_metabolism :: RandomGen g 
                      => g
                      -> SugAgentState
                      -> Bool
prop_agent_metabolism g0 as0 
    = isDead ao == metabKills &&
      asUnchanged &&
      absStateUnchanged &&
      envUnchanged
  where
    absState0 = defaultAbsState
    -- TODO: we need a proper environment here, with the agent occupying it
    env0      = emptyEnvironment

    metabKills = sugAgSugarMetab as0 >= sugAgSugarLevel as0

    (_, as', absState', env', _) = runAgentMonad agentMetabolism as0 absState0 env0 g0
    asUnchanged       = as0 == as'
    absStateUnchanged = absState0 == absState'
    envUnchanged      = env0 == env'
-}