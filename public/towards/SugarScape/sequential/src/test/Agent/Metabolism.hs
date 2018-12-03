module Agent.Metabolism
  ( prop_agent_starved_sugaronly
  , prop_agent_starved_sugarandspice
  , prop_agent_metabolism_sugaronly
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict

import Agent.Agent
import SugarScape.Agent.Metabolism
import SugarScape.Core.Common
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import Utils.Runner

prop_agent_starved_sugaronly :: SugAgentState
                             -> Double
                             -> Bool
prop_agent_starved_sugaronly asInit sugLvl 
    = starved == (sugLvl <= 0) && asUnchanged 
  where
    as0  = asInit { sugAgSugarLevel = sugLvl }
    (starved, as') = runState (starvedToDeath mkSugarScapeScenario) as0
    asUnchanged    = as0 == as'

prop_agent_starved_sugarandspice :: SugAgentState
                                 -> Double
                                 -> Double
                                 -> Bool
prop_agent_starved_sugarandspice asInit sugLvl spiLvl
    = starved == (sugLvl <= 0 || spiLvl <= 0) && asUnchanged 
  where
    as0  = asInit { sugAgSugarLevel = sugLvl, sugAgSpiceLevel = spiLvl }
    scen = mkSugarScapeScenario { spSpiceEnabled = True }

    (starved, as') = runState (starvedToDeath scen) as0
    asUnchanged    = as0 == as'

prop_agent_metabolism_sugaronly :: RandomGen g 
                                => g
                                -> SugAgentState
                                -> Bool
prop_agent_metabolism_sugaronly g0 as0 
    = metabAmount == metabAmountExp &&
      asUnchanged &&
      absStateUnchanged &&
      envUnchanged
  where
    absState0 = defaultAbsState
    -- TODO: we need a proper environment here, with the agent occupying it
    env0           = emptyEnvironment
    metabAmountExp = 0
    scen = mkSugarScapeScenario

    (metabAmount, as', absState', env', _g') = runAgentMonad (agentMetabolism scen 0) as0 absState0 env0 g0

    asUnchanged       = as0 == as'
    absStateUnchanged = absState0 == absState'
    envUnchanged      = env0 == env'
    -- rngUnchanged      = g0 == _g'