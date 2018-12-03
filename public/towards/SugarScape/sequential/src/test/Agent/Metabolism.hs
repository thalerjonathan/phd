module Agent.Metabolism
  ( prop_agent_starved_sugaronly
  , prop_agent_starved_sugarandspice
  , prop_agent_metabolism_sugaronly
  ) where

import Control.Monad.Random

import SugarScape.Agent.Metabolism
import SugarScape.Core.Common
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import Utils.Runner

prop_agent_starved_sugaronly :: RandomGen g 
                             => g
                             -> SugAgentState
                             -> Double
                             -> Bool
prop_agent_starved_sugaronly g0 asInit sugLvl
    = starved == (sugLvl <= 0) && asUnchanged 
  where
    as0            = asInit { sugAgSugarLevel = sugLvl }
    (starved, as') = runAgentMonadDefaultConst starvedToDeath as0 g0
    asUnchanged    = as0 == as'

prop_agent_starved_sugarandspice :: RandomGen g 
                                 => g 
                                 -> SugAgentState
                                 -> Double
                                 -> Double
                                 -> Bool
prop_agent_starved_sugarandspice g0 asInit sugLvl spiLvl
    = starved == (sugLvl <= 0 || spiLvl <= 0) && 
      asUnchanged &&
      absStateUnchanged &&
      envUnchanged 
  where
    as0       = asInit { sugAgSugarLevel = sugLvl, sugAgSpiceLevel = spiLvl }
    env0      = emptyEnvironment
    absState0 = defaultAbsState
    scen      = mkSugarScapeScenario { spSpiceEnabled = True }
    
    (starved, as', absState', env', _) = runAgentMonad starvedToDeath scen 0 as0 absState0 env0 g0
    asUnchanged       = as0 == as'
    absStateUnchanged = absState0 == absState'
    envUnchanged      = env0 == env'

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

    (metabAmount, as', absState', env', _g') = runAgentMonad agentMetabolism scen 0 as0 absState0 env0 g0

    asUnchanged       = as0 == as'
    absStateUnchanged = absState0 == absState'
    envUnchanged      = env0 == env'