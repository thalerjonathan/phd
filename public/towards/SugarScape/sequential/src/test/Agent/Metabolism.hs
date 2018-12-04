module Agent.Metabolism
  ( prop_agent_starved_sugaronly
  , prop_agent_starved_sugarandspice
  , prop_agent_metabolism_sugaronly
  ) where

import Control.Monad.Random

import SugarScape.Agent.Common
import SugarScape.Agent.Metabolism
import SugarScape.Core.Common
import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import Utils.Runner

prop_agent_starved_sugaronly :: SugAgentState
                             -> Double
                             -> Bool
prop_agent_starved_sugaronly asInit sugLvl
    = starved == (sugLvl <= 0) && asUnchanged 
  where
    -- change sugar-level to random value
    as0 = asInit { sugAgSugarLevel = sugLvl }
    -- run the agent with defaults for Scenario, ABSState, Environment and require them not to change
    (starved, as') = runAgentMonadDefaultConst starvedToDeath as0 Nothing Nothing Nothing 
    -- agent-state must not change
    asUnchanged = as0 == as'

prop_agent_starved_sugarandspice :: SugAgentState
                                 -> Double
                                 -> Double
                                 -> Bool
prop_agent_starved_sugarandspice asInit sugLvl spiLvl
    = starved == (sugLvl <= 0 || spiLvl <= 0) && asUnchanged
  where
    -- change sugar- and spice-level to random values
    as0 = asInit { sugAgSugarLevel = sugLvl, sugAgSpiceLevel = spiLvl }
    -- changed scenario: enable spice
    sc = mkSugarScapeScenario { spSpiceEnabled = True }
    
    -- run the agent with defaults for ABSState, Environment and require them not to change
    (starved, as') = runAgentMonadDefaultConst starvedToDeath as0 (Just sc) Nothing Nothing
    -- agent-state must not change!
    asUnchanged = as0 == as'

prop_agent_metabolism_sugaronly :: SugAgentState
                                -> SugEnvSite
                                -> Bool
prop_agent_metabolism_sugaronly as0 site0
    = metabAmount == metabAmountExp
        && absStateUnchanged
        && asStateExpectedChange
        && siteExpectedChange
  where
    -- default AgentId
    aid       = 0
    -- default ABSState
    absState0 = defaultAbsState
    -- singleton site environment coordinat
    siteCoord = (0, 0)
    -- default RNG, wont be used in agent computation but need one
    g0 = mkStdGen 42
    -- initialise spice to some non-zero values to prevent NaN in MRS 
    -- which would result in a failed equality test
    as = as0 { sugAgSpiceLevel = 1, sugAgSpiceMetab = 1}

    -- change site to have an occupier
    occ0 = occupier aid as
    site = site0 { sugEnvSiteOccupier = Just occ0 }
    -- create a 1x1 environment with the given site
    env  = createDiscrete2d (1, 1) moore WrapBoth [(siteCoord, site)]
    -- default scenario (no spice)
    sc = mkSugarScapeScenario

    -- either the agent consumes its remaining sugar level if the metabolism is larger
    -- or if there is still sugar left, it will simply take away the metabolism amount
    metabAmountExp = min (sugAgSugarLevel as) (fromIntegral $ sugAgSugarMetab as)

    -- run the agent computation
    (metabAmount, as', absState', env', _) = runAgentMonad agentMetabolism sc aid as absState0 env g0

    -- ABSState must not change
    absStateUnchanged = absState0 == absState'
    
    -- check agent state has changed: sugarlevel was reduced by metabAmount
    expSugarLevel = sugAgSugarLevel as - metabAmountExp
    asExp         = as { sugAgSugarLevel = expSugarLevel }
    asStateExpectedChange = as' == asExp 

    -- check if occupier on cell in environment has changed
    occExp  = occ0 { sugEnvOccSugarWealth = expSugarLevel, sugEnvOccMRS = mrsState asExp }
    siteExp = site { sugEnvSiteOccupier = Just occExp }
    site'   = cellAt siteCoord env'
    siteExpectedChange = site' == siteExp