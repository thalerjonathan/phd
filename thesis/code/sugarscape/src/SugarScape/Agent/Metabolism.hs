{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Metabolism 
  ( agentMetabolism
  , starvedToDeath
  ) where

import SugarScape.Agent.Common
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import SugarScape.Core.Utils

agentMetabolism :: AgentLocalMonad g Double
agentMetabolism =
  ifThenElseM
    (spSpiceEnabled <$> scenario)
    (do
      sugarMetab <- agentProperty sugAgSugarMetab
      sugarLevel <- agentProperty sugAgSugarLevel

      spiceMetab <- agentProperty sugAgSpiceMetab
      spiceLevel <- agentProperty sugAgSpiceLevel

      -- return the actual value which was metabolised which could be less than
      -- the metabolism value in case the agent owns less of the respective resource
      let sugarMetab' = min sugarLevel (fromIntegral sugarMetab)
          spiceMetab' = min spiceLevel (fromIntegral spiceMetab)
          -- negative values shouldn't occur due to min check but better be safe than sorry
          -- with floating point
          sugarLevel' = max 0 (sugarLevel - sugarMetab')
          spiceLevel' = max 0 (spiceLevel - spiceMetab')

      updateAgentState (\s' -> s' { sugAgSugarLevel = sugarLevel'
                                  , sugAgSpiceLevel = spiceLevel' })
      -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
      updateSiteOccupied

      return $ sugarMetab' + spiceMetab')
    (do
      sugarMetab <- agentProperty sugAgSugarMetab
      sugarLevel <- agentProperty sugAgSugarLevel

      -- return the actual value which was metabolised which could be less than
      -- the metabolism value in case the agent owns less of the respective resource
      let sugarMetab' = min sugarLevel (fromIntegral sugarMetab)
          -- negative values shouldn't occur due to min check but better be safe than sorry
          -- with floating point
          sugarLevel' = max 0 (sugarLevel - sugarMetab')
      
      updateAgentState (\s' -> s' { sugAgSugarLevel = sugarLevel' })
      -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
      updateSiteOccupied

      return sugarMetab')

starvedToDeath :: AgentLocalMonad g Bool
starvedToDeath =
  ifThenElseM
    (spSpiceEnabled <$> scenario)
    (do
      sugar <- agentProperty sugAgSugarLevel
      spice <- agentProperty sugAgSpiceLevel
      return $ sugar <= 0 || spice <= 0)
    (do
      sugar <- agentProperty sugAgSugarLevel
      return $ sugar <= 0)