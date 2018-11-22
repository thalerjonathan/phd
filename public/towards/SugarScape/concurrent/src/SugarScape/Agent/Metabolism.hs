{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Metabolism 
  ( agentMetabolism
  , starvedToDeath
  ) where

import SugarScape.Agent.Common
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import SugarScape.Core.Utils

agentMetabolism :: AgentLocalMonad g Int
agentMetabolism =
  ifThenElseM
    (spSpiceEnabled <$> scenario)
    (do
      sugarMetab <- agentProperty sugAgSugarMetab
      sugarLevel <- agentProperty sugAgSugarLevel

      spiceMetab <- agentProperty sugAgSpiceMetab
      spiceLevel <- agentProperty sugAgSpiceLevel

      let sugarLevel' = max 0 (sugarLevel - fromIntegral sugarMetab)
          spiceLevel' = max 0 (spiceLevel - fromIntegral spiceMetab)

      updateAgentState (\s' -> s' { sugAgSugarLevel = sugarLevel'
                                  , sugAgSpiceLevel = spiceLevel' })
      -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
      updateSiteOccupied

      return $ sugarMetab + spiceMetab)
    (do
      sugarMetab <- agentProperty sugAgSugarMetab
      sugarLevel <- agentProperty sugAgSugarLevel

      let sugarLevel' = max 0 (sugarLevel - fromIntegral sugarMetab)
      
      updateAgentState (\s' -> s' { sugAgSugarLevel = sugarLevel' })
      -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
      updateSiteOccupied

      return sugarMetab)

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