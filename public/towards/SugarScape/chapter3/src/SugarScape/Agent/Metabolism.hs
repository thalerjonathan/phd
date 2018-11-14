{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Metabolism 
  ( agentMetabolism
  , starvedToDeath
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Core.Common 
import SugarScape.Core.Model

agentMetabolism :: RandomGen g
                => SugarScapeScenario
                -> AgentId
                -> AgentAction g Int
agentMetabolism params myId
  | spSpiceEnabled params = do
    sugarMetab <- agentProperty sugAgSugarMetab
    sugarLevel <- agentProperty sugAgSugarLevel

    spiceMetab <- agentProperty sugAgSpiceMetab
    spiceLevel <- agentProperty sugAgSpiceLevel

    let sugarLevel' = max 0 (sugarLevel - fromIntegral sugarMetab)
        spiceLevel' = max 0 (spiceLevel - fromIntegral spiceMetab)

    updateAgentState (\s' -> s' { sugAgSugarLevel = sugarLevel'
                                , sugAgSpiceLevel = spiceLevel' })

    -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
    updateSiteWithOccupier myId

    return $ sugarMetab + spiceMetab
  
  | otherwise = do
    sugarMetab <- agentProperty sugAgSugarMetab
    sugarLevel <- agentProperty sugAgSugarLevel

    let sugarLevel' = max 0 (sugarLevel - fromIntegral sugarMetab)
    updateAgentState (\s' -> s' { sugAgSugarLevel = sugarLevel' })

    -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
    updateSiteWithOccupier myId

    return sugarMetab

starvedToDeath :: MonadState SugAgentState m
               => SugarScapeScenario
               -> m Bool
starvedToDeath params 
  | spSpiceEnabled params = do
    sugar <- agentProperty sugAgSugarLevel
    spice <- agentProperty sugAgSpiceLevel
    return $ sugar <= 0 || spice <= 0

  | otherwise = do
    sugar <- agentProperty sugAgSugarLevel
    return $ sugar <= 0