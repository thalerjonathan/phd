{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Metabolism 
  ( agentMetabolism
  , starvedToDeath
  ) where

import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Model

agentMetabolism :: MonadState SugAgentState m
                => SugarScapeParams
                -> m Int
agentMetabolism params 
  | spSpiceEnabled params = do
    sugarMetab <- agentProperty sugAgSugarMetab
    sugarLevel <- agentProperty sugAgSugarLevel

    spiceMetab <- agentProperty sugAgSpiceMetab
    spiceLevel <- agentProperty sugAgSpiceLevel

    let sugarLevel' = max 0 (sugarLevel - fromIntegral sugarMetab)
        spiceLevel' = max 0 (spiceLevel - fromIntegral spiceMetab)

    updateAgentState (\s' -> s' { sugAgSugarLevel = sugarLevel'
                                , sugAgSpiceLevel = spiceLevel' })

    return $ sugarMetab + spiceMetab
  
  | otherwise = do
    sugarMetab <- agentProperty sugAgSugarMetab
    sugarLevel <- agentProperty sugAgSugarLevel

    let sugarLevel' = max 0 (sugarLevel - fromIntegral sugarMetab)
    updateAgentState (\s' -> s' { sugAgSugarLevel = sugarLevel' })

    return sugarMetab

starvedToDeath :: MonadState SugAgentState m
               => SugarScapeParams
               -> m Bool
starvedToDeath params 
  | spSpiceEnabled params = do
    sugar <- agentProperty sugAgSugarLevel
    spice <- agentProperty sugAgSpiceLevel
    return $ sugar <= 0 || spice <= 0

  | otherwise = do
    sugar <- agentProperty sugAgSugarLevel
    return $ sugar <= 0