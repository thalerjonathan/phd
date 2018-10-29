{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Metabolism 
  ( agentMetabolism
  , starvedToDeath
  ) where

import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Model

agentMetabolism :: MonadState SugAgentState m
                => m Int
agentMetabolism = do
  sugarMetab <- agentProperty sugAgSugarMetab
  sugarLevel <- agentProperty sugAgSugarLevel

  let sugarLevel' = max 0 (sugarLevel - fromIntegral sugarMetab)

  updateAgentState (\s' -> s' { sugAgSugarLevel = sugarLevel' })

  return sugarMetab

starvedToDeath :: MonadState SugAgentState m
               => m Bool
starvedToDeath = do
  sugar <- agentProperty sugAgSugarLevel
  return $ sugar <= 0