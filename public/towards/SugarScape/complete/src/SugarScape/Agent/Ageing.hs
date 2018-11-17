{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Ageing 
  ( agentAgeing
  , dieOfAge
  ) where

import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Core.Model

agentAgeing :: MonadState SugAgentState m => m ()
agentAgeing = updateAgentState (\s' -> s' { sugAgAge = sugAgAge s' + 1 })

dieOfAge :: MonadState SugAgentState m => m Bool
dieOfAge = do
  ageSpan <- agentProperty sugAgMaxAge
  case ageSpan of 
    Nothing -> return False
    Just maxAge -> do
      age <- agentProperty sugAgAge
      return $ age >= maxAge