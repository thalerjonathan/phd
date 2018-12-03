{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Ageing 
  ( agentAgeing
  , dieOfAge
  ) where

import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Core.Common
import SugarScape.Core.Model

agentAgeing :: MonadState SugAgentState m => DTime -> m ()
agentAgeing dt = updateAgentState (\s' -> s' { sugAgAge = sugAgAge s' + dt })

dieOfAge :: MonadState SugAgentState m => m Bool
dieOfAge = do
  ageSpan <- agentProperty sugAgMaxAge
  case ageSpan of 
    Nothing -> return False
    Just maxAge -> do
      age <- agentProperty sugAgAge
      return $ age >= maxAge