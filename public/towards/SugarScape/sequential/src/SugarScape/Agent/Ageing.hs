{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Ageing 
  ( agentAgeing
  , dieOfAge
  ) where

import SugarScape.Agent.Common
import SugarScape.Core.Common
import SugarScape.Core.Model

agentAgeing :: DTime -> AgentLocalMonad g ()
agentAgeing dt = updateAgentState (\s' -> s' { sugAgAge = sugAgAge s' + dt })

dieOfAge :: AgentLocalMonad g Bool
dieOfAge = do
  ageSpan <- agentProperty sugAgMaxAge
  case ageSpan of 
    Nothing -> return False
    Just maxAge -> do
      age <- agentProperty sugAgAge
      return $ age >= maxAge