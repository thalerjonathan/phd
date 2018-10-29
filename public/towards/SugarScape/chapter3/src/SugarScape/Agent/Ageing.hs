{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Ageing 
  ( agentAgeing
  , dieOfAge
  ) where

import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Model

agentAgeing :: MonadState SugAgentState m
            => Int
            -> m ()
agentAgeing age = updateAgentState (\s' -> s' { sugAgAge = age })

dieOfAge :: MonadState SugAgentState m
          => m Bool
dieOfAge = do
  ageSpan <- agentProperty sugAgMaxAge
  case ageSpan of 
    Nothing -> return False
    Just maxAge -> do
      age <- agentProperty sugAgAge
      return $ age >= maxAge