module SugarScape.Agent.Disease
  ( agentDisease
  ) where

import Data.Maybe

import Control.Monad.Random

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Core.Model
import SugarScape.Core.Scenario

agentDisease :: RandomGen g
             => SugarScapeScenario
             -> AgentId
             -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
             -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
agentDisease params _myId cont
  | isNothing $ spDiseasesEnabled params = cont
  | otherwise = cont -- TODO: implement