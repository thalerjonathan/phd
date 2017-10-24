{-# LANGUAGE Arrows #-}
module NewAgents.Agent (
    newAgentBehaviour
  ) where

import NewAgents.Model

import FRP.FrABS
import FRP.Yampa

------------------------------------------------------------------------------------------------------------------------
newAgentBehaviour :: NewAgentBehaviour
newAgentBehaviour = doRepeatedlyEvery 1.0 (updateDomainStateR (+1))
------------------------------------------------------------------------------------------------------------------------