module Model 
  (
    NewAgentState
  , NewAgentMsg (..)

  , NewAgentEnvironment

  , NewAgentDef
  , NewAgentBehaviour
  , NewAgentIn
  , NewAgentOut
  , NewAgentObservable
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
type NewAgentState        = Int
data NewAgentMsg          = None

type NewAgentEnvironment  = ()

type NewAgentDef          = AgentDef NewAgentState NewAgentMsg NewAgentEnvironment
type NewAgentBehaviour    = AgentBehaviour NewAgentState NewAgentMsg NewAgentEnvironment
type NewAgentIn           = AgentIn NewAgentState NewAgentMsg NewAgentEnvironment
type NewAgentOut          = AgentOut NewAgentState NewAgentMsg NewAgentEnvironment
type NewAgentObservable   = AgentObservable NewAgentState
------------------------------------------------------------------------------------------------------------------------
