module NewAgents.Model (
    NewAgentState,
    NewAgentMsg (..),

    NewAgentEnvironment,
    NewAgentEnvironmentBehaviour,
    NewAgentEnvironmentCollapsing,

    NewAgentDef,
    NewAgentBehaviour,
    NewAgentIn,
    NewAgentOut
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
type NewAgentState = Int
data NewAgentMsg = None

type NewAgentEnvironment = ()
type NewAgentEnvironmentBehaviour = EnvironmentBehaviour NewAgentEnvironment
type NewAgentEnvironmentCollapsing = EnvironmentCollapsing NewAgentEnvironment

type NewAgentDef = AgentDef NewAgentState NewAgentMsg NewAgentEnvironment
type NewAgentBehaviour = AgentBehaviour NewAgentState NewAgentMsg NewAgentEnvironment
type NewAgentIn = AgentIn NewAgentState NewAgentMsg NewAgentEnvironment
type NewAgentOut = AgentOut NewAgentState NewAgentMsg NewAgentEnvironment
------------------------------------------------------------------------------------------------------------------------
