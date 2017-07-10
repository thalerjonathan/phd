module PolicyEffects.Model (
    PolicyEffectsMsg (..),

    PolicyEffectsState,
    PolicyEffectsEnvLink,
    PolicyEffectsEnvCell,
    PolicyEffectsEnvironment,

    PolicyEffectsAgentDef,
    PolicyEffectsAgentBehaviour,
    PolicyEffectsAgentIn,
    PolicyEffectsAgentOut
  ) where

import FrABS.Agent.Agent
import FrABS.Env.Environment

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data PolicyEffectsMsg = Spend Int deriving (Eq, Show)
type PolicyEffectsState = Int

type PolicyEffectsEnvLink = ()
type PolicyEffectsEnvCell = ()
type PolicyEffectsEnvironment = Environment PolicyEffectsEnvCell PolicyEffectsEnvLink

type PolicyEffectsAgentDef = AgentDef PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvCell PolicyEffectsEnvLink
type PolicyEffectsAgentBehaviour = AgentBehaviour PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvCell PolicyEffectsEnvLink
type PolicyEffectsAgentIn = AgentIn PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvCell PolicyEffectsEnvLink
type PolicyEffectsAgentOut = AgentOut PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvCell PolicyEffectsEnvLink
------------------------------------------------------------------------------------------------------------------------