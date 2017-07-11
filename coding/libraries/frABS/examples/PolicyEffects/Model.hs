module PolicyEffects.Model (
    PolicyEffectsMsg (..),

    PolicyEffectsState,
    PolicyEffectsEnvLink,
    PolicyEffectsEnvCell,
    PolicyEffectsEnvironment,

    PolicyEffectsAgentDef,
    PolicyEffectsAgentBehaviour,
    PolicyEffectsAgentIn,
    PolicyEffectsAgentOut,

    PolicyEffectsEnvironmentReplicator
  ) where

import FRP.FrABS

-- NOTE: this implementation was inspired by this article:
-- http://www.decisionsciencenews.com/2017/06/19/counterintuitive-problem-everyone-room-keeps-giving-dollars-random-others-youll-never-guess-happens-next/
-- It shows how random spending of money creates a huge deviation of wealth

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data PolicyEffectsMsg = Spend Double deriving (Eq, Show)
type PolicyEffectsState = Double

type PolicyEffectsEnvLink = ()
type PolicyEffectsEnvCell = ()
type PolicyEffectsEnvironment = Environment PolicyEffectsEnvCell PolicyEffectsEnvLink

type PolicyEffectsAgentDef = AgentDef PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvCell PolicyEffectsEnvLink
type PolicyEffectsAgentBehaviour = AgentBehaviour PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvCell PolicyEffectsEnvLink
type PolicyEffectsAgentIn = AgentIn PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvCell PolicyEffectsEnvLink
type PolicyEffectsAgentOut = AgentOut PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvCell PolicyEffectsEnvLink

type PolicyEffectsEnvironmentReplicator = EnvironmentReplicator PolicyEffectsEnvCell PolicyEffectsEnvLink
------------------------------------------------------------------------------------------------------------------------