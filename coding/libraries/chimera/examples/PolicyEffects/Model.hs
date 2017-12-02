module PolicyEffects.Model (
    PolicyEffectsMsg (..),

    PolicyEffectsState,
    PolicyEffectsEnvironment,

    PolicyEffectsAgentDef,
    PolicyEffectsAgentBehaviour,
    PolicyEffectsAgentIn,
    PolicyEffectsAgentOut,
    PolicyEffectsAgentObservable,
    
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

type PolicyEffectsEnvironment = Network ()

type PolicyEffectsAgentDef = AgentDef PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvironment 
type PolicyEffectsAgentBehaviour = AgentBehaviour PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvironment 
type PolicyEffectsAgentIn = AgentIn PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvironment 
type PolicyEffectsAgentOut = AgentOut PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvironment 
type PolicyEffectsAgentObservable = AgentObservable PolicyEffectsState

type PolicyEffectsEnvironmentReplicator = EnvironmentReplicator PolicyEffectsEnvironment 
------------------------------------------------------------------------------------------------------------------------