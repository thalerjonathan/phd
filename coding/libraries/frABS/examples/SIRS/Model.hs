module SIRS.Model (
    SIRSState (..),
    SIRSMsg (..),
    SIRSAgentState (..),

    SIRSEnvironment,

    SIRSAgentDef,
    SIRSAgentBehaviour,
    SIRSAgentIn,
    SIRSAgentOut,

    SIRSAgentPureBehaviour,
    SIRSAgentMonadicBehaviour,
    
    infectedDuration,
    immuneDuration,
    infectionProbability,
    initialInfectionProb
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SIRSState = Susceptible | Infected | Recovered deriving (Eq, Show)
data SIRSMsg = Contact SIRSState deriving (Eq, Show)

data SIRSAgentState = SIRSAgentState {
    sirsState :: SIRSState,
    sirsTime :: Double,
    sirsCoord :: Discrete2dCoord
} deriving (Show)

type SIRSEnvironment = Discrete2d AgentId

type SIRSAgentDef = AgentDef SIRSAgentState SIRSMsg SIRSEnvironment
type SIRSAgentBehaviour = AgentBehaviour SIRSAgentState SIRSMsg SIRSEnvironment
type SIRSAgentIn = AgentIn SIRSAgentState SIRSMsg SIRSEnvironment
type SIRSAgentOut = AgentOut SIRSAgentState SIRSMsg SIRSEnvironment

type SIRSAgentPureBehaviour = AgentPureBehaviour SIRSAgentState SIRSMsg SIRSEnvironment
type SIRSAgentMonadicBehaviour = AgentMonadicBehaviour SIRSAgentState SIRSMsg SIRSEnvironment
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
infectedDuration :: Double
infectedDuration = 7.0

immuneDuration :: Double
immuneDuration = 3.0

infectionProbability :: Double
infectionProbability = 0.3

initialInfectionProb :: Double
initialInfectionProb = 0.2
------------------------------------------------------------------------------------------------------------------------