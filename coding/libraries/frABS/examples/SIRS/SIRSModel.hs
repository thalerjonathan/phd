module SIRS.SIRSModel (
    SIRSState (..),
    SIRSMsg (..),
    SIRSAgentState (..),

    SIRSEnvLink,
    SIRSEnvCell,
    SIRSEnvironment,

    SIRSAgentDef,
    SIRSAgentBehaviour,
    SIRSAgentIn,
    SIRSAgentOut,

    infectedDuration,
    immuneDuration,
    infectionProbability,
    initialInfectionProb
  ) where

import FrABS.Agent.Agent
import FrABS.Env.Environment

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SIRSState = Susceptible | Infected | Recovered deriving (Eq, Show)
data SIRSMsg = Contact SIRSState deriving (Eq, Show)

data SIRSAgentState = SIRSAgentState {
    sirsState :: SIRSState,
    sirsTime :: Double
} deriving (Show)

type SIRSEnvLink = ()
type SIRSEnvCell = AgentId
type SIRSEnvironment = Environment SIRSEnvCell SIRSEnvLink

type SIRSAgentDef = AgentDef SIRSAgentState SIRSMsg SIRSEnvCell SIRSEnvLink
type SIRSAgentBehaviour = AgentBehaviour SIRSAgentState SIRSMsg SIRSEnvCell SIRSEnvLink
type SIRSAgentIn = AgentIn SIRSAgentState SIRSMsg SIRSEnvCell SIRSEnvLink
type SIRSAgentOut = AgentOut SIRSAgentState SIRSMsg SIRSEnvCell SIRSEnvLink
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