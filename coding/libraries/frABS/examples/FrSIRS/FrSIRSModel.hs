module FrSIRS.FrSIRSModel (
    FrSIRSState (..),
    FrSIRSMsg (..),

    FrSIRSAgentState,
    FrSIRSEnvLink,
    FrSIRSEnvCell,
    FrSIRSEnvironment,

    FrSIRSAgentDef,
    FrSIRSAgentBehaviour,
    FrSIRSAgentIn,
    FrSIRSAgentOut,

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
data FrSIRSState = Susceptible | Infected | Recovered deriving (Eq, Show)
data FrSIRSMsg = Contact FrSIRSState deriving (Eq, Show)

type FrSIRSAgentState = FrSIRSState

type FrSIRSEnvLink = ()
type FrSIRSEnvCell = AgentId
type FrSIRSEnvironment = Environment FrSIRSEnvCell FrSIRSEnvLink

type FrSIRSAgentDef = AgentDef FrSIRSAgentState FrSIRSMsg FrSIRSEnvCell FrSIRSEnvLink
type FrSIRSAgentBehaviour = AgentBehaviour FrSIRSAgentState FrSIRSMsg FrSIRSEnvCell FrSIRSEnvLink
type FrSIRSAgentIn = AgentIn FrSIRSAgentState FrSIRSMsg FrSIRSEnvCell FrSIRSEnvLink
type FrSIRSAgentOut = AgentOut FrSIRSAgentState FrSIRSMsg FrSIRSEnvCell FrSIRSEnvLink
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