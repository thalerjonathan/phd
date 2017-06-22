module SIRS.SIRSModel where

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SIRSState = Susceptible | Infected | Recovered deriving (Eq, Show)
data SIRSMsg = Contact SIRSState

type SIRSCoord = (Int, Int)

-- TODO: move 2d discrete coordinates to env / agent instead of agentstate s: map of agentid to coords? or spatialinfo in agentin/out?

data SIRSAgentState = SIRSAgentState {
    sirsState :: SIRSState,
    sirsCoord :: SIRSCoord,
    sirsTime :: Double
} deriving (Show)

type SIRSEnvCell = AgentId
type SIRSEnvironment = Environment SIRSEnvCell ()

type SIRSAgentDef = AgentDef SIRSAgentState SIRSMsg SIRSEnvCell ()
type SIRSAgentBehaviour = AgentBehaviour SIRSAgentState SIRSMsg SIRSEnvCell ()
type SIRSAgentIn = AgentIn SIRSAgentState SIRSMsg SIRSEnvCell ()
type SIRSAgentOut = AgentOut SIRSAgentState SIRSMsg SIRSEnvCell ()
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
infectedDuration :: Double
infectedDuration = 7.0

immuneDuration :: Double
immuneDuration = 3000.0

infectionProbability :: Double
infectionProbability = 0.3

initialInfectionProb :: Double
initialInfectionProb = 0.2
------------------------------------------------------------------------------------------------------------------------