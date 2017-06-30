module FrSIRS.FrSIRSModel (
    SIRSState (..),
    FrSIRSMsg (..),

    FrSIRSAgentState,
    FrSIRSEnvLink,
    FrSIRSEnvCell,
    FrSIRSEnvironment,

    FrSIRSAgentDef,
    FrSIRSAgentBehaviour,
    FrSIRSAgentIn,
    FrSIRSAgentOut,

    illnessDuration,
    immuneDuration,
    contactRate,
    infectivity,
    initialInfectionProb
  ) where

import FrABS.Agent.Agent
import FrABS.Env.Environment

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SIRSState = Susceptible | Infected | Recovered deriving (Eq, Show)
data FrSIRSMsg = Contact SIRSState deriving (Eq, Show)

type FrSIRSAgentState = SIRSState

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
illnessDuration :: Double
illnessDuration = 15

immuneDuration :: Double
immuneDuration = 3000.0

-- average number of contacts per time-unit
contactRate :: Double
contactRate = 5

-- average probability of getting infected 
infectivity :: Double
infectivity = 0.05

initialInfectionProb :: Double
initialInfectionProb = 0.2
------------------------------------------------------------------------------------------------------------------------