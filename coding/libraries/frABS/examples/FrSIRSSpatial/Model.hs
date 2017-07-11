module FrSIRSSpatial.Model (
    SIRSState (..),
    FrSIRSSpatialMsg (..),

    FrSIRSSpatialAgentState,
    FrSIRSSpatialEnvLink,
    FrSIRSSpatialEnvCell,
    FrSIRSSpatialEnvironment,

    FrSIRSSpatialAgentDef,
    FrSIRSSpatialAgentBehaviour,
    FrSIRSSpatialAgentIn,
    FrSIRSSpatialAgentOut,

    illnessDuration,
    immuneDuration,
    contactRate,
    infectivity,
    initialInfectionProb
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SIRSState = Susceptible | Infected | Recovered deriving (Eq, Show)
data FrSIRSSpatialMsg = Contact SIRSState deriving (Eq, Show)

type FrSIRSSpatialAgentState = SIRSState

type FrSIRSSpatialEnvLink = ()
type FrSIRSSpatialEnvCell = AgentId
type FrSIRSSpatialEnvironment = Environment FrSIRSSpatialEnvCell FrSIRSSpatialEnvLink

type FrSIRSSpatialAgentDef = AgentDef FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvCell FrSIRSSpatialEnvLink
type FrSIRSSpatialAgentBehaviour = AgentBehaviour FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvCell FrSIRSSpatialEnvLink
type FrSIRSSpatialAgentIn = AgentIn FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvCell FrSIRSSpatialEnvLink
type FrSIRSSpatialAgentOut = AgentOut FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvCell FrSIRSSpatialEnvLink
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
illnessDuration :: Double
illnessDuration = 15

immuneDuration :: Double
immuneDuration = 3000.0

-- average number of contacts per time-unit
contactRate :: Double
contactRate = 7

-- average probability of getting infected 
infectivity :: Double
infectivity = 0.05

initialInfectionProb :: Double
initialInfectionProb = 0.2
------------------------------------------------------------------------------------------------------------------------