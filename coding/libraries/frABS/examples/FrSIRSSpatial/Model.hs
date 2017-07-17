module FrSIRSSpatial.Model (
    SIRSState (..),
    FrSIRSSpatialMsg (..),

    FrSIRSSpatialAgentState (..),
    FrSIRSSpatialEnvironment,

    FrSIRSSpatialAgentDef,
    FrSIRSSpatialAgentBehaviour,
    FrSIRSSpatialAgentIn,
    FrSIRSSpatialAgentOut,

    FrSIRSSpatialEventSource,
    FrSIRSSpatialMessageSource,
    
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

data FrSIRSSpatialAgentState = FrSIRSSpatialAgentState {
    sirsState :: SIRSState,
    sirsCoord :: Discrete2dCoord
}

type FrSIRSSpatialEnvironment = Discrete2d AgentId

type FrSIRSSpatialAgentDef = AgentDef FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
type FrSIRSSpatialAgentBehaviour = AgentBehaviour FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
type FrSIRSSpatialAgentIn = AgentIn FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
type FrSIRSSpatialAgentOut = AgentOut FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment

type FrSIRSSpatialEventSource = EventSource FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
type FrSIRSSpatialMessageSource = MessageSource FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
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