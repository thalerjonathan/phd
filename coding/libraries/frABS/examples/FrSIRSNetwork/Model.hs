module FrSIRSNetwork.Model (
    SIRSState (..),
    FrSIRSNetworkMsg (..),

    FrSIRSNetworkAgentState,
    FrSIRSNetworkEnvironment,

    FrSIRSNetworkAgentDef,
    FrSIRSNetworkAgentBehaviour,
    FrSIRSNetworkAgentIn,
    FrSIRSNetworkAgentOut,

    FrSIRSNetworkEventSource,
    FrSIRSNetworkMessageSource,
    
    infectivity,
    contactRate,
    illnessDuration,
    immuneDuration,
    initialInfectionProb
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SIRSState = Susceptible | Infected | Recovered deriving (Eq, Show)
data FrSIRSNetworkMsg = Contact SIRSState deriving (Eq, Show)

type FrSIRSNetworkAgentState = SIRSState

type FrSIRSNetworkEnvironment = Network ()

type FrSIRSNetworkAgentDef = AgentDef FrSIRSNetworkAgentState FrSIRSNetworkMsg FrSIRSNetworkEnvironment
type FrSIRSNetworkAgentBehaviour = AgentBehaviour FrSIRSNetworkAgentState FrSIRSNetworkMsg FrSIRSNetworkEnvironment
type FrSIRSNetworkAgentIn = AgentIn FrSIRSNetworkAgentState FrSIRSNetworkMsg FrSIRSNetworkEnvironment
type FrSIRSNetworkAgentOut = AgentOut FrSIRSNetworkAgentState FrSIRSNetworkMsg FrSIRSNetworkEnvironment

type FrSIRSNetworkEventSource = EventSource FrSIRSNetworkAgentState FrSIRSNetworkMsg FrSIRSNetworkEnvironment
type FrSIRSNetworkMessageSource = MessageSource FrSIRSNetworkAgentState FrSIRSNetworkMsg FrSIRSNetworkEnvironment
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
-- average probability of getting infected 
infectivity :: Double
infectivity = 0.05

-- average number of contacts per time-unit
contactRate :: Double
contactRate = 7

illnessDuration :: Double
illnessDuration = 15

immuneDuration :: Double
immuneDuration = 3.0

initialInfectionProb :: Double
initialInfectionProb = 0.2
------------------------------------------------------------------------------------------------------------------------