module FrSIRSNetwork.Model (
    SIRSState (..),
    FrSIRSNetworkMsg (..),

    FrSIRSNetworkAgentState,
    FrSIRSNetworkEnvironment,

    FrSIRSNetworkAgentDef,
    FrSIRSNetworkAgentBehaviour,
    FrSIRSNetworkAgentBehaviourReadEnv,
    FrSIRSNetworkAgentBehaviourIgnoreEnv,
    FrSIRSNetworkAgentIn,
    FrSIRSNetworkAgentOut,
    FrSIRSNetworkAgentObservable,
    
    FrSIRSNetworkEventSource,
    FrSIRSNetworkMessageSource,
    
    infectivity,
    contactRate,
    illnessDuration,
    immuneDuration,
    initialInfectionProb,

    contactSS,
    illnessTimeoutSS,
    immuneTimeoutSS
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
type FrSIRSNetworkAgentBehaviourReadEnv = ReactiveBehaviourReadEnv FrSIRSNetworkAgentState FrSIRSNetworkMsg FrSIRSNetworkEnvironment
type FrSIRSNetworkAgentBehaviourIgnoreEnv = ReactiveBehaviourIgnoreEnv FrSIRSNetworkAgentState FrSIRSNetworkMsg FrSIRSNetworkEnvironment
type FrSIRSNetworkAgentIn = AgentIn FrSIRSNetworkAgentState FrSIRSNetworkMsg FrSIRSNetworkEnvironment
type FrSIRSNetworkAgentOut = AgentOut FrSIRSNetworkAgentState FrSIRSNetworkMsg FrSIRSNetworkEnvironment
type FrSIRSNetworkAgentObservable = AgentObservable FrSIRSNetworkAgentState

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
contactRate = 5

illnessDuration :: Double
illnessDuration = 15

immuneDuration :: Double
immuneDuration = 3000.0

initialInfectionProb :: Double
initialInfectionProb = 0.2

-- number of super-samples for contact-rate: because of high contact rate per time-unit we need an even higher number of samples
contactSS :: Int
contactSS = 20

-- number of super-samples for illness duration time-out: because the duration is quite long on average we can sample it with low frequency (low number of samples)
illnessTimeoutSS :: Int
illnessTimeoutSS = 2

-- number of super-samples for immune duration time-out: because the duration is quite long on average we can sample it with low frequency (low number of samples)
immuneTimeoutSS :: Int
immuneTimeoutSS = 2
------------------------------------------------------------------------------------------------------------------------