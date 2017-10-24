module FrSIRSSpatial.Model (
    SIRSState (..),
    FrSIRSSpatialMsg (..),

    FrSIRSSpatialAgentState (..),
    FrSIRSSpatialEnvironment,

    FrSIRSSpatialAgentDef,
    FrSIRSSpatialAgentBehaviour,
    FrSIRSSpatialAgentBehaviourReadEnv,
    FrSIRSSpatialAgentBehaviourIgnoreEnv,
    FrSIRSSpatialAgentIn,
    FrSIRSSpatialAgentOut,
    FrSIRSSpatialAgentObservable,
    
    FrSIRSSpatialEventSource,
    FrSIRSSpatialMessageSource,
    
    illnessDuration,
    immuneDuration,
    contactRate,
    infectivity,
    initialInfectionProb,

    contactSS,
    illnessTimeoutSS,
    immuneTimeoutSS
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SIRSState = Susceptible | Infected | Recovered deriving (Eq, Show, Read)
data FrSIRSSpatialMsg = Contact SIRSState deriving (Eq, Show, Read)

data FrSIRSSpatialAgentState = FrSIRSSpatialAgentState {
    sirsState :: SIRSState,
    sirsCoord :: Discrete2dCoord
} deriving (Show, Read)

type FrSIRSSpatialEnvironment = Discrete2d AgentId

type FrSIRSSpatialAgentDef = AgentDef FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
type FrSIRSSpatialAgentBehaviour = AgentBehaviour FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
type FrSIRSSpatialAgentBehaviourReadEnv = ReactiveBehaviourReadEnv FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
type FrSIRSSpatialAgentBehaviourIgnoreEnv = ReactiveBehaviourIgnoreEnv FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
type FrSIRSSpatialAgentIn = AgentIn FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
type FrSIRSSpatialAgentOut = AgentOut FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
type FrSIRSSpatialAgentObservable = AgentObservable FrSIRSSpatialAgentState

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
contactRate = 5

-- average probability of getting infected 
infectivity :: Double
infectivity = 0.05

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