module FrSIR.Model 
    (
      SIRState (..)
    , FrSIRMsg (..)

    , FrSIRAgentState
    , FrSIREnvironment

    , FrSIRAgentDef
    , FrSIRAgentBehaviour
    , FrSIRAgentIn
    , FrSIRAgentOut
    , FrSIRAgentObservable
    
    , FrSIREventSource
    , FrSIRMessageSource
    , FrSIRReplicationConfig
    
    , infectivity
    , contactRate
    , illnessDuration
    ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SIRState = Susceptible | Infected | Recovered deriving (Eq, Show)
data FrSIRMsg = Contact SIRState deriving (Eq, Show)

type FrSIRAgentState = SIRState

type FrSIREnvironment = Network ()

type FrSIRAgentDef = AgentDef FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRAgentBehaviour = AgentBehaviour FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRAgentIn = AgentIn FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRAgentOut = AgentOut FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRAgentObservable = AgentObservable FrSIRAgentState

type FrSIREventSource = EventSource FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRMessageSource = MessageSource FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRReplicationConfig = ReplicationConfig FrSIRAgentState FrSIRMsg FrSIREnvironment
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
-- average probability of getting infected 
infectivity :: Double
infectivity = 0.05

-- average number of contacts per time-unit
contactRate :: Double
contactRate = 5

-- average duration of illnes in time-units
illnessDuration :: Double
illnessDuration = 15
------------------------------------------------------------------------------------------------------------------------