module Model 
  (
    SIRState (..)
  , FrSIRMsg (..)

  , FrSIRAgentState
  , FrSIREnvironment

  , FrSIRAgentDef
  , FrSIRAgentBehaviour
  , FrSIRAgentBehaviourReadEnv
  , FrSIRAgentBehaviourIgnoreEnv
  , FrSIRAgentIn
  , FrSIRAgentOut
  , FrSIRAgentObservable
  
  , FrSIREventSource
  , FrSIRReplicationConfig
  , FrSIRAgentDefReplicator
  
  , infectivity
  , contactRate
  , illnessDuration

  , contactSS
  , illnessTimeoutSS
  ) where

import FRP.Chimera

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SIRState = Susceptible | Infected | Recovered deriving (Eq, Show)
data FrSIRMsg = Contact SIRState deriving (Eq, Show)

type FrSIRAgentState = SIRState

-- NOTE: here we are not interested in the network and their influences, instead we go for a fully-connected network
-- We could implement this using Network () as the environment type but the underlying graph-library (FGL) cannot
-- deal with big (>10.000 nodes) complete networks as it sucks up massive memory. 
type FrSIREnvironment = [AgentId]

type FrSIRAgentDef                = AgentDef FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRAgentBehaviour          = AgentBehaviour FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRAgentBehaviourReadEnv   = ReactiveBehaviourReadEnv FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRAgentBehaviourIgnoreEnv = ReactiveBehaviourIgnoreEnv FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRAgentIn                 = AgentIn FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRAgentOut                = AgentOut FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRAgentObservable         = AgentObservable FrSIRAgentState

type FrSIREventSource         = EventSource FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRReplicationConfig   = ReplicationConfig FrSIRAgentState FrSIRMsg FrSIREnvironment
type FrSIRAgentDefReplicator  = AgentDefReplicator FrSIRAgentState FrSIRMsg FrSIREnvironment
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

-- number of super-samples for contact-rate: because of high contact rate per time-unit we need an even higher number of samples
contactSS :: Int
contactSS = 1 -- 20

-- number of super-samples for illness duration time-out: because the duration is quite long on average we can sample it with low frequency (low number of samples)
illnessTimeoutSS :: Int
illnessTimeoutSS = 1 -- 2
------------------------------------------------------------------------------------------------------------------------