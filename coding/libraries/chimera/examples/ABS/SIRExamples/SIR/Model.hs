module Model 
  (
    SIRState (..)
  , SIRMsg (..)
  , SIRAgentState (..)

  , SIREnvironment

  , SIRAgentDef
  , SIRAgentBehaviour
  , SIRAgentIn
  , SIRAgentOut
  , SIRAgentObservable
  
  , SIRAgentPureReadEnvBehaviour
  , SIRAgentMonadicReadEnvBehaviour
  
  , contactRate
  , illnessDuration
  , infectionProbability
  ) where

import FRP.Yampa

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SIRState   = Susceptible | Infected | Recovered deriving (Eq, Show)
data SIRMsg     = Contact SIRState deriving (Eq, Show)

data SIRAgentState = SIRAgentState 
  {
    sirState      :: SIRState
  , sirStateTime  :: Double
  } deriving (Show)

type SIREnvironment = [AgentId]

type SIRAgentDef        = AgentDef SIRAgentState SIRMsg SIREnvironment
type SIRAgentBehaviour  = AgentBehaviour SIRAgentState SIRMsg SIREnvironment
type SIRAgentIn         = AgentIn SIRAgentState SIRMsg SIREnvironment
type SIRAgentOut        = AgentOut SIRAgentState SIRMsg SIREnvironment
type SIRAgentObservable = AgentObservable SIRAgentState

type SIRAgentPureReadEnvBehaviour    = AgentPureBehaviourReadEnv SIRAgentState SIRMsg SIREnvironment
type SIRAgentMonadicReadEnvBehaviour = AgentMonadicBehaviourReadEnv SIRAgentState SIRMsg SIREnvironment
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
contactRate :: Double
contactRate = 5

illnessDuration :: Double
illnessDuration = 15.0

infectionProbability :: Double
infectionProbability = 0.05
------------------------------------------------------------------------------------------------------------------------