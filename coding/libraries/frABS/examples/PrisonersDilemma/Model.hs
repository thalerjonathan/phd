module PrisonersDilemma.Model (
    Payoff,
    PDMsg (..),
    PDAgentState (..),
    PDAction (..),

    PDEnvironment,

    PDAgentDef,
    PDAgentBehaviour,
    PDAgentBehaviourIgnoreEnv,
    PDAgentIn,
    PDAgentOut,

    bParam,
    sParam,
    pParam,
    rParam,

    halfRoundTripTime
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
type Payoff = (PDAction, Double)
data PDMsg = NeighbourPayoff Payoff | NeighbourAction PDAction deriving (Eq, Show)

data PDAgentState = PDAgentState {
    pdCurrAction :: PDAction,
    pdPrevAction :: PDAction,
    
    pdLocalPo :: Double,

    pdBestPo :: Payoff,

    pdCoord :: Discrete2dCoord
} deriving (Show)

data PDAction = Defector | Cooperator deriving (Eq, Show)

type PDEnvironment = Discrete2d AgentId

type PDAgentDef = AgentDef PDAgentState PDMsg PDEnvironment
type PDAgentBehaviour = AgentBehaviour PDAgentState PDMsg PDEnvironment
type PDAgentBehaviourIgnoreEnv = ReactiveBehaviourIgnoreEnv PDAgentState PDMsg PDEnvironment
type PDAgentIn = AgentIn PDAgentState PDMsg PDEnvironment
type PDAgentOut = AgentOut PDAgentState PDMsg PDEnvironment
------------------------------------------------------------------------------------------------------------------------

halfRoundTripTime = 0.5

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
bParam :: Double
bParam = 1.95

sParam :: Double
sParam = 0.0

pParam :: Double
pParam = 0.0

rParam :: Double
rParam = 1.0
------------------------------------------------------------------------------------------------------------------------