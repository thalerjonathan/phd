module Model 
  (
    WildfireMsg (..)
  , WildfireAgentState (..)
  , LifeState (..)

  , WildfireEnvironment

  , WildfireAgentDef
  , WildfireAgentBehaviour
  , WildfireAgentIn
  , WildfireAgentOut

  , WildfireEventSource

  , ignitions
  , randomFuelInitRange
  , randomFuelRateRange
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data WildfireMsg = Ignite deriving (Eq, Show)

data WildfireAgentState = WildfireAgentState 
  {
    wfLifeState   :: LifeState
  , wfFuelCurr    :: Double
  , wfFuelRate    :: Double
  , wfCoord       :: Discrete2dCoord
  } deriving (Show)

data LifeState = Living | Burning | Dead deriving (Eq, Show)

type WildfireEnvironment = Discrete2d AgentId

type WildfireAgentDef         = AgentDef WildfireAgentState WildfireMsg WildfireEnvironment
type WildfireAgentBehaviour   = AgentBehaviour WildfireAgentState WildfireMsg WildfireEnvironment
type WildfireAgentIn          = AgentIn WildfireAgentState WildfireMsg WildfireEnvironment
type WildfireAgentOut         = AgentOut WildfireAgentState WildfireMsg WildfireEnvironment

type WildfireEventSource      = EventSource WildfireAgentState WildfireMsg WildfireEnvironment
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
ignitions :: Double
ignitions = 5

randomFuelInitRange :: (Double, Double)
randomFuelInitRange = (0.7, 1.0)

randomFuelRateRange :: (Double, Double)
randomFuelRateRange = (1.0, 1.0)
------------------------------------------------------------------------------------------------------------------------