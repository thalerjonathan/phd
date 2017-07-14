module Wildfire.Model (
    WildfireMsg (..),
    WildfireAgentState (..),
    LifeState (..),

    WildfireLinkLabel,
    WildfireCell,
    WildfireEnvironment,
    WildfireEnvironmentBehaviour,

    WildfireAgentDef,
    WildfireAgentBehaviour,
    WildfireAgentIn,
    WildfireAgentOut,

    WildfireEventSource,

    ignitions,
    randomFuelInitRange,
    randomFuelRateRange
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data WildfireMsg =
    Ignite 
    deriving (Eq, Show)

data WildfireAgentState = WildfireAgentState {
    wfLifeState :: LifeState,
    wfFuelCurr :: Double,
    wfFuelRate :: Double
} deriving (Show)

data LifeState = Living | Burning | Dead deriving (Eq, Show)

type WildfireLinkLabel = ()
type WildfireCell = AgentId
type WildfireEnvironment = Environment WildfireCell WildfireLinkLabel
type WildfireEnvironmentBehaviour = EnvironmentBehaviour WildfireCell WildfireLinkLabel

type WildfireAgentDef = AgentDef WildfireAgentState WildfireMsg WildfireCell WildfireLinkLabel
type WildfireAgentBehaviour = AgentBehaviour WildfireAgentState WildfireMsg WildfireCell WildfireLinkLabel
type WildfireAgentIn = AgentIn WildfireAgentState WildfireMsg WildfireCell WildfireLinkLabel
type WildfireAgentOut = AgentOut WildfireAgentState WildfireMsg WildfireCell WildfireLinkLabel

type WildfireEventSource = EventSource WildfireAgentState WildfireMsg WildfireCell WildfireLinkLabel
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