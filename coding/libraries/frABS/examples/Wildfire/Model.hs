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
    
    createWildFireAgent
  ) where

import FRP.FrABS

import FRP.Yampa

import System.Random
import Control.Monad.Random

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data WildfireMsg =
    Ignite 
    deriving (Eq, Show)

data WildfireAgentState = WildfireAgentState {
    wfLifeState :: LifeState,
    wfFuel :: Double
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

------------------------------------------------------------------------------------------------------------------------
createWildFireAgent :: (AgentId, EnvCoord)
                        -> WildfireAgentBehaviour 
                        -> Bool
                        -> Rand StdGen WildfireAgentDef
createWildFireAgent (agentId, coord) beh initIgnite = 
    do
        rng <- getSplit

        let s = WildfireAgentState {
          wfLifeState = Living,
          wfFuel = 1.0
        }

        let initMessages = if initIgnite then Event [(0, Ignite)] else NoEvent

        let adef = AgentDef {
           adId = agentId,
           adState = s,
           adEnvPos = coord,
           adConversation = Nothing,
           adInitMessages = initMessages,
           adBeh = beh,
           adRng = rng }

        return adef