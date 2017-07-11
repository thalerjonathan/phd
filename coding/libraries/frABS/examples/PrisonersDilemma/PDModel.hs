module PrisonersDilemma.PDModel (
    Payoff,
    PDMsg (..),
    PDAgentState (..),
    PDAction (..),

    PDCell,
    PDLinkLabel,
    PDEnvironment,
    PDEnvironmentBehaviour,

    PDAgentDef,
    PDAgentBehaviour,
    PDAgentIn,
    PDAgentOut,

    bParam,
    sParam,
    pParam,
    rParam,

    createPDAgent
  ) where

import FrABS.Agent.Agent
import FrABS.Env.Environment

import FRP.Yampa

import System.Random
import Control.Monad.Random
import Control.Monad
import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Map as Map

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
type Payoff = (PDAction, Double)
data PDMsg = NeighbourPayoff Payoff | NeighbourAction PDAction deriving (Eq, Show)

data PDAgentState = PDAgentState {
    pdCurrAction :: PDAction,
    pdPrevAction :: PDAction,
    
    pdLocalPo :: Double,

    pdBestPo :: Payoff
} deriving (Show)

data PDAction = Defector | Cooperator deriving (Eq, Show)

type PDCell = AgentId
type PDLinkLabel = ()

type PDEnvironment = Environment PDCell PDLinkLabel
type PDEnvironmentBehaviour = EnvironmentBehaviour PDCell PDLinkLabel

type PDAgentDef = AgentDef PDAgentState PDMsg PDCell PDLinkLabel
type PDAgentBehaviour = AgentBehaviour PDAgentState PDMsg PDCell PDLinkLabel
type PDAgentIn = AgentIn PDAgentState PDMsg PDCell PDLinkLabel
type PDAgentOut = AgentOut PDAgentState PDMsg PDCell PDLinkLabel
------------------------------------------------------------------------------------------------------------------------



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

createPDAgent :: (AgentId, EnvCoord)
                    -> PDAgentBehaviour
                    ->  PDAction
                    -> Rand StdGen PDAgentDef
createPDAgent (agentId, coord) beh a = 
    do
        rng <- getSplit

        let s = PDAgentState {
          pdCurrAction = a,
          pdPrevAction = a,
          
          pdLocalPo = 0.0,

          pdBestPo = (a, 0.0)
        }

        let adef = AgentDef {
           adId = agentId,
           adState = s,
           adEnvPos = coord,
           adConversation = Nothing,
           adInitMessages = NoEvent,
           adBeh = beh,
           adRng = rng }

        return adef