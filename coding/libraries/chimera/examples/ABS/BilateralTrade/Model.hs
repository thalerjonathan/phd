module BilateralTrade.Model (
      BTMsg
    , BTAgentState (..)
    , BTEnvironment

    , BTAgentDef
    , BTAgentBehaviour
    , BTAgentIn
    , BTAgentOut
    , BTAgentObservable

    , agents
    , goods
  ) where

import FRP.FrABS

-------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
-------------------------------------------------------------------------------
data BTMsg = BTMsgData deriving (Eq, Show)
data BTAgentState = BTAgentState 
  {
      prices    :: [Double]   -- price for each good, initially drawn uniform from 0,1
    , inventory :: [Double]   -- inventory for goods
    , score     :: Double     -- 
  } deriving (Eq, Show)

type BTEnvironment = ()

type BTAgentDef = AgentDef BTAgentState BTMsg BTEnvironment
type BTAgentBehaviour = AgentBehaviour BTAgentState BTMsg BTEnvironment
type BTAgentIn = AgentIn BTAgentState BTMsg BTEnvironment
type BTAgentOut = AgentOut BTAgentState BTMsg BTEnvironment
type BTAgentObservable = AgentObservable BTAgentState
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Model parameters
-------------------------------------------------------------------------------
-- Number of agents
agents :: Int
agents = 100

-- Number of goods
goods :: Int
goods = 10
