module HeroesCowards.Model (
    HACRole (..),
    HACMsg (..),
    HACAgentState (..),
    
    HACEnvironment,

    HACAgentDef,
    HACAgentBehaviour,
    HACAgentIn,
    HACAgentOut,

    HACAgentBehaviourReadEnv,
    
    stepWidthPerTimeUnit
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data HACRole = Hero | Coward deriving (Eq, Show)
data HACMsg = PositionRequest | PositionUpdate Continuous2dCoord deriving (Eq, Show)

data HACAgentState = HACAgentState {
    hacRole :: HACRole,
    hacCoord :: Continuous2dCoord,

    hacFriendCoord :: Continuous2dCoord,
    hacEnemyCoord :: Continuous2dCoord,

    hacFriend :: AgentId,
    hacEnemy :: AgentId
}

type HACEnvironment = Continuous2dEmpty

type HACAgentDef = AgentDef HACAgentState HACMsg HACEnvironment
type HACAgentBehaviour = AgentBehaviour HACAgentState HACMsg HACEnvironment
type HACAgentIn = AgentIn HACAgentState HACMsg HACEnvironment
type HACAgentOut = AgentOut HACAgentState HACMsg HACEnvironment 

type HACAgentBehaviourReadEnv = AgentMonadicBehaviourReadEnv HACAgentState HACMsg HACEnvironment 
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
stepWidthPerTimeUnit :: Double
stepWidthPerTimeUnit = 0.01
------------------------------------------------------------------------------------------------------------------------