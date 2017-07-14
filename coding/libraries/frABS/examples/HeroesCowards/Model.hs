module HeroesCowards.Model (
    HACRole (..),
    HACMsg (..),
    HACAgentState (..),

    ContPosition,

    HACEnvLink,
    HACEnvCell,
    HACEnvironment,

    HACAgentDef,
    HACAgentBehaviour,
    HACAgentIn,
    HACAgentOut,

    stepWidthPerTimeUnit
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data HACRole = Hero | Coward deriving (Eq, Show)
type ContPosition = (Double, Double)
data HACMsg = PositionRequest | PositionUpdate ContPosition deriving (Eq, Show)

data HACAgentState = HACAgentState {
    hacRole :: HACRole,
    hacPos :: ContPosition,
    hacFriendPos :: ContPosition,
    hacEnemyPos :: ContPosition,
    hacFriend :: AgentId,
    hacEnemy :: AgentId
}

type HACEnvLink = ()
type HACEnvCell = ()
type HACEnvironment = Environment HACEnvCell HACEnvLink

type HACAgentDef = AgentDef HACAgentState HACMsg HACEnvCell HACEnvLink
type HACAgentBehaviour = AgentBehaviour HACAgentState HACMsg HACEnvCell HACEnvLink
type HACAgentIn = AgentIn HACAgentState HACMsg HACEnvCell HACEnvLink
type HACAgentOut = AgentOut HACAgentState HACMsg HACEnvCell HACEnvLink
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
stepWidthPerTimeUnit :: Double
stepWidthPerTimeUnit = 0.01
------------------------------------------------------------------------------------------------------------------------