module SocialForce.Model (
      SocialForceMsg (..)
    , SocialForceEnvironment (..)
    , SocialForceAgentState (..)

    , SocialForceAgentDef
    , SocialForceAgentBehaviour
    , SocialForceAgentIn
    , SocialForceAgentOut
    , SocialForceAgentObservable
  ) where

import FRP.FrABS

import SocialForce.Markup

data SocialForceMsg = SocialForceMsg

data SocialForceEnvironment = SocialForceEnvironment
  {
      sfEnvWalls :: [Wall]
  } deriving Show

data SocialForceAgentState = SocialForceAgentState deriving Show

type SocialForceAgentDef = AgentDef SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentBehaviour = AgentBehaviour SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentIn = AgentIn SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentOut = AgentOut SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentObservable = AgentObservable SocialForceAgentState