module SocialForce.Init (
    initSocialForce
  ) where

import Control.Monad.Random
import qualified Data.Map.Strict as Map

import FRP.FrABS
import FRP.Yampa

import SocialForce.Model
import SocialForce.Markup
import SocialForce.Hall

initSocialForce :: SocialForceSimulationParams -> Rand StdGen ([SocialForceAgentDef], SocialForceEnvironment)
initSocialForce params = do
  let env = initEnvironment
  adefs <- initAgents params

  return (adefs, env)
  
initAgents :: SocialForceSimulationParams -> Rand StdGen [SocialForceAgentDef]
initAgents params = do
  hall <- initHall params
  return [hall]
  
initHall :: SocialForceSimulationParams -> Rand StdGen SocialForceAgentDef
initHall params = do
  let aid = newAgentId params
  rng <- getSplit

  let s = Hall {
    hallGroups = []
  }

  return AgentDef {
    adId = aid,
    adState = s,
    adConversation = Nothing,
    adInitMessages = NoEvent,
    adBeh = hallBehaviour rng,
    adRng = rng 
  }

initEnvironment :: SocialForceEnvironment
initEnvironment = 
    SocialForceEnvironment {
        sfEnvWalls = ws
      , sfEnvMovingArea = ma
      , sfEnvPeos = Map.empty
    } 
  where
    ws = initWalls
    ma = ((2.8, 5.2), 10.4, 11.6)

initWalls :: [Line]
initWalls = [ wall0, wall1, wall2, wall3, wall4, wall5, wall6, wall7, wall8, wall9, wall10]
  where
    wall0 = line (2, 4) (2, 18)
    wall1 = line (2, 18) (2.8, 18)

    wall2 = line (4.8, 18) (11.2, 18)
    wall3 = line (14, 18) (13.2, 18)
    wall4 = line (14, 18) (14, 4)
    wall5 = line (14, 4) (14, 3.2)
    wall6 = line (4.8, 4) (11.2, 4)
    
    -- Square Pillar
    wall7 = line (7.2, 7.2) (7.2, 8.8)
    wall8 = line (7.2, 8.8) (8.8, 8.8)
    wall9 = line (8.8, 8.8) (8.8, 7.2)
    wall10 = line (8.8, 7.2) (7.2, 7.2)
