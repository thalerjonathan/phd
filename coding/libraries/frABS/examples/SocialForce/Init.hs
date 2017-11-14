module SocialForce.Init (
    initSocialForce
  ) where

import Control.Monad.Random

import FRP.FrABS
import FRP.Yampa

import SocialForce.Model
import SocialForce.Markup
import SocialForce.Museum

initSocialForce :: Rand StdGen ([SocialForceAgentDef], SocialForceEnvironment)
initSocialForce = do
  let env = initEnvironment
  adefs <- initAgents

  return (adefs, env)
  
initAgents :: Rand StdGen [SocialForceAgentDef]
initAgents = do
  museum <- initMuseum
  return [museum]
  
initMuseum :: Rand StdGen SocialForceAgentDef
initMuseum = do
  rng <- getSplit

  let s = Museum {
      musStartPoint = (170, 520)
    , musGroupPoint0 = (210, 510)
    , musGroupPoints = [(200, 540), (130, 510), (140, 540)]
  }

  return AgentDef {
    adId = museumId,
    adState = s,
    adConversation = Nothing,
    adInitMessages = NoEvent,
    adBeh = museumBehaviour rng,
    adRng = rng 
  }

initEnvironment :: SocialForceEnvironment
initEnvironment = 
    SocialForceEnvironment {
      sfEnvWalls = ws
    } 
  where
    ws = initWalls

initWalls :: [Wall]
initWalls = [ wall0, wall1, wall2, wall3, wall4, wall5, wall6, wall7, wall8, wall9, wall10, room1Display, room2Display ]
  where
    wall0 = wall (50, 450) [(100, 0)]
    wall1 = wall (205, 450) [(245, 0)]
    wall2 = wall (599, 450) [(0, -400)]
    wall3 = wall (50, 50) [(60, 0), (400, 0)]
    wall4 = wall (50, 50) [(0, 400)]
    wall5 = wall (506, 50) [(93, 0)]
    wall6 = wall (450, 450) [(149, 0)]
    wall7 = wall (600, 400) [(-19, 0)]
    wall8 = wall (70, 80) [(-19, 0)]
    wall9 = wall (70, 400) [(-19, 0)]
    wall10 = wall (599, 80) [(-19, 0)]
    room1Display = wall (70, 80) [(0, 320)]
    room2Display = wall (579, 80) [(0, 320)]