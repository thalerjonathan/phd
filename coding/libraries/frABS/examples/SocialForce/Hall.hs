{-# LANGUAGE Arrows #-}
module SocialForce.Hall (
    hallBehaviour
  ) where

import Control.Monad
import Control.Monad.IfElse
import Control.Monad.Random
import Control.Monad.Trans.State

import FRP.FrABS
import FRP.Yampa

import SocialForce.Model
import SocialForce.Person

spawnVisitors :: RandomGen g => g -> SocialForceAgentBehaviour
spawnVisitors g = proc (ain, e) -> do
  r <- noiseR (0, 1) g -< ()
  if r <= groupSpawningProb then
    returnA $ (agentMonadicIgnoreEnv spawnGroup) -< (ain, e)
    else
      returnA $ (agentMonadicIgnoreEnv spawnPerson) -< (ain, e)

spawnPerson :: SocialForceAgentMonadicBehaviourNoEnv
spawnPerson _ ain = initPerson ain >>= (\p -> createAgentM p)

spawnGroup :: SocialForceAgentMonadicBehaviourNoEnv
spawnGroup _ ain = do
  s <- agentStateM
  groupColor <- agentRandomM randomColor
  let groupId = nextAgentId ain

  p0 <- initPerson ain
  p1 <- initPerson ain

  let p0' = p0 { adState = (adState p0) { perColor = groupColor, perBelGroup = Just groupId, perDestScreen = tdest } }
  let p1' = p1 { adState = (adState p1) { perPos = musGroupPoint0 s, perColor = groupColor, perBelGroup = Just groupId, perDestScreen = tdest } }

  createAgentM p0'
  createAgentM p1'

  mapM_ 
    (\pos -> 
      whenM
        (agentRandomM (randomBoolM 0.6))
        (do
          p <- initPerson ain
          let p' = p { adState = (adState p) { perPos = pos, perColor = groupColor, perBelGroup = Just groupId, perDestScreen = tdest } }
          createAgentM p'))
    (musGroupPoints s)

initPerson :: SocialForceAgentIn -> State SocialForceAgentOut SocialForceAgentDef
initPerson ain = do
  s <- agentStateM
  let personId = nextAgentId ain
  let personPos = musStartPoint s
  agentRandomM (createPerson personId personPos)

hallBehaviour :: RandomGen g => g -> SocialForceAgentBehaviour
hallBehaviour g = doOccasionallyEvery g enterSpeed (spawnVisitors g)