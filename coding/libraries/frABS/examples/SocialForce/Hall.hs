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

spawnVisitors :: RandomGen g => g -> Bool -> SocialForceAgentBehaviour
spawnVisitors g top = proc (ain, e) -> do
  r <- noiseR (0, 1) g -< ()
  if r <= groupSpawningProb then
    returnA $ (agentMonadicIgnoreEnv $ spawnGroup top) -< (ain, e)
    else
      returnA $ (agentMonadicIgnoreEnv $ spawnPerson top) -< (ain, e)

spawnPerson :: Bool -> SocialForceAgentMonadicBehaviourNoEnv
spawnPerson top _ ain = initPerson top start ain >>= (\p -> createAgentM p)
  where
    start = head $ startPoints top

spawnGroup :: Bool -> SocialForceAgentMonadicBehaviourNoEnv
spawnGroup top _ ain = do
  gr <- createGroup ain
  let (start0 : start1 : starts) = startPoints top

  pd0 <- initPerson top start0 ain
  pd1 <- initPerson top start1 ain

  pds <- foldM
    (\pdsAcc pos -> 
      ifThenElseM
        (agentRandomM $ randomBoolM 0.6)
        (do 
          pd <- initPerson top pos ain
          ri <- agentRandomRangeM (0.11, 0.25)
          vi0 <- agentRandomRangeM (1.0, 1.4) 
          let pd' = pd { adState = (adState pd) { perRi = ri, perVi0 = vi0 }}
          return $ pd' : pdsAcc)
        (return pdsAcc))
    []
    starts

  let pds' = pd0 : pd1 : pds

  mapM_ (\pd -> do
    let pd' = setPersonInGroup gr pd
    createAgentM pd')
    pds'

  let ais = map adId pds'
  let gr' = gr { grpPersons = ais }

  updateAgentStateM (\s -> s { hallGroups = gr' : (hallGroups s) })
  
initPerson :: Bool -> Continuous2dCoord -> SocialForceAgentIn -> State SocialForceAgentOut SocialForceAgentDef
initPerson top start ain = do
  let personId = nextAgentId ain
  let entry = entrance top
  agentRandomM (createPerson personId top start entry)

hallBehaviour :: RandomGen g => g -> SocialForceAgentBehaviour
hallBehaviour g = doOccasionallyEvery g enterSpeed (spawnVisitors g True) -- TODO: also do bottom

createGroup :: SocialForceAgentIn -> State SocialForceAgentOut Group
createGroup ain = do
  let groupId = nextAgentId ain
  gc <- agentRandomM randomColor
  rt <- agentRandomRangeM (10, 60) 

  return Group {
      grpId           = groupId
    , grpDest         = (0, 0)
    , grpReadingTime  = rt
    , grpModified     = False
    , grpExit         = False
    , grpColor        = gc
    , grpPersons      = []
  }

setPersonInGroup :: Group -> SocialForceAgentDef -> SocialForceAgentDef
setPersonInGroup g pd = pd { adState = (adState pd) { perColor = grpColor g, perGroup = Just $ grpId g, perReadingTime = grpReadingTime g }}