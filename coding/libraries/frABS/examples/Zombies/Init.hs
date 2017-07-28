module Zombies.Init (
    initZombies
  ) where

import           Zombies.Agent
import           Zombies.Environment
import           Zombies.Model

import           FRP.FrABS

import           FRP.Yampa

import           Control.Monad.Random

import Debug.Trace

initZombies :: IO ([ZombiesAgentDef], ZombiesEnvironment)
initZombies =
  do
    let dimsDisc2d@(dx,dy) = gridDimensions
    let dimsCont2d = disc2dToCont2d dimsDisc2d

    let coords = [ (x, y) | x <- [0..dx-1], y <- [0..dy-1] ]
    let cells = map (swap . ((,) 0)) coords

    humans <- mapM (createHuman dimsCont2d) [0..humanCount - 1]
    zombies <- mapM (createZombie dimsCont2d) [humanCount..humanCount + zombieCount - 1]

    netRng <- newStdGen
    discRng <- newStdGen

    let an = createEmptyNetwork netRng 
    let ap = createDiscrete2d
                        dimsDisc2d
                        moore
                        WrapBoth
                        cells
                        discRng

    let as = createContinuous2d dimsCont2d WrapBoth

    let ap' = updateEnvWithZombies zombies ap

    let e = ZombiesEnvironment {
      zAgentNetwork = an,
      zAgentSpace = as,
      zAgentPatches = ap'
    }

    return (humans ++ zombies, e)

  where
    swap (a,b) = (b,a)

updateEnvWithZombies :: [ZombiesAgentDef] -> ZombiesPatches -> ZombiesPatches
updateEnvWithZombies zombies e = foldr updateEnvWithZombiesAux e zombies
  where
    dims = envDisc2dDims e

    updateEnvWithZombiesAux :: ZombiesAgentDef -> ZombiesPatches -> ZombiesPatches
    updateEnvWithZombiesAux adef accEnv 
      | isHuman s = accEnv
      | otherwise = updateCellAt coordDisc2dClipped (+1) accEnv
      where
        s = adState adef
        coordCont2d = zAgentCoord s
        coordDisc2d = cont2dToDisc2d coordCont2d
        coordDisc2dClipped = wrapDisc2d dims ClipToMax coordDisc2d

createZombie :: Continuous2DDimension -> AgentId -> IO ZombiesAgentDef
createZombie dims@(dx, dy) aid =
  do
    rng <- newStdGen

    rx <- getRandomR (0, dx)
    ry <- getRandomR (0, dy)

    let s = ZombiesState {
      zAgentCoord = (rx, ry)
    }

    return AgentDef {
       adId = aid,
       adState = s,
       adConversation = Nothing,
       adInitMessages = NoEvent,
       adBeh = zombie,
       adRng = rng }

createHuman :: Continuous2DDimension -> AgentId -> IO ZombiesAgentDef
createHuman dims@(dx, dy) aid =
  do
    rng <- newStdGen

    rx <- getRandomR (0, dx)
    ry <- getRandomR (0, dy)
    re <- getRandomR humanInitEnergyRange

    let s = HumanState {
      zAgentCoord = (rx, ry),
      zHumanEnergy = re
    }

    return AgentDef {
       adId = aid,
       adState = s,
       adConversation = Nothing,
       adInitMessages = NoEvent,
       adBeh = human,
       adRng = rng }