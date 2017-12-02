{-# LANGUAGE Arrows #-}
module SocialForce.Person (
    createPerson
  ) where

import Control.Monad.Random
import Control.Monad.Trans.State
import qualified Data.Map.Strict as Map
import Data.Maybe

import FRP.FrABS
import FRP.Yampa

import SocialForce.Model
import SocialForce.Markup

-------------------------------------------------------------------------------
-- STATECHART
goingToEntranceState :: SocialForceAgentBehaviour
goingToEntranceState = transitionOnEvent 
                        isAtDest
                        goingToEntrance
                        movingState

goingToEntrance :: SocialForceAgentBehaviour
goingToEntrance = doOnceR $ ignoreEnv $ proc ain -> do
  let ao = agentOutFromIn ain
  let ao' = updateAgentState (\s -> s { perState = GoingToEntrance, perDest = (perEntry s) }) ao
  returnA -< ao

movingState :: SocialForceAgentBehaviour
movingState = transitionOnEvent 
                isAtDest
                moving
                holdingState

moving :: SocialForceAgentBehaviour
moving = proc (ain, e) -> do
  let ao = agentOutFromIn ain
  let ao' = setState Moving ao
  returnA -< (ao', e)

holdingState :: SocialForceAgentBehaviour
holdingState = undefined

holding :: SocialForceAgentBehaviour
holding = proc (ain, e) -> do
  let ao = agentOutFromIn ain
  let ao' = setState Holding ao
  returnA -< (ao', e)

isAtDest :: SocialForceEventSource
isAtDest = proc (ain, ao) -> do
  let s = agentState ao
  arrivedDestEvt <- edge -< perArrivedDest s
  returnA -< (ao, arrivedDestEvt)

setState :: PersonState -> SocialForceAgentOut -> SocialForceAgentOut
setState state = updateAgentState (\s -> s { perState = state })
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- UPDATING
personUpdate :: SocialForceAgentMonadicBehaviour
personUpdate e t ain = do
  checkDestination
  
  calculatePeople e
  calculateWalls e

  checkInjured

  updateSpeed
  updateHeading
  updatePosition e

checkDestination :: State SocialForceAgentOut ()
checkDestination = do
  arrivedDest <- agentStateFieldM perArrivedDest
  (destX, destY) <- agentStateFieldM perDest
  (x, y) <- agentStateFieldM perPos

  unless
    (arrivedDest)
    (when 
      ((destX - 1 < x && x < destX + 1) && (destY - 1 < y && y < destY + 1))
      (updateAgentStateM (\s -> s { perArrivedDest = True })))

calculatePeople :: SocialForceEnvironment -> State SocialForceAgentOut ()
calculatePeople e = do
    s <- agentStateM 
    aid <- agentIdM
    let ps = Map.assocs $ sfEnvPeos e
    let (sumFijH', sumFijV') = foldr (calcPerson aid s) (0,0) ps
    updateAgentStateM (\s -> s { perSumFijH = sumFijH', perSumFijV = sumFijV' })
  where
    calcPerson :: AgentId
                  -> SocialForceAgentState
                  -> (AgentId, PersonEnvObs)
                  -> (Double, Double) 
                  -> (Double, Double)
    calcPerson aid s (peoId, p) acc
      | aid == peoId = acc -- ignore self
      | otherwise = do
        let (px, py) = peoPos p
        let (x, y) = perPos s
        let connRange = perConRange s

        let dij = distanceEuclideanCont2d (px, py) (x, y)
        if dij > connRange
          then acc
          else calcPersonAux s p dij acc

      where
          calcPersonAux :: SocialForceAgentState
                            -> PersonEnvObs
                            -> Double
                            -> (Double, Double) 
                            -> (Double, Double)
          calcPersonAux s p dij (sumFijH, sumFijV) = do
            let (px, py) = peoPos p
            let (pSpeedX, pSpeedY) = peoSpeed p
            let (speedX, speedY) = perSpeed s
            let pRi = peoRi p

            let (x, y) = perPos s
            let ri = perRi s
            let ai = perAi s
            let aiGrp = perAiGrp s
            let bi = perBi s
            let applyPsy = perApplyPsy s
            let kCap = perK s
            let k = perk s
            let attentionAngle = perAttAngle s

            let i = if sameGroup s p then -1 else 1

            let theta = (atan2 (py - y) (px - x)) - (atan2 speedY speedX)
            let cosTheta = if (theta<((-attentionAngle)/2) || theta>(attentionAngle/2)) && enableVisionArea
                            then 0
                            else 1

            let rij = ri + pRi
            let nij1 = if x == px then 0 else (x - px) / dij
            let nij2 = if y == py then 0 else (y - py) / dij
            let tij1 = -nij2
            let tij2 = nij1
            let gx = if dij > rij then 0 else rij - dij
            let fpsy = ai * (exp $ (rij - dij) / bi) * cosTheta

            let fpsy' = if i == -1 
                          then
                            if applyPsy 
                              then (-aiGrp) * (exp (dij - rij) / 2) * cosTheta
                              else 0
                          else
                            fpsy

            let fbody = kCap * gx

            let deltavH = pSpeedX - speedX
            let deltavV = pSpeedY - speedY
            let deltav = deltavH * tij1 + deltavV * tij2
            let friction = k * gx * deltav;
            let fijH = (fpsy+fbody)*nij1+friction*tij1;
            let fijV = (fpsy+fbody)*nij2+friction*tij2;

            (sumFijH + fijH, sumFijV + fijV) 

          sameGroup :: SocialForceAgentState -> PersonEnvObs -> Bool
          sameGroup s p 
              | isJust sg && isJust pg = (fromJust sg) == (fromJust pg)
              | otherwise = False
            where
              sg = perGroup s
              pg = peoGroup p

calculateWalls :: SocialForceEnvironment -> State SocialForceAgentOut ()
calculateWalls e = do
    s <- agentStateM 
    let ws = sfEnvWalls e
    let (sumFiWH', sumFiWV') = foldr (calcWall s) (0,0) ws
    let (sumFiWH'', sumFiWV'') = calcAdaptiveWall s (sumFiWH', sumFiWV')

    updateAgentStateM (\s -> s { perSumFiWH = sumFiWH', perSumFiWV = sumFiWV' })
  
  where
    calcWall :: SocialForceAgentState
                -> Line 
                -> (Double, Double) 
                -> (Double, Double)
    calcWall s w acc@(sumFiWH, sumFiWV) = do
        let pos = perPos s
        let connRange = perConRange s

        let (diW, np) = nearestPointLine w pos
        if diW > connRange
           then (sumFiWH, sumFiWV)
           else calcWallAux s (diW, np) acc

      where
        calcWallAux :: SocialForceAgentState 
                        -> (Double, Continuous2dCoord)
                        -> (Double, Double) 
                        -> (Double, Double) 
        calcWallAux s (diW, (nx, ny)) (sumFiWH, sumFiWV) = do
          let (x, y) = perPos s
          let (speedX, speedY) = perSpeed s
          let attentionAngle = perAttAngle s
          let ri = perRi s
          let aiWall = perAiWall s
          let bi = perBi s
          let kCap = perK s
          let k = perk s

          let theta = (atan2 (ny - y) (nx - x)) - (atan2 speedY speedX)
          let cosTheta = if (theta > (-attentionAngle) / 2) || (theta > (attentionAngle / 2)) then 0 else 1

          let niW1 = if x == nx then 0 else (x - nx) / diW
          let niW2 = if y == ny then 0 else (y - ny) / diW
          let tiW1 = -niW2
          let tiW2 = niW1

          let gx = if diW > ri then 0 else ri - diW
          let fpsy = aiWall * (exp (ri - diW / bi)) * cosTheta
          let fbody = kCap * gx
          let deltavH = -speedX
          let deltavV = -speedY
          let deltav = deltavH * tiW1 + deltavV * tiW2
          let friction = k * gx * deltav
          let fiWH = (fpsy + fbody) * niW1 + friction * tiW1
          let fiWV = (fpsy + fbody) * niW2 + friction * tiW2

          (sumFiWH + fiWH, sumFiWV + fiWV)

    calcAdaptiveWall :: SocialForceAgentState
                        -> (Double, Double) 
                        -> (Double, Double) 
    calcAdaptiveWall s (sumFiWH, sumFiWV) = (sumFiWH, sumFiWV) -- TODO: implement

checkInjured :: State SocialForceAgentOut ()
checkInjured = do
  sumFiWH <- agentStateFieldM perSumFiWH
  sumFiWV <- agentStateFieldM perSumFiWV
  sumFijH <- agentStateFieldM perSumFijH
  sumFijV <- agentStateFieldM perSumFijV

  let totalForce = sqrt((sumFijH ** 2) + (sumFijV ** 2))
                   + sqrt((sumFiWH ** 2) + (sumFiWV ** 2))

  ri <- agentStateFieldM perRi

  when
    (totalForce / 2 / pi / ri >= 16000) -- TODO: what is this magic number????
    (updateAgentStateM (\s -> s { 
          perInjured = True
        , perColor = blackColor
        , perVi0 = 0
        , perSpeed = (0, 0)}))

updateSpeed :: State SocialForceAgentOut ()
updateSpeed = do
    (x, y) <- agentStateFieldM perPos
    (destX, destY) <- agentStateFieldM perDest
    (speedX, speedY) <- agentStateFieldM perSpeed

    sumFiWH <- agentStateFieldM perSumFiWH
    sumFiWV <- agentStateFieldM perSumFiWV
    sumFijH <- agentStateFieldM perSumFijH
    sumFijV <- agentStateFieldM perSumFijV
    mi <- agentStateFieldM perMi
    vi0 <- agentStateFieldM perVi0

    let distToDest = distanceEuclideanCont2d (x, y) (destX, destY)

    let vi0Vert = if (destY == 0 || vi0 == 0) then 0 else (destY - y) * vi0 / distToDest
    let vi0Hori = if (destX == 0 || vi0 == 0) then 0 else (destX - x) * vi0 / distToDest
    
    let accVert = ((vi0Vert - speedY) * mi / 0.5 + sumFijV + sumFiWV) / mi
    let accHori = ((vi0Hori - speedX) * mi / 0.5 + sumFijH + sumFiWH) / mi
    
    let speedX' = speedX + accHori * unitTime
    let speedY' = speedY + accVert * unitTime

    updateAgentStateM (\s -> s { perSpeed = (speedX', speedY')})

updateHeading :: State SocialForceAgentOut ()
updateHeading = do
  (speedX, speedY) <- agentStateFieldM perSpeed

  updateAgentStateM (\s -> s { perHeading = (atan2 speedX speedY) + pi / 2})

updatePosition :: SocialForceEnvironment -> State SocialForceAgentOut SocialForceEnvironment
updatePosition e = do
  aid <- agentIdM
  (x, y) <- agentStateFieldM perPos
  (speedX, speedY) <- agentStateFieldM perSpeed
  ri <- agentStateFieldM perRi
  gr <- agentStateFieldM perGroup

  let x' = x + speedX * unitTime -- TODO: time-delta dependend value
  let y' = y + speedY * unitTime -- TODO: time-delta dependend value
  
  updateAgentStateM (\s -> s { perPos = (x', y')})

  let peo = PersonEnvObs {
      peoPos    = (x', y')
    , peoSpeed  = (speedX, speedY)
    , peoRi     = ri
    , peoGroup  = gr
  }

  return $ e { sfEnvPeos = Map.insert aid peo (sfEnvPeos e) }

inState :: PersonState -> State SocialForceAgentOut Bool
inState ps = do
  ps' <- agentStateFieldM perState
  return $ ps == ps'
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- REACTIVE BEHAVIOUR
-- TODO: do state-chart first and then do personupdate
personBehaviour :: SocialForceAgentBehaviour
personBehaviour = (agentMonadic personUpdate) -- doRepeatedlyEvery unitTime (agentMonadic personUpdate) 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- CREATION
createPerson :: AgentId
                -> Bool
                -> Continuous2dCoord 
                -> Continuous2dCoord 
                -> Rand StdGen SocialForceAgentDef
createPerson aid top start entry = do
  rng <- getSplit

  readingTimeRand <- getRandomR (10, 60)
  riRand <- getRandomR (0.15, 0.25)

  let s = Person {
      perState        = GoingToEntrance
    , perTop          = top

    , perPos          = start
    , perDest         = (0, 0)
    , perSpeed        = (0, 0)
    , perHeading      = 0

    , perEntry        = entry

    , perAttAngle     = 5*pi/6
    , perConRange     = 10

    , perColor        = whiteColor
    , perGroup        = Nothing
    , perReadingTime  = readingTimeRand
    , perInjured      = False
    , perArrivedDest  = False
    
    , perAiWall       = 2*100
    , perAiGrp        = 5
    , perBi           = 0.2
    , perAi           = 2*100
    , perK            = 1.2*100000
    , perk            = 2.4*100000
    , perRi           = riRand
    , perVi0          = vi0Init
    , perMi           = 80

    , perApplyPsy     = True

    , perSumFiWH      = 0
    , perSumFiWV      = 0
    , perSumFijH      = 0
    , perSumFijV      = 0
  }

  return AgentDef {
    adId = aid,
    adState = s,
    adConversation = Nothing,
    adInitMessages = NoEvent,
    adBeh = personBehaviour,
    adRng = rng 
  }
-------------------------------------------------------------------------------