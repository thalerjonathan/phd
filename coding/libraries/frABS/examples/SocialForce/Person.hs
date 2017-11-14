module SocialForce.Person (
    createPerson
  ) where

import Control.Monad.Random
import Control.Monad.Trans.State

import FRP.FrABS
import FRP.Yampa

import SocialForce.Model

personStateChart :: SocialForceAgentBehaviour
personStateChart = undefined 

personUpdate :: SocialForceAgentMonadicBehaviour
personUpdate e t ain = do
  checkDestination
  
  calculatePeople e
  calculateWalls e

  checkInjured

  updateSpeed
  updateHeading
  e' <- updatePosition e

  return e'

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
  return ()

calculateWalls :: SocialForceEnvironment -> State SocialForceAgentOut ()
calculateWalls e = do
  return ()

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
  (x, y) <- agentStateFieldM perPos
  (destX, destY) <- agentStateFieldM perDest
  (speedX, speedY) <- agentStateFieldM perSpeed

  ifThenElseM
    (inState Reading)
    (updateAgentStateM (\s -> s { perHeading = (atan2 (destY - y) (destX - 40 / 25 - x)) + pi / 2})) -- TODO: wtf? what are these magic numbers?
    (updateAgentStateM (\s -> s { perHeading = (atan2 speedX speedY) + pi / 2}))

updatePosition :: SocialForceEnvironment -> State SocialForceAgentOut SocialForceEnvironment
updatePosition e = do
  (x, y) <- agentStateFieldM perPos
  (speedX, speedY) <- agentStateFieldM perSpeed

  let x' = x + speedX * unitTime
  let y' = y + speedY * unitTime
  
  updateAgentStateM (\s -> s { perPos = (x', y')})

  -- TODO: update position in environment
  return e

inState :: PersonState -> State SocialForceAgentOut Bool
inState ps = do
  ps' <- agentStateFieldM perState
  return $ ps == ps'

personBehaviour :: SocialForceAgentBehaviour
personBehaviour = doRepeatedlyEvery unitTime (agentMonadic personUpdate) -- TODO: do state-chart first and then do personupdate

-------------------------------------------------------------------------------
createPerson :: AgentId
                -> Continuous2dCoord 
                -> Rand StdGen SocialForceAgentDef
createPerson aid p = do
  rng <- getSplit

  riRand <- getRandomR (0.15, 0.25)

  let s = Person {
      perState        = GoingToEntrance
    , perPos          = p
    , perSpeed        = (0, 0)
    , perArrivedDest  = False
    , perHeading      = 0
    , perVi0          = 1.4
    , perAi           = 50 * (pre_ppl_psy ** 2) 
    , perBi           = 0.2
    , perK            = 1.2 * 100000
    , perk            = 2.4 * 100000
    , perRi           = riRand
    , perMi           = 80
    , perDest         = (0, 0)
    , perConRange     = pre_range
    , perAttRange     = pre_angle
    , perAiWall       = 50 * (pre_wall_psy ** 2)
    , perAiGrp        = 5
    , perColor        = whiteColor
    , perBelGroup     = Nothing
    , perDestScreen   = -1
    , perSumFiWH      = 0
    , perSumFiWV      = 0
    , perSumFijH      = 0
    , perSumFijV      = 0
    , perInjured      = False
  }

  return AgentDef {
    adId = aid,
    adState = s,
    adConversation = Nothing,
    adInitMessages = NoEvent,
    adBeh = personBehaviour,
    adRng = rng 
  }