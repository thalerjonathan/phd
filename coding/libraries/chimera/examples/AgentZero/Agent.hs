{-# LANGUAGE Arrows #-}
module Agent 
  (
    agentZeroAgentBehaviour
  ) where

import Control.Monad.IfElse
import Control.Monad.Random
import Control.Monad.Trans.State
import Data.Maybe

import FRP.FrABS
import FRP.Yampa

import Model

-------------------------------------------------------------------------------
-- DOMAIN-STATE functions
-------------------------------------------------------------------------------
isAttackingSite :: AgentZeroEnvCell -> Bool
isAttackingSite AgentZeroEnvCell{ azCellState = cellState } = Attack == cellState

onAttackingSite :: AgentZeroEnvironment -> AgentZeroAgentState -> Bool
onAttackingSite e s = isAttackingSite cell
  where
    coord = azAgentCoord s
    coordPatch = agentCoordToPatchCoord e coord
    cell = cellAt coordPatch (azWorldPatches e)

onAttackingSiteM :: AgentZeroEnvironment -> State AgentZeroAgentOut Bool
onAttackingSiteM e = agentStateM >>= \s -> return $ onAttackingSite e s
  
agentCoordToPatchCoord :: AgentZeroEnvironment -> Continuous2dCoord -> Discrete2dCoord
agentCoordToPatchCoord e cc = cont2dTransDisc2d wp as cc
  where
    wp = azWorldPatches e
    as = azAgentSpace e

incrementEventCount :: AgentZeroAgentState -> AgentZeroAgentState
incrementEventCount s = s { azAgentEventCount = azAgentEventCount s + 1}

doesTakeAction :: AgentZeroAgentState -> Bool
doesTakeAction s = azAgentDispo s > 0
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MONADIC AGENT-BEHAVIOUR 
-------------------------------------------------------------------------------
agentZeroAgentBehaviourFuncM :: RandomGen g => g 
                                -> AgentZeroEnvironment 
                                -> Double 
                                -> AgentZeroAgentIn 
                                -> State AgentZeroAgentOut (AgentZeroEnvironment, g)
agentZeroAgentBehaviourFuncM g e _ ain = do
    g' <- randomMoveM g aid e
    updateEventCountM e
    updateAffectM e
    updateProbM e
    updateDispoM e ain

    ifThenElseM 
      takeActionM 
      ((destroyPatchesM e) >>= \e' -> return (e', g'))
      (return (e, g'))
  where
    aid = agentId ain

randomMoveM :: RandomGen g => g -> AgentId -> AgentZeroEnvironment -> State AgentZeroAgentOut g
randomMoveM g 0 _ = return g -- no movement for agent with id = 0
randomMoveM g _ e = do
  coord <- agentStateFieldM azAgentCoord
  let (newCoord, g') = runRand (stepRandom coord (azAgentSpace e) movementSpeed) g
  updateAgentStateM (\s -> s { azAgentCoord = newCoord })
  return g'

updateEventCountM :: AgentZeroEnvironment -> State AgentZeroAgentOut ()
updateEventCountM e = do
  evtCount <- agentStateFieldM azAgentEventCount
  whenM 
    (onAttackingSiteM e)
    (updateAgentStateM (\s -> s { azAgentEventCount = evtCount + 1} ))

updateAffectM :: AgentZeroEnvironment -> State AgentZeroAgentOut ()
updateAffectM e = do
  affect <- agentStateFieldM azAgentAffect
  learningRate <- agentStateFieldM azAgentLearningRate
  delta <- agentStateFieldM azAgentDelta
  lambda <- agentStateFieldM azAgentLambda

  ifThenElseM 
    (onAttackingSiteM e)
    (updateAgentStateM (\s -> s { azAgentAffect = affect + (learningRate * (affect ** delta) * (lambda - affect))}))
    (updateAgentStateM (\s -> s { azAgentAffect = affect + (learningRate * (affect ** delta) * extinctionRate * (0 - affect))}))

updateProbM :: AgentZeroEnvironment -> State AgentZeroAgentOut ()
updateProbM e = do
    coord <- agentStateFieldM azAgentCoord
    let coordDisc = agentCoordToPatchCoord e coord
    let cs = cellsAroundRadius coordDisc sampleRadius (azWorldPatches e)

    mem <- agentStateFieldM azAgentMemory

    let csAttacking = filter (isAttackingSite . snd) cs
    let localProb = fromRational (fromIntegral $ length csAttacking) / (fromIntegral $ length cs)
    let mem' = localProb : init mem
    let newProb = mean mem'

    updateAgentStateM (\s -> s { azAgentProb = newProb, azAgentMemory = mem' })

  where
    mean :: (Fractional a) => [a] -> a
    mean xs = s / n
      where
        s = sum xs
        n = fromIntegral $ length xs

updateDispoM :: AgentZeroEnvironment -> AgentZeroAgentIn -> State AgentZeroAgentOut ()
updateDispoM e ain = do
    affect <- agentStateFieldM azAgentAffect
    prob <- agentStateFieldM azAgentProb
    thresh <- agentStateFieldM azAgentThresh

    let dispoLocal = affect + prob

    updateAgentStateM (\s -> s { azAgentDispo = dispoLocal - thresh })
    onMessageMState dispositionMessageHandleM ain
    broadcastMessageM (Disposition dispoLocal) linkIds

  where
    aid = agentId ain
    net = azAgentNetwork e
    linkIds = neighbourNodes aid net

    dispositionMessageHandleM :: AgentMessage AgentZeroMsg -> State AgentZeroAgentOut ()
    dispositionMessageHandleM (senderId, Disposition d) = updateAgentStateM (\s -> s { azAgentDispo = azAgentDispo s + (d * weight)})
      where
        mayWeight = directLinkBetween senderId aid net
        weight = fromMaybe 0 mayWeight

takeActionM :: State AgentZeroAgentOut Bool
takeActionM = agentStateFieldM azAgentDispo >>= \dispo -> return $ dispo > 0

destroyPatchesM :: AgentZeroEnvironment -> State AgentZeroAgentOut AgentZeroEnvironment
destroyPatchesM e = agentStateFieldM azAgentCoord >>= \coord -> return $ destroyPatches coord e
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  AGENT-BEHAVIOUR NON-MONADIC implementation
-------------------------------------------------------------------------------
updateEventCount :: AgentZeroEnvironment -> AgentZeroAgentState -> AgentZeroAgentState
updateEventCount e s
  | onAttackingSite e s = incrementEventCount s
  | otherwise = s

updateAffect :: AgentZeroEnvironment -> AgentZeroAgentState -> AgentZeroAgentState
updateAffect e s
    | onAttackingSite e s = s { azAgentAffect = affect + (learningRate * (affect ** delta) * (lambda - affect)) }
    | otherwise = s { azAgentAffect = affect + (learningRate * (affect ** delta) * extinctionRate * (0 - affect)) }
  where
    affect = azAgentAffect s
    learningRate = azAgentLearningRate s
    delta = azAgentDelta s
    lambda = azAgentLambda s

updateProb :: AgentZeroEnvironment -> AgentZeroAgentState -> AgentZeroAgentState
updateProb e s = s { azAgentProb = newProb, azAgentMemory = mem' }
  where
    coordPatch = agentCoordToPatchCoord e (azAgentCoord s)
    cs = cellsAroundRadius coordPatch sampleRadius (azWorldPatches e)
    csAttacking = filter (isAttackingSite . snd) cs

    localProb = fromRational (fromIntegral $ length csAttacking) / (fromIntegral $ length cs)

    mem = azAgentMemory s
    mem' = localProb : init mem

    newProb = mean mem'

    mean :: (Fractional a) => [a] -> a
    mean xs = s / n
      where
        s = sum xs
        n = fromIntegral $ length xs

updateDispo :: AgentZeroEnvironment -> AgentZeroAgentIn -> AgentZeroAgentOut -> AgentZeroAgentOut
updateDispo e ain ao = broadcastMessage (Disposition dispoLocal) linkIds aDispoFinal
  where
    aid = agentId ain
    s = agentState ao

    affect = azAgentAffect s
    prob = azAgentProb s
    thresh = azAgentThresh s
    dispoLocal = affect + prob

    net = azAgentNetwork e
    linkIds = neighbourNodes aid net

    aDispoSelf = updateAgentState (\s -> s { azAgentDispo = dispoLocal - thresh }) ao
    aDispoFinal = onMessage dispositionMessageHandle ain aDispoSelf

    dispositionMessageHandle :: AgentMessage AgentZeroMsg -> AgentZeroAgentOut -> AgentZeroAgentOut
    dispositionMessageHandle (senderId, Disposition d) ao = updateAgentState (\s -> s { azAgentDispo = azAgentDispo s + (d * weight)}) ao
      where
        mayWeight = directLinkBetween senderId aid net
        weight = fromMaybe 0 mayWeight

destroyPatches :: Continuous2dCoord -> AgentZeroEnvironment -> AgentZeroEnvironment
destroyPatches coordCont e = e { azWorldPatches = wp' }
  where
    wp = azWorldPatches e
    aspace = azAgentSpace e

    coordDisc = cont2dTransDisc2d wp aspace coordCont

    cs = cellsAroundRadius coordDisc destructionRadius wp
    wp' = foldr (\(coord, cell) wpAcc -> changeCellAt coord (cell { azCellState = Dead }) wpAcc) wp cs

randomMove :: RandomGen g => g -> AgentId -> AgentZeroEnvironment -> AgentZeroAgentOut -> (AgentZeroAgentOut, g)
randomMove g 0 _ ao = (ao, g) -- no movement for agent with id = 0 
randomMove g _ e ao = (updateAgentState (\s -> s { azAgentCoord = newCoord }) ao, g')
  where
    coord = azAgentCoord $ agentState ao
    (newCoord, g') = runRand (stepRandom coord (azAgentSpace e) movementSpeed) g

agentZeroAgentBehaviourFunc :: RandomGen g => g 
                              -> AgentZeroEnvironment 
                              -> Double 
                              -> AgentZeroAgentIn 
                              -> AgentZeroAgentOut 
                              -> (AgentZeroAgentOut, AgentZeroEnvironment, g)
agentZeroAgentBehaviourFunc g e _ ain ao
    | doesTakeAction s2 = (ao2, e', g')
    | otherwise = (ao2, e, g')
  where
    (ao0, g') = randomMove g (agentId ain) e ao

    s0 = updateProb e $
          updateAffect e $
          updateEventCount e (agentState ao0)

    ao1 = setAgentState s0 ao0
    ao2 = updateDispo e ain ao1

    s2 = agentState ao2
    coord = azAgentCoord s2

    e' = destroyPatches coord e 

agentZeroAgentBehaviour :: RandomGen g => g -> AgentZeroAgentState -> AgentZeroAgentBehaviour
agentZeroAgentBehaviour gi is = proc (ain, e) -> do
    rec
      s' <- iPre is -< s
      g' <- iPre gi -< g''
      let ao = agentOutObs s'
      t <- time -< ()
      --let ((e', g''), ao') = runState (agentZeroAgentBehaviourFuncM g' e t ain) ao
      let (ao', e', g'') = agentZeroAgentBehaviourFunc g' e t ain ao
      let s = agentState ao'

    returnA -< (ao', e')
-------------------------------------------------------------------------------
