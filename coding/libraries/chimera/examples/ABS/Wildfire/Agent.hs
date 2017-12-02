{-# LANGUAGE Arrows #-}
module Agent 
  (
    wildfireAgentBehaviour
  ) where

import FRP.Chimera
import FRP.Yampa

import Model

------------------------------------------------------------------------------------------------------------------------
-- Non-Reactive Functions
------------------------------------------------------------------------------------------------------------------------
isBurnedDown :: WildfireAgentState -> Bool
isBurnedDown s = wfFuelCurr s <= 0.0
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
-- DEAD
wildfireAgentDie :: WildfireAgentBehaviour
wildfireAgentDie = proc (ain, e) -> do
  let aout = agentOutFromIn ain
  let aout0 = updateAgentState (\s -> s { wfLifeState = Dead }) aout
  let aout1 = kill aout0 -- NOTE: killing leads to increased performance but leaves the patch in the color of the background
  returnA -<  (aout1, e)

-- BURNING
wildfireAgentBurning :: RandomGen g => g -> Double -> WildfireAgentBehaviour
wildfireAgentBurning g initFuel = transitionOnBoolState
                                    isBurnedDown
                                    (wildfireAgentBurningBehaviour g initFuel)
                                    wildfireAgentDie

wildfireAgentBurningBehaviour :: RandomGen g => g -> Double -> WildfireAgentBehaviour
wildfireAgentBurningBehaviour g initFuel = proc (ain, e) -> do
  (ao0, _) <- doOnceR $ updateAgentStateR (\s -> s { wfLifeState = Burning }) -< (ain, e)
  ao1 <- burndown initFuel -< ao0
  ao2 <- igniteNeighbours g -< (ao1, e)
  returnA -< (ao2, e)

burndown :: Double -> SF WildfireAgentOut WildfireAgentOut
burndown initFuel = proc ao -> do
  currFuel <- drain initFuel -< wfFuelRate $ agentState ao
  let ao' = updateAgentState (\s -> s { wfFuelCurr = currFuel }) ao
  returnA -< ao'

igniteNeighbours :: RandomGen g => g -> SF (WildfireAgentOut, WildfireEnvironment) WildfireAgentOut
igniteNeighbours g = sendMessageOccasionallySrc 
                      g 
                      (1 / ignitions)
                      (randomNeighbourCellMsgSource wfCoord Ignite False)

-- LIVING
wildfireAgentLiving :: RandomGen g => g -> Double -> WildfireAgentBehaviour
wildfireAgentLiving g initFuel = transitionOnMessage 
                                  Ignite
                                  doNothing
                                  (wildfireAgentBurning g initFuel)

-- NOTE: this implementation has a probabilistic Ignition which depends on how 'burnable' (=initialFuel) the agent (cell) is
wildfireAgentLivingGuarded :: RandomGen g => g -> Double -> WildfireAgentBehaviour
wildfireAgentLivingGuarded g initFuel = transitionOnEventWithGuard 
                                          (messageEventSource Ignite)
                                          (randomBoolM initFuel)
                                          doNothing
                                          (wildfireAgentBurning g initFuel)

-- INITIAL
wildfireAgentBehaviour :: RandomGen g => g -> Double -> WildfireAgentBehaviour
wildfireAgentBehaviour = wildfireAgentLivingGuarded
------------------------------------------------------------------------------------------------------------------------