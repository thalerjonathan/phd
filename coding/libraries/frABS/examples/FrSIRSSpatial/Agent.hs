{-# LANGUAGE Arrows #-}
module FrSIRSSpatial.Agent (
    sirsAgentBehaviour,
    sirsAgentBehaviourRandInfected
  ) where

import FrSIRSSpatial.Model

import FRP.Yampa

import FRP.FrABS

import Control.Monad.Random

------------------------------------------------------------------------------------------------------------------------
-- Non-Reactive Functions
------------------------------------------------------------------------------------------------------------------------
respondToContactWith :: SIRSState -> FrSIRSSpatialAgentIn -> FrSIRSSpatialAgentOut -> FrSIRSSpatialAgentOut
respondToContactWith state ain ao = onMessage respondToContactWithAux ain ao
    where
        respondToContactWithAux :: AgentMessage FrSIRSSpatialMsg -> FrSIRSSpatialAgentOut -> FrSIRSSpatialAgentOut
        respondToContactWithAux (senderId, Contact _) ao = sendMessage (senderId, Contact state) ao

gotInfected :: FrSIRSSpatialAgentIn -> Rand StdGen Bool
gotInfected ain = onMessageM gotInfectedAux ain False
    where
        gotInfectedAux :: Bool -> AgentMessage FrSIRSSpatialMsg -> Rand StdGen Bool
        gotInfectedAux False (_, Contact Infected) = drawRandomBoolM infectivity
        gotInfectedAux False _ = return False
        gotInfectedAux True _ = return True
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
-- SUSCEPTIBLE
sirsAgentSuceptible :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentSuceptible g = transitionOnEvent
                            sirsAgentInfectedEvent
                            (sirsAgentSusceptibleBehaviour g)
                            (sirsAgentBehaviourRandInfected g Infected)

sirsAgentInfectedEvent :: FrSIRSSpatialEventSource
sirsAgentInfectedEvent = proc (ain, ao) ->
    do
        let (isInfected, ao') = runAgentRandom (gotInfected ain) ao
        infectionEvent <- edge -< isInfected
        returnA -< (ao', infectionEvent)

sirsAgentSusceptibleBehaviour :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentSusceptibleBehaviour g = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        ao0 <- doOnce (updateDomainState (\s -> s { sirsState = Susceptible})) -< ao
        ao1 <- sendMessageOccasionallySrc 
                    g 
                    (1 / contactRate) 
                    (randomNeighbourCellMsgSource sirsCoord (Contact Susceptible)) -< (ao0, e)
        returnA -< (ao1, e)

-- INFECTED
sirsAgentInfected :: RandomGen g => g -> Double -> FrSIRSSpatialAgentBehaviour
sirsAgentInfected g duration = transitionAfter 
                                    duration 
                                    (sirsAgentInfectedBehaviour g) 
                                    (sirsAgentRecovered g)

sirsAgentInfectedBehaviour :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentInfectedBehaviour g = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        let ao0 = respondToContactWith Infected ain ao

        ao1 <- doOnce (updateDomainState (\s -> s { sirsState = Infected})) -< ao0
        ao2 <- sendMessageOccasionallySrc 
                    g 
                    (1 / contactRate) 
                    (randomNeighbourCellMsgSource sirsCoord (Contact Infected)) -< (ao1, e)

        returnA -< (ao2, e)

-- RECOVERED
sirsAgentRecovered :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentRecovered g = transitionAfter 
                            immuneDuration 
                            sirsAgentRecoveredBehaviour 
                            (sirsAgentSuceptible g)

sirsAgentRecoveredBehaviour :: FrSIRSSpatialAgentBehaviour
sirsAgentRecoveredBehaviour = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        ao' <- doOnce (updateDomainState (\s -> s { sirsState = Recovered})) -< ao
        returnA -< (ao', e)

-- INITIAL CASES
sirsAgentBehaviour :: RandomGen g => g -> SIRSState -> FrSIRSSpatialAgentBehaviour
sirsAgentBehaviour g Susceptible = sirsAgentSuceptible g
sirsAgentBehaviour g Infected = sirsAgentInfected g illnessDuration
sirsAgentBehaviour g Recovered = sirsAgentRecovered g

sirsAgentBehaviourRandInfected :: RandomGen g => g -> SIRSState -> FrSIRSSpatialAgentBehaviour
sirsAgentBehaviourRandInfected g Infected = sirsAgentInfected g' duration
    where
        (duration, g') = drawRandomExponential g (1/illnessDuration)
sirsAgentBehaviourRandInfected g s = sirsAgentBehaviour g s
------------------------------------------------------------------------------------------------------------------------