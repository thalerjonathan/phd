{-# LANGUAGE Arrows #-}
module FrSIRSSpatial.Agent (
    sirsAgentBehaviour
  ) where

import FrSIRSSpatial.Model

import FRP.Yampa

import FRP.FrABS

import Control.Monad.Random

------------------------------------------------------------------------------------------------------------------------
-- Non-Reactive Functions
------------------------------------------------------------------------------------------------------------------------
gotInfected :: FrSIRSSpatialAgentIn -> Rand StdGen Bool
gotInfected ain = onMessageM gotInfectedAux ain False
    where
        gotInfectedAux :: Bool -> AgentMessage FrSIRSSpatialMsg -> Rand StdGen Bool
        gotInfectedAux False (_, Contact Infected) = randomBoolM infectivity
        gotInfectedAux x _ = return x

respondToContactWith :: SIRSState -> FrSIRSSpatialAgentIn -> FrSIRSSpatialAgentOut -> FrSIRSSpatialAgentOut
respondToContactWith state ain ao = onMessage respondToContactWithAux ain ao
  where
    respondToContactWithAux :: AgentMessage FrSIRSSpatialMsg -> FrSIRSSpatialAgentOut -> FrSIRSSpatialAgentOut
    respondToContactWithAux (senderId, Contact _) ao = sendMessage (senderId, Contact state) ao
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
-- SUSCEPTIBLE
sirsAgentSuceptible :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentSuceptible g = transitionOnEvent
                            sirsAgentInfectedEvent
                            (readEnv $ sirsAgentSusceptibleBehaviour g)
                            (sirsAgentInfected g)

sirsAgentInfectedEvent :: FrSIRSSpatialEventSource
sirsAgentInfectedEvent = proc (ain, ao) -> do
    let (isInfected, ao') = agentRandom (gotInfected ain) ao
    infectionEvent <- edge -< isInfected
    returnA -< (ao', infectionEvent)

sirsAgentSusceptibleBehaviour :: RandomGen g => g -> FrSIRSSpatialAgentBehaviourReadEnv
sirsAgentSusceptibleBehaviour g = proc (ain, e) -> do
    let ao = agentOutFromIn ain
    ao1 <- doOnce (updateAgentState (\s -> s { sirsState = Susceptible})) -< ao
    ao2 <- sendMessageOccasionallySrcSS 
                g 
                (1 / contactRate) 
                contactSS 
                (randomNeighbourCellMsgSource sirsCoord (Contact Susceptible) False) -< (ao1, e)
    returnA -< ao2

-- INFECTED
sirsAgentInfected :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentInfected g = transitionAfterExpSS
                        g 
                        illnessDuration 
                        illnessTimeoutSS
                        (ignoreEnv $ sirsAgentInfectedBehaviour) 
                        (sirsAgentRecovered g)

sirsAgentInfectedBehaviour :: FrSIRSSpatialAgentBehaviourIgnoreEnv
sirsAgentInfectedBehaviour = proc ain -> do
    let ao = agentOutFromIn ain
    ao1 <- doOnce (updateAgentState (\s -> s { sirsState = Infected })) -< ao
    let ao2 = respondToContactWith Infected ain ao1
    returnA -< ao2

-- RECOVERED
sirsAgentRecovered :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentRecovered _ = doOnceR $ updateAgentStateR (\s -> s { sirsState = Recovered })
{--
sirsAgentRecovered :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentRecovered g = transitionAfterExpSS 
                            g
                            immuneDuration 
                            immuneTimeoutSS
                            sirsAgentRecoveredBehaviour
                            (sirsAgentSuceptible g)

sirsAgentRecoveredBehaviour :: FrSIRSSpatialAgentBehaviour
sirsAgentRecoveredBehaviour = updateAgentStateR (\s -> s { sirsState = Recovered })
-}

-- INITIAL CASES
sirsAgentBehaviour :: RandomGen g => g -> SIRSState -> FrSIRSSpatialAgentBehaviour
sirsAgentBehaviour g Susceptible = sirsAgentSuceptible g
sirsAgentBehaviour g Infected = sirsAgentInfected g
sirsAgentBehaviour g Recovered = sirsAgentRecovered g
------------------------------------------------------------------------------------------------------------------------