{-# LANGUAGE Arrows #-}
module FrSIRSNetwork.Agent (
    sirsNetworkAgentBehaviour,
    -- sirsNetworkAgentBehaviourRandInfected
  ) where

import FrSIRSNetwork.Model

import FRP.Yampa

import FRP.FrABS

import Control.Monad.Random

------------------------------------------------------------------------------------------------------------------------
-- Non-Reactive Functions
------------------------------------------------------------------------------------------------------------------------
respondToContactWith :: SIRSState -> FrSIRSNetworkAgentIn -> FrSIRSNetworkAgentOut -> FrSIRSNetworkAgentOut
respondToContactWith state ain ao = onMessage respondToContactWithAux ain ao
    where
        respondToContactWithAux :: AgentMessage FrSIRSNetworkMsg -> FrSIRSNetworkAgentOut -> FrSIRSNetworkAgentOut
        respondToContactWithAux (senderId, Contact _) ao = sendMessage (senderId, Contact state) ao

gotInfected :: FrSIRSNetworkAgentIn -> Rand StdGen Bool
gotInfected ain = onMessageM gotInfectedAux ain False
    where
        gotInfectedAux :: Bool -> AgentMessage FrSIRSNetworkMsg -> Rand StdGen Bool
        gotInfectedAux False (_, Contact Infected) = randomBoolM infectivity
        gotInfectedAux False _ = return False
        gotInfectedAux True _ = return True
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
-- SUSCEPTIBLE
sirsAgentSuceptible :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentSuceptible g = transitionOnEvent
                            sirsAgentInfectedEvent
                            (sirsAgentSusceptibleBehaviour g)
                            (sirsAgentInfected g illnessDuration)

sirsAgentInfectedEvent :: FrSIRSNetworkEventSource
sirsAgentInfectedEvent = proc (ain, ao) ->
    do
        let (isInfected, ao') = agentRandom (gotInfected ain) ao
        infectionEvent <- edge -< isInfected
        returnA -< (ao', infectionEvent)

sirsAgentSusceptibleBehaviour :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentSusceptibleBehaviour g = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        ao0 <- doOnce (setDomainState Susceptible) -< ao
        ao1 <- sendMessageOccasionallySrc 
                    g 
                    (1 / contactRate) 
                    (randomNeighbourNodeMsgSource (Contact Susceptible)) -< (ao0, e)
        returnA -< (ao1, e)

-- INFECTED
sirsAgentInfected :: RandomGen g => g -> Double -> FrSIRSNetworkAgentBehaviour
sirsAgentInfected g duration = transitionAfterExp
                                    g
                                    duration 
                                    (sirsAgentInfectedBehaviour g) 
                                    (sirsAgentRecovered g)

sirsAgentInfectedBehaviour :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentInfectedBehaviour g = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        let ao0 = respondToContactWith Infected ain ao

        ao1 <- doOnce (setDomainState Infected) -< ao0
        ao2 <- sendMessageOccasionallySrc 
                    g 
                    (1 / contactRate) 
                    (randomNeighbourNodeMsgSource (Contact Infected)) -< (ao1, e)

        returnA -< (ao2, e)

-- RECOVERED
sirsAgentRecovered :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentRecovered g = transitionAfter 
                            immuneDuration 
                            sirsAgentRecoveredBehaviour
                            (sirsAgentSuceptible g)

sirsAgentRecoveredBehaviour :: FrSIRSNetworkAgentBehaviour
sirsAgentRecoveredBehaviour = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        ao' <- doOnce (setDomainState Recovered) -< ao
        returnA -< (ao', e)

-- INITIAL CASES
sirsNetworkAgentBehaviour :: RandomGen g => g -> SIRSState -> FrSIRSNetworkAgentBehaviour
sirsNetworkAgentBehaviour g Susceptible = sirsAgentSuceptible g
sirsNetworkAgentBehaviour g Infected = sirsAgentInfected g illnessDuration
sirsNetworkAgentBehaviour g Recovered = sirsAgentRecovered g

{-
-- TODO: replace this by transitionAfterExp: need to implement our own afterExp SF
sirsNetworkAgentBehaviourRandInfected :: RandomGen g => g -> SIRSState -> FrSIRSNetworkAgentBehaviour
sirsNetworkAgentBehaviourRandInfected g Infected = sirsAgentInfected g' duration
    where
        (duration, g') = randomExp g (1 / illnessDuration)
sirsNetworkAgentBehaviourRandInfected g s = sirsNetworkAgentBehaviour g s
-}
------------------------------------------------------------------------------------------------------------------------