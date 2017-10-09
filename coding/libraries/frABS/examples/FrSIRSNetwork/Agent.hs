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
gotInfected :: FrSIRSNetworkAgentIn -> Rand StdGen Bool
gotInfected ain = onMessageM gotInfectedAux ain False
    where
        gotInfectedAux :: Bool -> AgentMessage FrSIRSNetworkMsg -> Rand StdGen Bool
        gotInfectedAux False (_, Contact Infected) = randomBoolM infectivity
        gotInfectedAux x _ = return x
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
-- SUSCEPTIBLE
sirsAgentSuceptible :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentSuceptible g = transitionOnEvent
                            sirsAgentInfectedEvent
                            sirsAgentSusceptibleBehaviour
                            (sirsAgentInfected g)

sirsAgentInfectedEvent :: FrSIRSNetworkEventSource
sirsAgentInfectedEvent = proc (ain, ao) ->
    do
        let (isInfected, ao') = agentRandom (gotInfected ain) ao
        infectionEvent <- edge -< isInfected
        returnA -< (ao', infectionEvent)

sirsAgentSusceptibleBehaviour :: FrSIRSNetworkAgentBehaviour
sirsAgentSusceptibleBehaviour = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        ao0 <- doOnce (setDomainState Susceptible) -< ao
        returnA -< (ao0, e)

-- INFECTED
sirsAgentInfected :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentInfected g = transitionAfterExp
                        g
                        illnessDuration 
                        (sirsAgentInfectedBehaviour g) 
                        (sirsAgentRecoveredBehaviour)

sirsAgentInfectedBehaviour :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentInfectedBehaviour g = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        ao0 <- doOnce (setDomainState Infected) -< ao
        ao1 <- sendMessageOccasionallySrc 
                    g 
                    (1 / contactRate) 
                    (randomNeighbourNodeMsgSource (Contact Infected)) -< (ao0, e)
        returnA -< (ao1, e)

-- RECOVERED
sirsAgentRecovered :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentRecovered g = transitionAfterExp
                            g 
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
sirsNetworkAgentBehaviour g Infected = sirsAgentInfected g
sirsNetworkAgentBehaviour g Recovered = sirsAgentRecovered g
------------------------------------------------------------------------------------------------------------------------