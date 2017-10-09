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
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
-- SUSCEPTIBLE
sirsAgentSuceptible :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentSuceptible g = transitionOnEvent
                            sirsAgentInfectedEvent
                            sirsAgentSusceptibleBehaviour
                            (sirsAgentInfected g)

sirsAgentInfectedEvent :: FrSIRSSpatialEventSource
sirsAgentInfectedEvent = proc (ain, ao) ->
    do
        let (isInfected, ao') = agentRandom (gotInfected ain) ao
        infectionEvent <- edge -< isInfected
        returnA -< (ao', infectionEvent)

sirsAgentSusceptibleBehaviour :: FrSIRSSpatialAgentBehaviour
sirsAgentSusceptibleBehaviour = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        ao' <- doOnce (updateDomainState (\s -> s { sirsState = Susceptible})) -< ao
        returnA -< (ao', e)

-- INFECTED
sirsAgentInfected :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentInfected g = transitionAfterExp
                        g 
                        illnessDuration 
                        (sirsAgentInfectedBehaviour g) 
                        (sirsAgentRecovered g )

sirsAgentInfectedBehaviour :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentInfectedBehaviour g = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        ao0 <- doOnce (updateDomainState (\s -> s { sirsState = Infected})) -< ao
        ao1 <- sendMessageOccasionallySrc 
                    g 
                    (1 / contactRate) 
                    (randomNeighbourCellMsgSource sirsCoord (Contact Infected) False) -< (ao0, e)
        returnA -< (ao1, e)

-- RECOVERED
sirsAgentRecovered :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentRecovered g = transitionAfterExp 
                            g
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
sirsAgentBehaviour g Infected = sirsAgentInfected g
sirsAgentBehaviour g Recovered = sirsAgentRecovered g
------------------------------------------------------------------------------------------------------------------------