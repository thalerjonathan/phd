{-# LANGUAGE Arrows #-}
module FrSIRSSpatial.Agent (
    sirsAgentBehaviour,
    sirsAgentBehaviourRandInfected
  ) where

import FrSIRSSpatial.Model

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Agent.Utils
import FrABS.Agent.Random
import FrABS.Agent.Monad

import Control.Monad
import Control.Monad.Random

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR Functional Reactive implementation
------------------------------------------------------------------------------------------------------------------------
randomContact :: SIRSState -> FrSIRSSpatialAgentOut -> FrSIRSSpatialAgentOut
randomContact state ao = sendMessage ao' (randNeigh, Contact state)
    where
        ((_, randNeigh), ao') = runAgentRandom ao (pickRandomNeighbourCell ao)

respondToContactWith :: SIRSState -> FrSIRSSpatialAgentIn -> FrSIRSSpatialAgentOut -> FrSIRSSpatialAgentOut
respondToContactWith state ain ao = onMessage ain respondToContactWithAux ao
    where
        respondToContactWithAux :: FrSIRSSpatialAgentOut -> AgentMessage FrSIRSSpatialMsg -> FrSIRSSpatialAgentOut
        respondToContactWithAux ao (senderId, Contact _) = sendMessage ao (senderId, Contact state) 

gotInfected :: FrSIRSSpatialAgentIn -> Rand StdGen Bool
gotInfected ain = onMessageM ain gotInfectedAux False
    where
        gotInfectedAux :: Bool -> AgentMessage FrSIRSSpatialMsg -> Rand StdGen Bool
        gotInfectedAux False (_, Contact Infected) = drawRandomBoolM infectivity
        gotInfectedAux False _ = return False
        gotInfectedAux True _ = return True

sirsAgentMakeContact :: RandomGen g => g -> SIRSState -> SF FrSIRSSpatialAgentOut FrSIRSSpatialAgentOut
sirsAgentMakeContact g state = proc ao ->
    do
        makeContact <- occasionally g (1 / contactRate) () -< ()
        let ao' = event ao (\_ -> randomContact state ao) makeContact
        returnA -< ao'


sirsAgentSuceptible :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentSuceptible g = switch 
                            (sirsAgentSusceptibleBehaviour g)
                            (sirsAgentSusceptibleInfected g)

sirsAgentSusceptibleBehaviour :: RandomGen g => g -> SF FrSIRSSpatialAgentIn (FrSIRSSpatialAgentOut, Event ())
sirsAgentSusceptibleBehaviour g = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao0 = setDomainState ao Susceptible
        let (isInfected, ao1) = runAgentRandom ao0 (gotInfected ain)
    
        infectionEvent <- edge -< isInfected
        ao2 <- sirsAgentMakeContact g Susceptible -< ao1

        returnA -< (ao2, infectionEvent)

-- TODO: update sirsState to infected here once, no need to constantly set to infected in infecedbehaviourSF
sirsAgentSusceptibleInfected :: RandomGen g => g -> () -> FrSIRSSpatialAgentBehaviour
sirsAgentSusceptibleInfected g _ = sirsAgentBehaviourRandInfected g Infected



sirsAgentInfected :: RandomGen g => g -> Double -> FrSIRSSpatialAgentBehaviour
sirsAgentInfected g duration = switch 
                            (sirsAgentInfectedBehaviour g duration)
                            (sirsAgentInfectedRecovered g)


sirsAgentInfectedBehaviour :: RandomGen g => g -> Double -> SF FrSIRSSpatialAgentIn (FrSIRSSpatialAgentOut, Event ())
sirsAgentInfectedBehaviour g duration = proc ain ->
    do
        recoveredEvent <- after duration () -< ()
    
        let ao = agentOutFromIn ain
        let ao0 = setDomainState ao Infected
        let ao1 = respondToContactWith Infected ain ao0 

        ao2 <- sirsAgentMakeContact g Infected -< ao1

        returnA -< (ao1, recoveredEvent)

-- TODO: update sirsState to recovered here once, no need to constantly set to recovered in recoverbehaviourSF
sirsAgentInfectedRecovered :: RandomGen g => g -> () -> FrSIRSSpatialAgentBehaviour
sirsAgentInfectedRecovered g _ = sirsAgentRecovered g



sirsAgentRecovered :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentRecovered g = switch 
                            sirsAgentRecoveredBehaviour
                            (sirsAgentRecoveredSusceptible g)

sirsAgentRecoveredBehaviour :: SF FrSIRSSpatialAgentIn (FrSIRSSpatialAgentOut, Event ())
sirsAgentRecoveredBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao' = setDomainState ao Recovered

        lostImmunityEvent <- after immuneDuration () -< ()

        returnA -< (ao', lostImmunityEvent)

-- TODO: update sirsState to susceptible here once, no need to constantly set to susceptible in susceptiblebehaviourSF
sirsAgentRecoveredSusceptible :: RandomGen g => g -> () -> FrSIRSSpatialAgentBehaviour
sirsAgentRecoveredSusceptible g _ = sirsAgentSuceptible g

-- NOTE: this is the initial SF which will be only called once
sirsAgentBehaviourRandInfected :: RandomGen g => g -> SIRSState -> FrSIRSSpatialAgentBehaviour
sirsAgentBehaviourRandInfected g Susceptible = sirsAgentSuceptible g
-- NOTE: when initially infected then select duration uniformly random 
sirsAgentBehaviourRandInfected g Infected = sirsAgentInfected g' duration
    where
        (duration, g') = drawRandomExponential g (1/illnessDuration)
sirsAgentBehaviourRandInfected g Recovered = sirsAgentRecovered g

-- NOTE: this is the initial SF which will be only called once
--          this behaviour should be used when initially a given number of agents is infected 
--          where is assumed that their illness-duration is not uniform randomly distributed
sirsAgentBehaviour :: RandomGen g => g -> SIRSState -> FrSIRSSpatialAgentBehaviour
sirsAgentBehaviour g Susceptible = sirsAgentSuceptible g
sirsAgentBehaviour g Infected = sirsAgentInfected g illnessDuration
sirsAgentBehaviour g Recovered = sirsAgentRecovered g
------------------------------------------------------------------------------------------------------------------------