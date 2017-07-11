{-# LANGUAGE Arrows #-}
module FrSIRSNetwork.Agent (
    sirsNetworkAgentBehaviour,
    sirsNetworkAgentBehaviourRandInfected
  ) where

import FrSIRSNetwork.Model

import FRP.Yampa

import FRP.FrABS

import Control.Monad
import Control.Monad.Random

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR Functional Reactive implementation
------------------------------------------------------------------------------------------------------------------------
randomContact :: SIRSState -> FrSIRSNetworkAgentOut -> FrSIRSNetworkAgentOut
randomContact state ao = sendMessage ao' (randNeigh, Contact state)
    where
        (randNeigh, ao') = runAgentRandom ao (pickRandomNeighbourNode ao)

respondToContactWith :: SIRSState -> FrSIRSNetworkAgentIn -> FrSIRSNetworkAgentOut -> FrSIRSNetworkAgentOut
respondToContactWith state ain ao = onMessage ain respondToContactWithAux ao
    where
        respondToContactWithAux :: FrSIRSNetworkAgentOut -> AgentMessage FrSIRSNetworkMsg -> FrSIRSNetworkAgentOut
        respondToContactWithAux ao (senderId, Contact _) = sendMessage ao (senderId, Contact state) 

gotInfected :: FrSIRSNetworkAgentIn -> Rand StdGen Bool
gotInfected ain = onMessageM ain gotInfectedAux False
    where
        gotInfectedAux :: Bool -> AgentMessage FrSIRSNetworkMsg -> Rand StdGen Bool
        gotInfectedAux False (_, Contact Infected) = drawRandomBoolM infectivity
        gotInfectedAux False _ = return False
        gotInfectedAux True _ = return True

sirsAgentMakeContact :: RandomGen g => g -> SIRSState -> SF FrSIRSNetworkAgentOut FrSIRSNetworkAgentOut
sirsAgentMakeContact g state = proc ao ->
    do
        makeContact <- occasionally g (1 / contactRate) () -< ()
        let ao' = event ao (\_ -> randomContact state ao) makeContact
        returnA -< ao'


sirsAgentSuceptible :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentSuceptible g = switch 
                            (sirsAgentSusceptibleBehaviour g)
                            (sirsAgentSusceptibleInfected g)

sirsAgentSusceptibleBehaviour :: RandomGen g => g -> SF FrSIRSNetworkAgentIn (FrSIRSNetworkAgentOut, Event ())
sirsAgentSusceptibleBehaviour g = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao0 = setDomainState ao Susceptible
        let (isInfected, ao1) = runAgentRandom ao0 (gotInfected ain)
    
        infectionEvent <- edge -< isInfected
        ao2 <- sirsAgentMakeContact g Susceptible -< ao1

        returnA -< (ao2, infectionEvent)

-- TODO: update sirsState to infected here once, no need to constantly set to infected in infecedbehaviourSF
sirsAgentSusceptibleInfected :: RandomGen g => g -> () -> FrSIRSNetworkAgentBehaviour
sirsAgentSusceptibleInfected g _ = sirsNetworkAgentBehaviourRandInfected g Infected




sirsAgentInfected :: RandomGen g => g -> Double -> FrSIRSNetworkAgentBehaviour
sirsAgentInfected g duration = switch 
                            (sirsAgentInfectedBehaviour g duration)
                            (sirsAgentInfectedRecovered g)


sirsAgentInfectedBehaviour :: RandomGen g => g -> Double -> SF FrSIRSNetworkAgentIn (FrSIRSNetworkAgentOut, Event ())
sirsAgentInfectedBehaviour g duration = proc ain ->
    do
        recoveredEvent <- after duration () -< ()
    
        let ao = agentOutFromIn ain
        let ao0 = setDomainState ao Infected
        let ao1 = respondToContactWith Infected ain ao0 

        ao2 <- sirsAgentMakeContact g Infected -< ao1

        returnA -< (ao2, recoveredEvent)

-- TODO: update sirsState to recovered here once, no need to constantly set to recovered in recoverbehaviourSF
sirsAgentInfectedRecovered :: RandomGen g => g -> () -> FrSIRSNetworkAgentBehaviour
sirsAgentInfectedRecovered g _ = sirsAgentRecovered g



sirsAgentRecovered :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentRecovered g = switch 
                            sirsAgentRecoveredBehaviour
                            (sirsAgentRecoveredSusceptible g)

sirsAgentRecoveredBehaviour :: SF FrSIRSNetworkAgentIn (FrSIRSNetworkAgentOut, Event ())
sirsAgentRecoveredBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao' = setDomainState ao Recovered

        lostImmunityEvent <- after immuneDuration () -< ()

        returnA -< (ao', lostImmunityEvent)

-- TODO: update sirsState to susceptible here once, no need to constantly set to susceptible in susceptiblebehaviourSF
sirsAgentRecoveredSusceptible :: RandomGen g => g -> () -> FrSIRSNetworkAgentBehaviour
sirsAgentRecoveredSusceptible g _ = sirsAgentSuceptible g

-- NOTE: this is the initial SF which will be only called once
sirsNetworkAgentBehaviourRandInfected :: RandomGen g => g -> SIRSState -> FrSIRSNetworkAgentBehaviour
sirsNetworkAgentBehaviourRandInfected g Susceptible = sirsAgentSuceptible g
-- NOTE: when initially infected then select duration uniformly random 
sirsNetworkAgentBehaviourRandInfected g Infected = sirsAgentInfected g' duration
    where
        (duration, g') = drawRandomExponential g (1/illnessDuration)
sirsNetworkAgentBehaviourRandInfected g Recovered = sirsAgentRecovered g

-- NOTE: this is the initial SF which will be only called once
--          this behaviour should be used when initially a given number of agents is infected 
--          where is assumed that their illness-duration is not uniform randomly distributed
sirsNetworkAgentBehaviour :: RandomGen g => g -> SIRSState -> FrSIRSNetworkAgentBehaviour
sirsNetworkAgentBehaviour g Susceptible = sirsAgentSuceptible g
sirsNetworkAgentBehaviour g Infected = sirsAgentInfected g illnessDuration
sirsNetworkAgentBehaviour g Recovered = sirsAgentRecovered g
------------------------------------------------------------------------------------------------------------------------