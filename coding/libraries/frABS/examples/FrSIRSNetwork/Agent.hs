{-# LANGUAGE Arrows #-}
module FrSIRSNetwork.Agent (
    sirsNetworkAgentBehaviour,
    sirsNetworkAgentBehaviourRandInfected
  ) where

import FrSIRSNetwork.Model

import FRP.Yampa

import FRP.FrABS

import Control.Monad.Random

------------------------------------------------------------------------------------------------------------------------
-- Non-Reactive Functions
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

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
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
sirsAgentInfected g duration = transitionAfter duration (sirsAgentInfectedBehaviour g) (sirsAgentRecovered g)


sirsAgentInfectedBehaviour :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentInfectedBehaviour g = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao0 = setDomainState ao Infected
        let ao1 = respondToContactWith Infected ain ao0 

        ao2 <- sirsAgentMakeContact g Infected -< ao1

        returnA -< ao2


sirsAgentRecovered :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentRecovered g = transitionAfter immuneDuration sirsAgentRecoveredBehaviour (sirsAgentSuceptible g)

sirsAgentRecoveredBehaviour :: FrSIRSNetworkAgentBehaviour
sirsAgentRecoveredBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao' = setDomainState ao Recovered

        -- ao' <- doOnce () -< ao

        returnA -< ao'


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