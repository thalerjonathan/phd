{-# LANGUAGE Arrows #-}
module FrSIRSNetwork.FrSIRSNetworkAgent (
    sirsNetworkAgentBehaviour,
    sirsNetworkAgentBehaviourRandInfected
  ) where

import FrSIRSNetwork.FrSIRSNetworkModel

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Agent.AgentUtils

import Control.Monad
import Control.Monad.Random

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR Functional Reactive implementation
------------------------------------------------------------------------------------------------------------------------
randomContact :: FrSIRSNetworkAgentOut -> FrSIRSNetworkAgentOut
randomContact ao = sendMessage ao' (randNeigh, Contact Infected)
    where
        (randNeigh, ao') = runAgentRandom ao (pickRandomNeighbourNode ao)

gotInfected :: FrSIRSNetworkAgentIn -> Rand StdGen Bool
gotInfected ain = onMessageM ain gotInfectedAux False
    where
        gotInfectedAux :: Bool -> AgentMessage FrSIRSNetworkMsg -> Rand StdGen Bool
        gotInfectedAux False (_, Contact Infected) = drawRandomBool infectivity
        gotInfectedAux False _ = return False
        gotInfectedAux True _ = return True

sirsAgentSuceptible :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentSuceptible g = switch 
                            sirsAgentSusceptibleBehaviour
                            (sirsAgentSusceptibleInfected g)

sirsAgentSusceptibleBehaviour :: SF FrSIRSNetworkAgentIn (FrSIRSNetworkAgentOut, Event ())
sirsAgentSusceptibleBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao0 = setDomainState ao Susceptible
        let (isInfected, ao1) = runAgentRandom ao0 (gotInfected ain)

        infectionEvent <- iEdge False -< isInfected

        returnA -< (ao1, infectionEvent)

-- TODO: update sirsState to infected here once, no need to constantly set to infected in infecedbehaviourSF
sirsAgentSusceptibleInfected :: RandomGen g => g -> () -> FrSIRSNetworkAgentBehaviour
sirsAgentSusceptibleInfected g _ = sirsAgentInfected g illnessDuration



sirsAgentInfected :: RandomGen g => g -> Double -> FrSIRSNetworkAgentBehaviour
sirsAgentInfected g duration = switch 
                            (sirsAgentInfectedBehaviour g illnessDuration)
                            (sirsAgentInfectedRecovered g)


sirsAgentInfectedBehaviour :: RandomGen g => g -> Double -> SF FrSIRSNetworkAgentIn (FrSIRSNetworkAgentOut, Event ())
sirsAgentInfectedBehaviour g duration = proc ain ->
    do
        recoveredEvent <- after duration () -< ()
        makeContact <- occasionally g (1 / contactRate) () -< ()

        let ao = agentOutFromIn ain
        let ao0 = setDomainState ao Infected
        -- NOTE: if recovered, then don't make contact anymore but it is very unlikely that both occur at the same time
        let ao1 = event ao0 (\_ -> randomContact ao0) makeContact

        returnA -< (ao1, recoveredEvent)

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
        (duration, g') = randomR (0.0, illnessDuration) g
sirsNetworkAgentBehaviourRandInfected g Recovered = sirsAgentRecovered g

-- NOTE: this is the initial SF which will be only called once
--          this behaviour should be used when initially a given number of agents is infected 
--          where is assumed that their illness-duration is not uniform randomly distributed
sirsNetworkAgentBehaviour :: RandomGen g => g -> SIRSState -> FrSIRSNetworkAgentBehaviour
sirsNetworkAgentBehaviour g Susceptible = sirsAgentSuceptible g
sirsNetworkAgentBehaviour g Infected = sirsAgentInfected g illnessDuration
sirsNetworkAgentBehaviour g Recovered = sirsAgentRecovered g
------------------------------------------------------------------------------------------------------------------------