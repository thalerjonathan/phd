{-# LANGUAGE Arrows #-}
module FrSIRS.FrSIRSAgent (
    sirsAgentBehaviour
  ) where

import FrSIRS.FrSIRSModel

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Agent.AgentUtils

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR Functional Reactive implementation
------------------------------------------------------------------------------------------------------------------------
randomContact :: FrSIRSAgentOut -> FrSIRSAgentOut
randomContact ao = sendMessage ao' (randNeigh, Contact Infected)
    where
        ((_, randNeigh), ao') = runAgentRandom ao (pickRandomNeighbourCell ao)

sirsAgentSuceptible :: RandomGen g => g -> FrSIRSAgentBehaviour
sirsAgentSuceptible g = switch 
                            sirsAgentSusceptibleBehaviour
                            (sirsAgentSusceptibleInfected g)

sirsAgentSusceptibleBehaviour :: SF FrSIRSAgentIn (FrSIRSAgentOut, Event ())
sirsAgentSusceptibleBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao' = setDomainState ao Susceptible

        infectionEvent <- iEdge False -< hasMessage ain (Contact Infected)

        returnA -< (ao', infectionEvent)

-- TODO: update sirsState to infected here once, no need to constantly set to infected in infecedbehaviourSF
sirsAgentSusceptibleInfected :: RandomGen g => g -> () -> FrSIRSAgentBehaviour
sirsAgentSusceptibleInfected g _ = sirsAgentInfected g infectedDuration



sirsAgentInfected :: RandomGen g => g -> Double -> FrSIRSAgentBehaviour
sirsAgentInfected g duration = switch 
                            (sirsAgentInfectedBehaviour g infectedDuration)
                            (sirsAgentInfectedRecovered g)


sirsAgentInfectedBehaviour :: RandomGen g => g -> Double -> SF FrSIRSAgentIn (FrSIRSAgentOut, Event ())
sirsAgentInfectedBehaviour g duration = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao' = setDomainState ao Infected

        recoveredEvent <- after duration () -< ()

        -- NOTE: this means the agent is randomly contacting two neighbours within the infected duration
        makeContact <- occasionally g (infectedDuration * 0.5) () -< ()

        let ao'' = if isEvent makeContact then
                    randomContact ao'
                        else
                            ao'

        returnA -< (ao'', recoveredEvent)

-- TODO: update sirsState to recovered here once, no need to constantly set to recovered in recoverbehaviourSF
sirsAgentInfectedRecovered :: RandomGen g => g -> () -> FrSIRSAgentBehaviour
sirsAgentInfectedRecovered g _ = sirsAgentRecovered g



sirsAgentRecovered :: RandomGen g => g -> FrSIRSAgentBehaviour
sirsAgentRecovered g = switch 
                            sirsAgentRecoveredBehaviour
                            (sirsAgentRecoveredSusceptible g)

sirsAgentRecoveredBehaviour :: SF FrSIRSAgentIn (FrSIRSAgentOut, Event ())
sirsAgentRecoveredBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao' = setDomainState ao Recovered

        lostImmunityEvent <- after immuneDuration () -< ()

        returnA -< (ao', lostImmunityEvent)

-- TODO: update sirsState to susceptible here once, no need to constantly set to susceptible in susceptiblebehaviourSF
sirsAgentRecoveredSusceptible :: RandomGen g => g -> () -> FrSIRSAgentBehaviour
sirsAgentRecoveredSusceptible g _ = sirsAgentSuceptible g

-- NOTE: this is the initial SF which will be only called once
sirsAgentBehaviour :: RandomGen g => g -> FrSIRSState -> FrSIRSAgentBehaviour
sirsAgentBehaviour g Susceptible = sirsAgentSuceptible g
-- NOTE: when initially infected then select duration uniformly random 
sirsAgentBehaviour g Infected = sirsAgentInfected g' duration
    where
        (duration, g') = randomR (0.0, infectedDuration) g

sirsAgentBehaviour g Recovered = sirsAgentRecovered g
------------------------------------------------------------------------------------------------------------------------