{-# LANGUAGE Arrows #-}
module FrSIRSNetwork.FrSIRSNetworkAgent (
    sirsNetworkAgentBehaviour
  ) where

import FrSIRSNetwork.FrSIRSNetworkModel

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Agent.AgentUtils

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR Functional Reactive implementation
------------------------------------------------------------------------------------------------------------------------
randomContact :: FrSIRSNetworkAgentOut -> FrSIRSNetworkAgentOut
randomContact ao = sendMessage ao' (randNeigh, Contact Infected)
    where
        (randNeigh, ao') = runAgentRandom ao (pickRandomNeighbourNode ao)

-- NOTE: infect with a given probability, every message has equal probability
gotInfected :: FrSIRSNetworkAgentIn -> Bool
gotInfected ain = onMessage ain gotInfectedAux False
    where
        gotInfectedAux :: Bool -> AgentMessage FrSIRSNetworkMsg -> Bool
        gotInfectedAux False (_, Contact Infected) = True -- TODO: draw with a given probability
        gotInfectedAux False _ = False
        gotInfectedAux True _ = True

sirsAgentSuceptible :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentSuceptible g = switch 
                            sirsAgentSusceptibleBehaviour
                            (sirsAgentSusceptibleInfected g)

sirsAgentSusceptibleBehaviour :: SF FrSIRSNetworkAgentIn (FrSIRSNetworkAgentOut, Event ())
sirsAgentSusceptibleBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao' = setDomainState ao Susceptible

        infectionEvent <- iEdge False -< gotInfected ain

        returnA -< (ao', infectionEvent)

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
        let ao = agentOutFromIn ain
        let ao' = setDomainState ao Infected

        recoveredEvent <- after duration () -< ()

        -- NOTE: this means the agent is randomly contacting two neighbours within the infected duration
        -- NOTE: this is a rate: needs to be sampled at high frequency
        makeContact <- occasionally g (duration * 0.5) () -< ()

        let ao'' = if isEvent makeContact then
                    randomContact ao'
                        else
                            ao'

        returnA -< (ao'', recoveredEvent)

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
sirsNetworkAgentBehaviour :: RandomGen g => g -> SIRSState -> FrSIRSNetworkAgentBehaviour
sirsNetworkAgentBehaviour g Susceptible = sirsAgentSuceptible g
-- NOTE: when initially infected then select duration uniformly random 
sirsNetworkAgentBehaviour g Infected = sirsAgentInfected g' duration
    where
        (duration, g') = randomR (0.0, illnessDuration) g

sirsNetworkAgentBehaviour g Recovered = sirsAgentRecovered g
------------------------------------------------------------------------------------------------------------------------