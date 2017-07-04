{-# LANGUAGE Arrows #-}
module FrSIRSSpatial.FrSIRSSpatialAgent (
    sirsAgentBehaviour,
    sirsAgentBehaviourRandInfected
  ) where

import FrSIRSSpatial.FrSIRSSpatialModel

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Agent.AgentUtils

import Control.Monad
import Control.Monad.Random

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR Functional Reactive implementation
------------------------------------------------------------------------------------------------------------------------
randomContact :: FrSIRSSpatialAgentOut -> FrSIRSSpatialAgentOut
randomContact ao = sendMessage ao' (randNeigh, Contact Infected)
    where
        ((_, randNeigh), ao') = runAgentRandom ao (pickRandomNeighbourCell ao)

gotInfected :: FrSIRSSpatialAgentIn -> Rand StdGen Bool
gotInfected ain = onMessageM ain gotInfectedAux False
    where
        gotInfectedAux :: Bool -> AgentMessage FrSIRSSpatialMsg -> Rand StdGen Bool
        gotInfectedAux False (_, Contact Infected) = drawRandomBool infectivity
        gotInfectedAux False _ = return False
        gotInfectedAux True _ = return True

sirsAgentSuceptible :: RandomGen g => g -> FrSIRSSpatialAgentBehaviour
sirsAgentSuceptible g = switch 
                            sirsAgentSusceptibleBehaviour
                            (sirsAgentSusceptibleInfected g)

sirsAgentSusceptibleBehaviour :: SF FrSIRSSpatialAgentIn (FrSIRSSpatialAgentOut, Event ())
sirsAgentSusceptibleBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao' = setDomainState ao Susceptible
        let (isInfected, ao'') = runAgentRandom ao' (gotInfected ain)

        infectionEvent <- iEdge False -< isInfected

        returnA -< (ao'', infectionEvent)

-- TODO: update sirsState to infected here once, no need to constantly set to infected in infecedbehaviourSF
sirsAgentSusceptibleInfected :: RandomGen g => g -> () -> FrSIRSSpatialAgentBehaviour
sirsAgentSusceptibleInfected g _ = sirsAgentInfected g illnessDuration



sirsAgentInfected :: RandomGen g => g -> Double -> FrSIRSSpatialAgentBehaviour
sirsAgentInfected g duration = switch 
                            (sirsAgentInfectedBehaviour g illnessDuration)
                            (sirsAgentInfectedRecovered g)


sirsAgentInfectedBehaviour :: RandomGen g => g -> Double -> SF FrSIRSSpatialAgentIn (FrSIRSSpatialAgentOut, Event ())
sirsAgentInfectedBehaviour g duration = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao' = setDomainState ao Infected

        recoveredEvent <- after duration () -< ()

        makeContact <- occasionally g (1 / contactRate) () -< ()

        let ao'' = if isEvent makeContact then
                    randomContact ao'
                        else
                            ao'

        returnA -< (ao'', recoveredEvent)

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
sirsNetworkAgentBehaviourRandInfected g Infected = sirsAgentInfected g' duration
    where
        (duration, g') = randomR (0.0, illnessDuration) g
sirsNetworkAgentBehaviourRandInfected g Recovered = sirsAgentRecovered g

-- NOTE: this is the initial SF which will be only called once
--          this behaviour should be used when initially a given number of agents is infected 
--          where is assumed that their illness-duration is not uniform randomly distributed
sirsAgentBehaviour :: RandomGen g => g -> SIRSState -> FrSIRSSpatialAgentBehaviour
sirsAgentBehaviour g Susceptible = sirsAgentSuceptible g
sirsAgentBehaviour g Infected = sirsAgentInfected g illnessDuration
sirsAgentBehaviour g Recovered = sirsAgentRecovered g
------------------------------------------------------------------------------------------------------------------------