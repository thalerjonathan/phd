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

respondToContactWith :: SIRSState -> FrSIRSNetworkAgentIn -> FrSIRSNetworkAgentOut -> FrSIRSNetworkAgentOut
respondToContactWith state ain ao = onMessage respondToContactWithAux ain ao
  where
    respondToContactWithAux :: AgentMessage FrSIRSNetworkMsg -> FrSIRSNetworkAgentOut -> FrSIRSNetworkAgentOut
    respondToContactWithAux (senderId, Contact _) ao = sendMessage (senderId, Contact state) ao
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
-- SUSCEPTIBLE
sirsAgentSuceptible :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentSuceptible g = transitionOnEvent
                            sirsAgentInfectedEvent
                            (readEnv $ sirsAgentSusceptibleBehaviour g)
                            (sirsAgentInfected g)

sirsAgentInfectedEvent :: FrSIRSNetworkEventSource
sirsAgentInfectedEvent = proc (ain, ao) -> do
  let (isInfected, ao') = agentRandom (gotInfected ain) ao
  infectionEvent <- edge -< isInfected
  returnA -< (ao', infectionEvent)

sirsAgentSusceptibleBehaviour :: RandomGen g => g -> FrSIRSNetworkAgentBehaviourReadEnv
sirsAgentSusceptibleBehaviour g = proc (ain, e) -> do
  let ao = agentOutFromIn ain
  ao1 <- doOnce (setAgentState Susceptible) -< ao
  ao2 <- sendMessageOccasionallySrcSS 
          g 
          (1 / contactRate) 
          contactSS 
          (randomNeighbourNodeMsgSource (Contact Susceptible)) -< (ao1, e)
  returnA -< ao2

-- INFECTED
sirsAgentInfected :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentInfected g = transitionAfterExpSS
                        g
                        illnessDuration 
                        illnessTimeoutSS
                        (ignoreEnv $ sirsAgentInfectedBehaviour g) 
                        (sirsAgentRecovered g)

sirsAgentInfectedBehaviour :: RandomGen g => g -> FrSIRSNetworkAgentBehaviourIgnoreEnv
sirsAgentInfectedBehaviour g = proc ain -> do
    let ao = agentOutFromIn ain
    ao1 <- doOnce (setAgentState Infected) -< ao
    let ao2 = respondToContactWith Infected ain ao1
    returnA -< ao2

-- RECOVERED
sirsAgentRecovered :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentRecovered _ = doOnceR $ setAgentStateR Recovered
{--
sirsAgentRecovered :: RandomGen g => g -> FrSIRSNetworkAgentBehaviour
sirsAgentRecovered g = transitionAfterExpSS
                            g 
                            immuneDuration 
                            immuneTimeoutSS
                            sirsAgentRecoveredBehaviour
                            (sirsAgentSuceptible g)

sirsAgentRecoveredBehaviour :: FrSIRSNetworkAgentBehaviour
sirsAgentRecoveredBehaviour = setAgentStateR Recovered
-}

-- INITIAL CASES
sirsNetworkAgentBehaviour :: RandomGen g => g -> SIRSState -> FrSIRSNetworkAgentBehaviour
sirsNetworkAgentBehaviour g Susceptible = sirsAgentSuceptible g
sirsNetworkAgentBehaviour g Infected = sirsAgentInfected g
sirsNetworkAgentBehaviour g Recovered = sirsAgentRecovered g
------------------------------------------------------------------------------------------------------------------------