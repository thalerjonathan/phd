{-# LANGUAGE Arrows #-}
module Agent 
  (
    sirAgentBehaviour
  ) where

import Control.Monad.Random
import FRP.Chimera
import FRP.Yampa

import Model

------------------------------------------------------------------------------------------------------------------------
-- Non-Reactive Functions
------------------------------------------------------------------------------------------------------------------------
gotInfected :: RandomGen g => FrSIRAgentIn -> Rand g Bool
gotInfected ain = onMessageM gotInfectedAux ain False
  where
    gotInfectedAux :: RandomGen g => Bool -> AgentMessage FrSIRMsg -> Rand g Bool
    gotInfectedAux False (_, Contact Infected) = randomBoolM infectivity
    gotInfectedAux x _ = return x

respondToContactWith :: SIRState -> FrSIRAgentIn -> FrSIRAgentOut -> FrSIRAgentOut
respondToContactWith state ain ao = onMessage respondToContactWithAux ain ao
  where
    respondToContactWithAux :: AgentMessage FrSIRMsg -> FrSIRAgentOut -> FrSIRAgentOut
    respondToContactWithAux (senderId, Contact _) ao = sendMessage (senderId, Contact state) ao
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
-- SUSCEPTIBLE
sirAgentSuceptible :: RandomGen g => g -> FrSIRAgentBehaviour
sirAgentSuceptible g = 
  transitionOnEvent (sirAgentInfectedEvent g) (readEnv $ sirAgentSusceptibleBehaviour g) (sirAgentInfected g)

sirAgentInfectedEvent :: RandomGen g => g -> FrSIREventSource
sirAgentInfectedEvent g = proc (ain, ao, e) -> do
    isInfected <- randomSF g -< gotInfected ain
    infectionEvent <- edge -< isInfected
    returnA -< infectionEvent

sirAgentSusceptibleBehaviour :: RandomGen g => g -> FrSIRAgentBehaviourReadEnv
sirAgentSusceptibleBehaviour g = proc (ain, e) -> do
    let ao = agentOutObs Susceptible
    ao' <- sendMessageOccasionallySrcSS 
            g 
            (1 / contactRate) 
            contactSS 
            (randomAgentIdMsgSource g (Contact Susceptible) True) -< (ain, ao, e)
    returnA -< ao'

-- INFECTED
sirAgentInfected :: RandomGen g => g -> FrSIRAgentBehaviour
sirAgentInfected g = 
  transitionAfterExpSS g illnessDuration illnessTimeoutSS (ignoreEnv $ sirAgentInfectedBehaviour g) sirAgentRecovered

sirAgentInfectedBehaviour :: RandomGen g => g -> FrSIRAgentBehaviourIgnoreEnv
sirAgentInfectedBehaviour g = proc ain -> do
    let ao = agentOutObs Infected
    returnA -< respondToContactWith Infected ain ao

-- RECOVERED
sirAgentRecovered :: FrSIRAgentBehaviour
sirAgentRecovered = doNothingObs Recovered

-- INITIAL CASES
sirAgentBehaviour :: RandomGen g => g -> SIRState -> FrSIRAgentBehaviour
sirAgentBehaviour g Susceptible = sirAgentSuceptible g
sirAgentBehaviour g Infected = sirAgentInfected g
sirAgentBehaviour _ Recovered = sirAgentRecovered
------------------------------------------------------------------------------------------------------------------------