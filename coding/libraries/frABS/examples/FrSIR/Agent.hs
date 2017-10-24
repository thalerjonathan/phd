{-# LANGUAGE Arrows #-}
module FrSIR.Agent 
    (
      sirAgentBehaviour
    ) where

import Control.Monad.Random

import FRP.FrABS
import FRP.Yampa

import FrSIR.Model

-- TODO: maybe when using conversations this makes a difference??

------------------------------------------------------------------------------------------------------------------------
-- Non-Reactive Functions
------------------------------------------------------------------------------------------------------------------------
gotInfected :: FrSIRAgentIn -> Rand StdGen Bool
gotInfected ain = onMessageM gotInfectedAux ain False
  where
    gotInfectedAux :: Bool -> AgentMessage FrSIRMsg -> Rand StdGen Bool
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
  transitionOnEvent sirAgentInfectedEvent (readEnv $ sirAgentSusceptibleBehaviour g) (sirAgentInfected g)

sirAgentInfectedEvent :: FrSIREventSource
sirAgentInfectedEvent = proc (ain, ao) -> do
    let (isInfected, ao') = agentRandom (gotInfected ain) ao
    infectionEvent <- edge -< isInfected
    returnA -< (ao', infectionEvent)

sirAgentSusceptibleBehaviour :: RandomGen g => g -> FrSIRAgentBehaviourReadEnv
sirAgentSusceptibleBehaviour g = proc (ain, e) -> do
    let ao = agentOutFromIn ain
    ao1 <- doOnce (setDomainState Susceptible) -< ao
    ao2 <- sendMessageOccasionallySrcSS g (1 / contactRate) contactSS (randomAgentIdMsgSource (Contact Susceptible) True) -< (ao1, e)
    returnA -< ao2

-- INFECTED
sirAgentInfected :: RandomGen g => g -> FrSIRAgentBehaviour
sirAgentInfected g = 
  transitionAfterExpSS g illnessDuration illnessTimeoutSS (ignoreEnv $ sirAgentInfectedBehaviour g) sirAgentRecovered

sirAgentInfectedBehaviour :: RandomGen g => g -> FrSIRAgentBehaviourIgnoreEnv
sirAgentInfectedBehaviour g = proc ain -> do
    let ao = agentOutFromIn ain
    ao1 <- doOnce (setDomainState Infected) -< ao
    let ao2 = respondToContactWith Infected ain ao1
    --ao2 <- sendMessageOccasionallySrcSS g (1 / contactRate) contactSS (randomAgentIdMsgSource (Contact Infected) True) -< (ao1, e)
    returnA -< ao2

-- RECOVERED
sirAgentRecovered :: FrSIRAgentBehaviour
sirAgentRecovered = doOnceR $ setDomainStateR Recovered

-- INITIAL CASES
sirAgentBehaviour :: RandomGen g => g -> SIRState -> FrSIRAgentBehaviour
sirAgentBehaviour g Susceptible = sirAgentSuceptible g
sirAgentBehaviour g Infected = sirAgentInfected g
sirAgentBehaviour _ Recovered = sirAgentRecovered
------------------------------------------------------------------------------------------------------------------------