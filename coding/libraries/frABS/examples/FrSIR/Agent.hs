{-# LANGUAGE Arrows #-}
module Agent 
  (
    sirAgentBehaviour
  ) where

import Control.Monad.Random

import FRP.FrABS
import FRP.Yampa

import Model

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
    let aid = agentId ain
    let ao = agentOut Susceptible ain
    ao' <- sendMessageOccasionallySrcSS g (1 / contactRate) contactSS (randomAgentIdMsgSource (Contact Susceptible) True) -< (ain, ao, e)
    returnA -< ao'

-- INFECTED
sirAgentInfected :: RandomGen g => g -> FrSIRAgentBehaviour
sirAgentInfected g = 
  transitionAfterExpSS g illnessDuration illnessTimeoutSS (ignoreEnv $ sirAgentInfectedBehaviour g) sirAgentRecovered

sirAgentInfectedBehaviour :: RandomGen g => g -> FrSIRAgentBehaviourIgnoreEnv
sirAgentInfectedBehaviour g = proc ain -> do
    let ao = agentOut Infected ain
    returnA -< respondToContactWith Infected ain ao

-- RECOVERED
sirAgentRecovered :: FrSIRAgentBehaviour
sirAgentRecovered = doNothing Recovered

-- INITIAL CASES
sirAgentBehaviour :: RandomGen g => g -> SIRState -> FrSIRAgentBehaviour
sirAgentBehaviour g Susceptible = sirAgentSuceptible g
sirAgentBehaviour g Infected = sirAgentInfected g
sirAgentBehaviour _ Recovered = sirAgentRecovered
------------------------------------------------------------------------------------------------------------------------